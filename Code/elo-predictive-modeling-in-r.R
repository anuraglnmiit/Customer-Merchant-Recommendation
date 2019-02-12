## Importing packages
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(Matrix)
library(Metrics)
list.files(path = "../input")


#---------------------------
cat("Preprocessing historical transactions...\n")



#importing historical transactions 
htrans <- read_csv("historical_transactions.csv")



#renaming column card_id to card to avoid conflict further
names(htrans)[names(htrans) == 'card_id'] <- 'card'



View(head(htrans))


fn <- funs(sum, mean, min, max, sd, n_distinct, .args = list(na.rm = TRUE))
htrans$authorized_flag <- ifelse(htrans$authorized_flag == "Y",1,0)

hist_summary <- htrans %>%
    group_by(card_id) %>% 
    select(c("card_id","purchase_amount","month_lag","installments","authorized_flag")) %>%
    summarize_all(fn) %>%
    left_join(hist_summary2,by="card_id") %>%
    left_join(hist_summary3,by="card_id") %>%
    left_join(hist_cat1,by="card_id") %>%
    left_join(hist_cat2[,-7],by="card_id") %>%
    left_join(hist_cat3[,-5],by="card_id") 

head(htrans)



# historical transaction proportional distribution authorisation flag
table(htrans$authorized_flag)
prop.table(table(histdata$authorized_flag))



# historical transaction proportional distribution category1
table(htrans$category_1)
prop.table(table(htrans$category_1))

# historical transaction proportional distribution category2
table(htrans$category_2)
prop.table(table(htrans$category_2))

# historical transaction proportional distribution category3
table(htrans$category_3)
prop.table(table(htrans$category_3))


#analysing the distribution of purchase amount and month lag
summary(htrans$purchase_amount)

quantile(htrans$purchase_amount, quar_seq) 

summary(htrans$month_lag)
quantile(htrans$month_lag, quar_seq) 



# historical transaction ids
sum_htrans_id <- htrans %>%
  group_by(card) %>%
  summarise_at(vars(ends_with("_id")), n_distinct, na.rm = TRUE) 

#one hot encoding the categorical variables for historical transactions.
ohe_htrans <- htrans %>%
  select(authorized_flag, starts_with("category")) %>% 
  mutate_all(factor) %>% 
  model.matrix.lm(~ . - 1, ., na.action = NULL) %>% 
  as_tibble()



View(head(ohe_htrans))

#garbage clearance
rm(htrans, sum_htrans_id, ohe_htrans)


fn <- funs(mean, sd, min, max, sum, n_distinct, .args = list(na.rm = TRUE))

# feature engineer historic transactions
sum_htrans <- htrans %>%
  select(-authorized_flag, -starts_with("category"), -ends_with("_id")) %>% 
  add_count(card) %>%
  group_by(card) %>%
  mutate(date_diff = as.integer(diff(range(purchase_date))),
         prop = n() / sum(n)) %>% 
  ungroup() %>% 
  mutate(year = year(purchase_date),
         month = month(purchase_date),
         day = day(purchase_date),
         hour = hour(purchase_date),
         month_diff = as.integer(ymd("2018-12-01") - date(purchase_date)) / 30 + month_lag) %>% 
  select(-purchase_date) %>% 
  bind_cols(ohe_htrans) %>% 
  group_by(card) %>%
  summarise_all(fn) %>% 
  left_join(sum_htrans_id)


rm(htrans, sum_htrans_id, ohe_htrans); invisible(gc())

#---------------------------
cat("Preprocessing new transactions...\n")


# joinging merchants information with the new merchants transactions
ntrans <- read_csv("new_merchant_transactions.csv") %>% 
  left_join(read_csv("merchants.csv"),
            by = "merchant_id", suffix = c("", "_y")) %>%
  select(-authorized_flag) %>% 
  rename(card = card_id)


# new merchants transaction data
# newdata category
new_cat1 <- newdata %>% group_by(card_id,category_1) %>% 
  summarize(count=n())%>%
  spread(key=category_1, value=count)

new_cat2 <- newdata %>% group_by(card_id,category_2) %>% 
  summarize(count=n())%>%
  spread(key=category_2, value=count)

new_cat3 <- newdata %>% group_by(card_id,category_3) %>% 
  summarize(count=n())%>%
  spread(key=category_3, value=count)


#similarly transaction sums done (same as historic transactions)
sum_ntrans_id <- ntrans %>%
  group_by(card) %>%
  summarise_at(vars(contains("_id")), n_distinct, na.rm = TRUE) 


# One hot encoding new tranasactions
ohe_ntrans <- ntrans %>%
  select(starts_with("category"), starts_with("most_recent")) %>% 
  mutate_all(factor) %>% 
  model.matrix.lm(~ . - 1, ., na.action = NULL) %>% 
  as_tibble()

# feature engineering variables for new transactions
fn <- funs(mean, sd, min, max, sum, n_distinct, .args = list(na.rm = TRUE))
sum_ntrans <- ntrans %>%
  select(-starts_with("category"), -starts_with("most_recent"), -contains("_id")) %>% 
  add_count(card) %>%
  group_by(card) %>%
  mutate(date_diff = as.integer(diff(range(purchase_date))),
         prop = n() / sum(n)) %>% 
  ungroup() %>% 
  mutate(year = year(purchase_date),
         month = month(purchase_date),
         day = day(purchase_date),
         hour = hour(purchase_date),
         month_diff = as.integer(ymd("2018-12-01") - date(purchase_date)) / 30 + month_lag) %>% 
  select(-purchase_date) %>% 
  bind_cols(ohe_ntrans) %>% 
  group_by(card) %>%
  summarise_all(fn) %>% 
  left_join(sum_ntrans_id)


# garbage collection
rm(ntrans, sum_ntrans_id, ohe_ntrans, fn); invisible(gc())

#---------------------------
cat("Joining datasets...\n")

tr <- read_csv("train.csv") 
te <- read_csv("test.csv")

tri <- 1:nrow(tr)
y <- tr$target


# preparing training and testing data
# plot target variable in training data

tr %>% 
  bind_rows(te) %>% 
  mutate(set = factor(if_else(is.na(target), "Test", "Train")),
         first_active_month = ymd(first_active_month, truncated = 1)) %>% 
  ggplot(aes(x = first_active_month, fill = set)) +
  geom_bar() +
  theme_minimal()

## Visualising the distribution in training and test
## the three anonymised feature variables
tr %>% 
  bind_rows(te) %>% 
  mutate(set = factor(if_else(is.na(target), "Test", "Train"))) %>% 
  select(-first_active_month, -card_id, -target) %>% 
  gather(key = "feature", value = "value", -set) %>% 
  mutate(value = factor(value)) %>% 
  ggplot(aes(value, fill = set)) +
  geom_bar(aes(y=..prop.., group = 1)) +
  scale_y_continuous(labels = percent_format()) + 
  facet_wrap(set ~ feature, scales = "free") +
  theme_minimal()



tr_te <- tr %>% 
  select(-target) %>% 
  bind_rows(te) %>%
  rename(card = card_id) %>% 
  mutate(first_active_month = ymd(first_active_month, truncated = 1),
         year = year(first_active_month),
         month = month(first_active_month),
         date_diff = as.integer(ymd("2018-02-01") - first_active_month)) %>% 
  select(-first_active_month) %>% 
  left_join(sum_htrans, by = "card") %>% 
  left_join(sum_ntrans, by = "card") %>% 
  select(-card) %>% 
  mutate_all(funs(ifelse(is.infinite(.), NA, .))) %>% 
  select_if(~ n_distinct(.x) > 1) %>% 
  data.matrix()

rm(tr, te, sum_htrans, sum_ntrans); invisible(gc())

#---------------------------
cat("Preparing data...\n")


#
val <- caret::createDataPartition(y, p = 0.2, list = FALSE)
dtrain <- xgb.DMatrix(data = tr_te[tri, ][-val, ], label = y[-val])
dval <- xgb.DMatrix(data = tr_te[tri, ][val, ], label = y[val])
dtest <- xgb.DMatrix(data = tr_te[-tri, ])
cols <- colnames(tr_te)

rm(tr_te, y, tri); gc()

#---------------------------
cat("Training model...\n")
p <- list(objective = "reg:linear",
          booster = "gbtree",
          eval_metric = "rmse",
          nthread = 4,
          eta = 0.02,
          max_depth = 7,
          min_child_weight = 100,
          gamma = 0,
          subsample = 0.85,
          colsample_bytree = 0.8,
          colsample_bylevel = 0.8,
          alpha = 0,
          lambda = 0.1)

set.seed(0)

#tuning trainaing variables
m_xgb <- xgb.train(p, dtrain, 2000, list(val = dval), print_every_n = 100, early_stopping_rounds = 200)


# plotting xgb importance features
xgb.importance(cols, model = m_xgb) %>% 
  xgb.ggplot.importance(top_n = 20) + theme_minimal()


#writing xgb prediction on file
read_csv("sample_submission.csv") %>%  
  mutate(target = predict(m_xgb, dtest)) %>%
  write_csv(paste0("tidy_elo_", round(m_xgb$best_score, 5), ".csv"))

