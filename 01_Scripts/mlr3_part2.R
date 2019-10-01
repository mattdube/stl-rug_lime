# library(dplyr)
# library(readr)
library(ggplot2)
library(ggthemes)
library(data.table)

library(mlr3)
library(mlr3viz)
library(mlr3learners)
library(mlr3pipelines)
library(mlr3tuning)

library(paradox)
library(pryr)


# remotes::install_github("mlr-org/mlr3db")
# remotes::install_github("mlr-org/mlr3viz")
# remotes::install_github("mlr-org/mlr3learners")
# remotes::install_github("mlr-org/mlr3filters")
# remotes::install_github("mlr-org/mlr3pipelines")

# Load data
#customer <- read_csv(here::here("00_Data/raw", "WA_Fn-UseC_-Telco-Customer-Churn.csv"))
customerDT <- fread(here::here("00_Data/raw", "WA_Fn-UseC_-Telco-Customer-Churn.csv"))
# clean data
customerDT[,c("customerID", "TotalCharges", "gender", "PhoneService") := NULL][
    ,Churn := as.factor(Churn)][
      , SeniorCitizen := ifelse(SeniorCitizen == 1, "Yes", "No")]

# cols = colnames(customerDT)[1:3]
# cols2 = colnames(customerDT)[5:15]
# customerDT[,(cols):= lapply(.SD, as.factor), .SDcols=cols][
#   ,(cols2):= lapply(.SD, as.factor), .SDcols = cols2]

str(customerDT)

# customer <- customer %>%
#   filter(!is.na(TotalCharges)) %>%
#   select(-c(customerID, TotalCharges, gender, PhoneService)) %>%
#   mutate(SeniorCitizen = ifelse(SeniorCitizen == 1, "Yes", "No"),
#          Churn = as.factor(Churn)) %>%
#   mutate_if(is.character, as.factor)


write_csv(customer_clean, here::here("00_Data/clean/", "customer_clean.csv"))

# create task
tsk_cust <- TaskClassif$new(id = "churn", backend = customerDT, target = "Churn", positive = "Yes")

# review task
tsk_cust
tsk_cust$ncol
tsk_cust$nrow
tsk_cust$feature_names
tsk_cust$feature_types
tsk_cust$col_roles
tsk_cust$formula()
tsk_cust$class_names
tsk_cust$tsk_type
tsk_cust$data()


# EDA plot of task ###################################
p <- autoplot(tsk_cust, type = "pairs") + theme_light()

autoplot(tsk_cust) + facet_wrap(~Contract) + theme_light()

for(i in 1:p$nrow) {
  for(j in 1:p$ncol){
    p[i,j] <- p[i,j] + 
      scale_fill_economist() +
      scale_color_economist()  
  }
}

p

table(customerDT$Churn)
table(customerDT[test.idx]$Churn)
table(customerDT[train.idx]$Churn)

# split data train/test
set.seed(4411)
train.idx <- sample(seq_len(tsk_cust$nrow), 0.7 * tsk_cust$nrow)
test.idx <- setdiff(seq_len(tsk_cust$nrow), train.idx)

length(train.idx)
length(test.idx)

# create learner
lrn_rpart <- mlr_learners$get("classif.rpart")

# decision tree
lrn_rpart$train(tsk_cust, row_ids = train.idx)

plot(lrn_rpart$model)
text(lrn_rpart$model)

lrn_rpart$model
lrn_rpart$importance()

rpart_predict <- lrn_rpart$predict(tsk_cust, test.idx)
meas_ce <- msr("classif.ce")
meas_acc <- msr("classif.acc")

rpart_predict$score(list(meas_ce, meas_acc))

# create resampling strategy - 5 fold cross-validation - using only the training data
cv5 <- rsmp("cv", folds = 5)
cv5$instantiate(tsk_cust$clone()$filter(train.idx))

rcv5 <- rsmp("repeated_cv", folds = 5, repeats = 3)

# decision tree using cv plan
lrn_rpart <- lrn("classif.rpart", predict_type = "prob")

as.data.table(mlr_measures)
measure = msr("classif.auc")
meas_ce = msr("classif.ce")

lrn_rpart$train(tsk_cust)
rpart_predict <- lrn_rpart$predict(tsk_cust)
rpart_predict$score(measure)
rpart_predict$score(meas_ce)
rpart_predict$confusion


res <- resample(task = tsk_cust$clone()$filter(train.idx), lrn_rpart, cv5)
res$score("classif.ce")

rr5 <- resample(task = tsk_cust$clone()$filter(train.idx), lrn_rpart, rcv5, store_models = TRUE)
rr5
rr5$resampling
rr5$resampling$iters
rr5$resampling$test_set(3)
lrn_1 = rr5$learners[[1]]
lrn_1$model


# > res$aggregate()
# classif.ce 
# 0.211906 
# > 1 - res$aggregate()
# classif.ce 
# 0.788094 
# > 

# random forest (ranger)
lrn_ranger <- lrn("classif.ranger", predict_type = "prob", num.trees = 15L)
lrn_ranger$train(tsk_cust, train.idx)
ranger_predict <- lrn_ranger$predict(tsk_cust, test.idx)
ranger_predict$score(list(meas_ce, meas_acc))

rr_ranger <- resample(task = tsk_cust$clone()$filter(train.idx), lrn_ranger, rcv5)
rr_ranger$score("classif.ce")
rr_ranger$aggregate()
1 - res_ranger$aggregate()

# > res_ranger$aggregate()
# classif.ce 
# 0.2179984 
# > 1 - res_ranger$aggregate()
# classif.ce 
# 0.7820016 

lrn_xgboost <- lrn("classif.xgboost")
rr_xg <- resample(task = tsk_cust$clone()$filter(train.idx), lrn_xgboost, rcv5, store_models = TRUE)
rr_xg$score("classif.ce")
rr_xg$aggregate()
