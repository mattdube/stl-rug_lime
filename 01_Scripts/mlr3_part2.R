library(dplyr)
library(readr)
library(ggplot2)
library(ggthemes)
library(data.table)

library(mlr3)
library(mlr3viz)
library(mlr3learners)
library(mlr3pipelines)
library(mlr3tuning)

library(paradox)


# remotes::install_github("mlr-org/mlr3db")
# remotes::install_github("mlr-org/mlr3viz")
# remotes::install_github("mlr-org/mlr3learners")
# remotes::install_github("mlr-org/mlr3filters")
# remotes::install_github("mlr-org/mlr3pipelines")

# Load data
customer <- read_csv(here::here("00_Data/raw", "WA_Fn-UseC_-Telco-Customer-Churn.csv"))

# clean data
customer_clean <- customer %>%
  filter(!is.na(TotalCharges)) %>%
  select(-c(customerID, TotalCharges, gender, PhoneService)) %>%
  mutate(SeniorCitizen = ifelse(SeniorCitizen == 1, "Yes", "No"),
         Churn = as.factor(Churn)) %>%
  mutate_if(is.character, as.factor)


write_csv(customer_clean, here::here("00_Data/clean/", "customer_clean.csv"))

# create task
task_cust <- TaskClassif$new(id = "churn", backend = customer_clean, target = "Churn", positive = "Yes")

# review task
task_cust
task_cust$ncol
task_cust$nrow
task_cust$feature_names
task_cust$feature_types
task_cust$col_roles

# split data train/test
set.seed(4411)
train.idx <- sample(seq_len(task_cust$nrow), 0.7 * task_cust$nrow)
test.idx <- setdiff(seq_len(task_cust$nrow), train.idx)

length(train.idx)
length(test.idx)

# create learner
lrn_cust <- mlr_learners$get("classif.rpart")

# decision tree
lrn_cust$train(task_cust, row_ids = train.idx)

plot(lrn_cust$model)
text(lrn_cust$model)

lrn_cust$model
lrn_cust$importance()

# create resampling strategy - 5 fold cross-validation - using only the training data
cv5 <- rsmp("cv", folds = 5, iters = 3)
cv5$instantiate(task_cust$clone()$filter(train.idx))

rcv5 <- rsmp("repeated_cv", folds = 5, repeats = 3)

# decision tree using cv plan
lrn_rpart <- lrn("classif.rpart", predict_type = "prob")

lrn_rpart$train(task_cust)
rpart_predict <- lrn_rpart$predict(task_cust)
rpart_predict$score(measure)
rpart_predict$confusion


res <- resample(task = task_cust$clone()$filter(train.idx), lrn_rpart, cv5)
res$score("classif.ce")
# > res$aggregate()
# classif.ce 
# 0.211906 
# > 1 - res$aggregate()
# classif.ce 
# 0.788094 
# > 

# random forest (ranger)
lrn_ranger <- lrn("classif.ranger", predict_type = "prob", num.trees = 15L)

res_ranger <- resample(task = task_cust$clone()$filter(train.idx), lrn_ranger, cv5)
res_ranger$score("classif.ce")
res_ranger$aggregate()
1 - res_ranger$aggregate()

# > res_ranger$aggregate()
# classif.ce 
# 0.2179984 
# > 1 - res_ranger$aggregate()
# classif.ce 
# 0.7820016 


