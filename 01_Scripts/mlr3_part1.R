library(dplyr)
library(here)
library(readr)
library(mlr3)
library(mlr3viz)
library(ggplot2)
library(tidyquant)
library(ggthemes)
library(data.table)
library(mlr3learners)
library(mlr3tuning)
library(paradox)




# Load data
customer <- read_csv(here("00_Data/raw", "WA_Fn-UseC_-Telco-Customer-Churn.csv"))


# mlr3 quickstart example ##############################
task <- mlr_tasks$get("iris")
learner <- mlr_learners$get("classif.rpart")

# train a model of this learner for a subset of the task
learner$train(task, row_ids = 1:120)

# this is what the decision tree looks like
learner$model

predictions <- learner$predict(task, row_ids = 121:150)
predictions 

# accuracy of model
predictions$score("classif.acc")

######################################################

learner

# EDA plot of task ###################################
p <- autoplot(task, type = "pairs") + theme_light()


for(i in 1:p$nrow) {
  for(j in 1:p$ncol){
    p[i,j] <- p[i,j] + 
      scale_fill_economist() +
      scale_color_economist()  
  }
}

p

#####################################################

task
as.data.table(mlr_tasks)

as.data.table(task)
summary(as.data.table(task))
task$col_roles


# optimizing simplee classification tree ###########

pima_task <- tsk("pima")
pima_task

pima_learner <- lrn("classif.rpart")
pima_learner$param_set

tune_ps <- ParamSet$new(list(
  ParamDbl$new("cp", lower = 0.001, upper = 0.1),
  ParamInt$new("minsplit", lower = 1, upper = 10)
))

tune_ps
