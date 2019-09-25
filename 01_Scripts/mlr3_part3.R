library(mlr3viz)
library(mlr3pipelines)
library(paradox)
library(mlr3tuning)
?mlr3pipelines::PipeOpEncode

lrn_ranger <- lrn("classif.ranger", predict_type = "prob", num.trees = 15L)
lrn_rpart <- lrn("classif.rpart", predict_type = "prob")

# create task
tsk_cust <- TaskClassif$new(id = "churn", backend = customer_clean, target = "Churn", positive = "Yes")

# split data train/test
set.seed(4411)
train.idx <- sample(seq_len(task_cust$nrow), 0.75 * task_cust$nrow)
test.idx <- setdiff(seq_len(task_cust$nrow), train.idx)

lrn_glmnet <- lrn("classif.glmnet", predict_type = "prob")
rcv5 <- rsmp("repeated_cv", folds = 5, repeats = 3)
lrn_glmnet$param_set

# create preprocessing and learner pipeline
op1 <- PipeOpScale$new()
op2 <- PipeOpEncode$new()

graph_pp_xgb = op1 %>>% op2 %>>%
  mlr_pipeops$get("learner",
                  learner = lrn_xgboost)

plot(graph_pp_xgb, html = TRUE)

graph_pp_xgb$train(tsk_cust)
graph_pp_xgb$state[3]

gxglrn = GraphLearner$new(graph_pp_xgb)

gxglrn$train(tsk_cust)

gxglrn$model


gxglrn$train(tsk_cust)$state
as.data.table(gxglrn$param_set)

xg_prediction <- gxglrn$predict(tsk_cust)
xg_prediction$score("classif.ce")

rr_gxglrn <- resample(tsk_cust, gxglrn, rcv5, store_models = TRUE)
rr_gxglrn$aggregate()

tune_xg_ps <- ParamSet$new(list(
  ParamDbl$new("classif.xgboost.eta", lower = 0.01, upper = 0.3),
  ParamInt$new("classif.xgboost.nrounds", lower = 100, upper = 1000)
))

# meas_ce
# rcv5

evals20 <- term("evals", n_evals = 20)

instance <- TuningInstance



instance = TuningInstance$new(
  task = task_cust,
  learner = glrn_glm,
  resampling = hout,
  measures = tune_measure,
  param_set = glrn_glm_ps,
  terminator = evals20
)

instance

tuner = tnr("random_search")

result = tuner$tune(instance)


at_xgb = AutoTuner$new(
  learner = gxglrn,
  resampling = rcv5,
  measures = meas_ce,
  tune_ps = tune_xg_ps,
  terminator = evals20,
  tuner = tuner
)

at_xgb

grid = benchmark_grid(
  task = tsk_cust,
  learner = list(at_xgb, gxglrn, lrn_rpart),
  resamplings = rsmp("cv", folds = 3)
)

bmr = benchmark(grid)

bmr$aggregate(measure)












graph_pp_glm = op1 %>>% op2 %>>%
  mlr_pipeops$get("learner",
                  learner = lrn_glmnet)

plot(graph_pp_glm, html = TRUE)

glrn_glm = GraphLearner$new(graph_pp_glm)

glrn_glm$param_set

glrn_glm_ps <- ParamSet$new(list(
  ParamDbl$new("classif.glmnet.alpha", lower = 0, upper = 1)
))
  

res_glrn_glm <- resample(task = task_cust$clone()$filter(train.idx), glrn_glm, rcv5)
res_glrn_glm$score("classif.ce")


glrn_glm$train(task_cust)
glm_predict <- glrn_glm$predict(task_cust)

measure = msr("classif.acc")
glm_predict$score(measure)


lrn_ranger

customer_clean %>% head()

op1 <- PipeOpScale$new()
op2 <- PipeOpEncode$new()

linear_pipeline <- op1 %>>% op2

pipe_op_model_test <- linear_pipeline$train(task_cust)
pipe_op_model_test


task_cust$row_roles$use = test.idx
linear_pipeline$predict(task_cust)

plot(linear_pipeline)


graph_pp = op1 %>>% op2 %>>%
  mlr_pipeops$get("learner",
                  learner = lrn_ranger)

graph_pp2 = op1 %>>% op2 %>>%
  mlr_pipeops$get("learner",
                  learner = lrn_rpart)

graph_pp2$train(task_cust)
graph_pp2
graph_pp2$predict(task_cust)

graph_pp2$state
graph_pp$state

plot(graph_pp)

graph_pp$plot(html = TRUE)

glrn = GraphLearner$new(graph_pp)


glrn$train(task_cust)

glrn$model


glrn$train(task_cust)$state


cust_prediction <- glrn$predict(task_cust)
cust_prediction

print(cust_prediction)
cust_prediction$confusion

measure = msr("classif.ce")
cust_prediction$score(measure)
