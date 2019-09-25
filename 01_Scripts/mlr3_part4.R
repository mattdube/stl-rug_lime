

hout = rsmp("holdout")
tune_measure = msr("classif.ce")
evals20 = term("evals", n_evals = 20)

instance = TuningInstance$new(
  task = task_cust,
  learner = glrn_glm,
  resampling = hout,
  measures = tune_measure,
  param_set = glrn_glm_ps,
  terminator = evals20
)

instance

tuner = tnr("grid_search", resolution = 5)

result = tuner$tune(instance)


at = AutoTuner$new(
  learner = glrn_glm,
  resampling = hout,
  measures = tune_measure,
  tune_ps = glrn_glm_ps,
  terminator = evals20,
  tuner = tuner
)

at

grid = benchmark_grid(
  task = task_cust,
  learner = list(at, glrn_glm),
  resamplings = rsmp("repeated_cv", folds = 5, repeats = 3)
)

bmr = benchmark(grid)

bmr$aggregate(measure)

at$train(task_cust)
predict_glm = at$predict(task_cust)

tuned_glmnet_model <- at$model
tuned_glmnet <- at

save(tuned_glmnet, file = "03_Models/tuned_glmnet.rda")
saveRDS(tuned_glmnet_model, file = "03_Models/tuned_glmnet_model.rds")
saveRDS(tuned_glmnet, file = "03_Models/tuned_glmnet.rds")

saveRDS(task_cust, file = "03_Models/task_cust.rds")
saveRDS(customer_clean, file = "03_Models/customer_clean.rds")


model.glmnet <- vector(mode = 'list')

model.glmnet$tuned_glmnet <- tuned_glmnet
model.glmnet$task_cust <- task_cust
model.glmnet$customer_clean <- customer_clean

saveRDS(model.glmnet, file = "03_Models/model_glmnet.rds")

at$predict(task_cust)$confusion
at$predict(task_cust)$score(measure)
at$predict(task_cust)$score(msr("classif.ce"))


lrn_xgboost <- lrn("classif.xgboost")

lrn_xgboost$param_set

