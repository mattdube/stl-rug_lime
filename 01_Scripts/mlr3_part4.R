

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

at$predict(task_cust)$confusion
at$predict(task_cust)$score(measure)
at$predict(task_cust)$score(msr("classif.ce"))


lrn_xgboost <- lrn("classif.xgboost")

lrn_xgboost$param_set
