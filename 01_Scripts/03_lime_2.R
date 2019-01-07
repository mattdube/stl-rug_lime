library(lime)
library(vip)


train_x <- trainData %>% select(-Churn)
train_y <- trainData %>% select(Churn)
test_x <- testData %>% select(-Churn)
test_y <- testData %>% select(Churn)

set.seed(5431)
local_obs_1 <- test_x[sample(1:nrow(test_x),4, replace = FALSE),]

set.seed(5432)
local_obs_2 <- test_x[sample(1:nrow(test_x),4, replace = FALSE),]

set.seed(5433)
local_obs_3 <- test_x[sample(1:nrow(test_x),4, replace = FALSE),]

set.seed(5434)
local_obs_4 <- test_x[sample(1:nrow(test_x),4, replace = FALSE),]

set.seed(5435)
local_obs_5 <- test_x[sample(1:nrow(train_x),4, replace = FALSE),]

explainer_mars <- lime(train_x, mars.fit, n_bins = 5)
explainer_gbm <- lime(train_x, gbm.fit, n_bins = 5)
explainer_glmnet <- lime(train_x, glmnet.fit, n_bins = 5)
explainer_nnet <- lime(train_x, nnet.fit, n_bins = 5)
explainer_xgb <- lime(train_x, xgb.fit, n_bins = 5)

class(explainer_mars)

explanation_mars <- explain(
  x = sample1,
  explainer = explainer_mars,
  n_permutations = 5000,
  dist_fun = "manhattan",
  kernel_width = .75,
  n_features = 10,
  feature_select = "lasso_path",
  labels = "Yes"
  )





explanation_gbm <- explain(
  x = sample1,
  explainer = explainer_gbm,
  n_permutations = 5000,
  dist_fun = "manhattan",
  kernel_width = .75,
  n_features = 10,
  feature_select = "lasso_path",
  labels = "Yes"
  )

explanation_glmnet <- explain(
  x = sample1,
  explainer = explainer_glmnet,
  n_permutations = 5000,
  dist_fun = "manhattan",
  kernel_width = .75,
  n_features = 10,
  feature_select = "lasso_path",
  labels = "Yes"
)

explanation_nnet <- explain(
  x = sample1,
  explainer = explainer_nnet,
  n_permutations = 5000,
  dist_fun = "manhattan",
  kernel_width = .75,
  n_features = 10,
  feature_select = "lasso_path",
  labels = "Yes"
)

explanation_xgb <- explain(
  x = sample1,
  explainer = explainer_xgb,
  n_permutations = 5000,
  dist_fun = "manhattan",
  kernel_width = .75,
  n_features = 10,
  feature_select = "lasso_path",
  labels = "Yes"
)

plot_features(explanation_mars) + ggtitle(label = "mars explanation: sample 1") +
  theme(plot.title = element_text(size = 20, hjust = 0.3))

plot_features(explanation_glmnet) + ggtitle(label = "glmnet explanation: sample 1") +
  theme(plot.title = element_text(size = 20, hjust = 0.3))

plot_features(explanation_gbm) + ggtitle(label = "gbm explanation: sample 1") +
  theme(plot.title = element_text(size = 20, hjust = 0.3))

plot_features(explanation_nnet) + ggtitle(label = "nnet explanation: sample 1") +
  theme(plot.title = element_text(size = 20, hjust = 0.3))

plot_features(explanation_xgb) + ggtitle(label = "xgb explanation: sample 1") +
  theme(plot.title = element_text(size = 20, hjust = 0.3))


explanation_mars <- explain(
  x = sample2,
  explainer = explainer_mars,
  n_permutations = 5000,
  dist_fun = "manhattan",
  kernel_width = .75,
  n_features = 10,
  feature_select = "lasso_path",
  labels = "Yes"
)

explanation_gbm <- explain(
  x = sample2,
  explainer = explainer_gbm,
  n_permutations = 5000,
  dist_fun = "manhattan",
  kernel_width = .75,
  n_features = 10,
  feature_select = "lasso_path",
  labels = "Yes"
)

explanation_glmnet <- explain(
  x = sample2,
  explainer = explainer_glmnet,
  n_permutations = 5000,
  dist_fun = "manhattan",
  kernel_width = .75,
  n_features = 10,
  feature_select = "lasso_path",
  labels = "Yes"
)

explanation_nnet <- explain(
  x = sample2,
  explainer = explainer_nnet,
  n_permutations = 5000,
  dist_fun = "manhattan",
  kernel_width = .75,
  n_features = 10,
  feature_select = "lasso_path",
  labels = "Yes"
)

explanation_xgb <- explain(
  x = sample2,
  explainer = explainer_xgb,
  n_permutations = 5000,
  dist_fun = "manhattan",
  kernel_width = .75,
  n_features = 10,
  feature_select = "lasso_path",
  labels = "Yes"
)


plot_features(explanation_mars) + ggtitle(label = "mars explanation: sample 2") +
  theme(plot.title = element_text(size = 20, hjust = 0.3))

plot_features(explanation_glmnet) + ggtitle(label = "glmnet explanation: sample 2") +
  theme(plot.title = element_text(size = 20, hjust = 0.3))

plot_features(explanation_gbm) + ggtitle(label = "gbm explanation: sample 2") +
  theme(plot.title = element_text(size = 20, hjust = 0.3))

plot_features(explanation_nnet) + ggtitle(label = "nnet explanation: sample 2") +
  theme(plot.title = element_text(size = 20, hjust = 0.3))

plot_features(explanation_xgb) + ggtitle(label = "xgb explanation: sample 2") +
  theme(plot.title = element_text(size = 20, hjust = 0.3))


explanation_mars <- explain(
  x = sample3,
  explainer = explainer_mars,
  n_permutations = 5000,
  dist_fun = "manhattan",
  kernel_width = .75,
  n_features = 10,
  feature_select = "lasso_path",
  labels = "Yes"
)

explanation_gbm <- explain(
  x = sample3,
  explainer = explainer_gbm,
  n_permutations = 5000,
  dist_fun = "manhattan",
  kernel_width = .75,
  n_features = 10,
  feature_select = "lasso_path",
  labels = "Yes"
)

explanation_glmnet <- explain(
  x = sample3,
  explainer = explainer_glmnet,
  n_permutations = 5000,
  dist_fun = "manhattan",
  kernel_width = .75,
  n_features = 10,
  feature_select = "lasso_path",
  labels = "Yes"
)

explanation_nnet <- explain(
  x = sample3,
  explainer = explainer_nnet,
  n_permutations = 5000,
  dist_fun = "manhattan",
  kernel_width = .75,
  n_features = 10,
  feature_select = "lasso_path",
  labels = "Yes"
)

explanation_xgb <- explain(
  x = sample3,
  explainer = explainer_xgb,
  n_permutations = 5000,
  dist_fun = "manhattan",
  kernel_width = .75,
  n_features = 10,
  feature_select = "lasso_path",
  labels = "Yes"
)

plot_features(explanation_mars) + ggtitle(label = "mars explanation: sample 3") +
  theme(plot.title = element_text(size = 20, hjust = 0.3))

plot_features(explanation_glmnet) + ggtitle(label = "glmnet explanation: sample 3") +
  theme(plot.title = element_text(size = 20, hjust = 0.3))

plot_features(explanation_gbm) + ggtitle(label = "gbm explanation: sample 3") +
  theme(plot.title = element_text(size = 20, hjust = 0.3))

plot_features(explanation_nnet) + ggtitle(label = "nnet explanation: sample 3") +
  theme(plot.title = element_text(size = 20, hjust = 0.3))

plot_features(explanation_xgb) + ggtitle(label = "xgb explanation: sample 3") +
  theme(plot.title = element_text(size = 20, hjust = 0.3))


explanation_mars <- explain(
  x = sample4,
  explainer = explainer_mars,
  n_permutations = 5000,
  dist_fun = "manhattan",
  kernel_width = .75,
  n_features = 10,
  feature_select = "lasso_path",
  labels = "Yes"
)

explanation_gbm <- explain(
  x = sample4,
  explainer = explainer_gbm,
  n_permutations = 5000,
  dist_fun = "minkowski",
  kernel_width = .75,
  n_features = 10,
  feature_select = "lasso_path",
  labels = "Yes"
)

explanation_glmnet <- explain(
  x = sample4,
  explainer = explainer_glmnet,
  n_permutations = 5000,
  dist_fun = "manhattan",
  kernel_width = .75,
  n_features = 10,
  feature_select = "lasso_path",
  labels = "Yes"
)

explanation_nnet <- explain(
  x = sample4,
  explainer = explainer_nnet,
  n_permutations = 5000,
  dist_fun = "manhattan",
  kernel_width = .75,
  n_features = 10,
  feature_select = "lasso_path",
  labels = "Yes"
)

explanation_xgb <- explain(
  x = sample4,
  explainer = explainer_xgb,
  n_permutations = 5000,
  dist_fun = "manhattan",
  kernel_width = .75,
  n_features = 10,
  feature_select = "lasso_path",
  labels = "Yes"
)

plot_features(explanation_mars) + ggtitle(label = "mars explanation: sample 4") +
  theme(plot.title = element_text(size = 20, hjust = 0.3))

plot_features(explanation_glmnet) + ggtitle(label = "glmnet explanation: sample 4") +
  theme(plot.title = element_text(size = 20, hjust = 0.3))

plot_features(explanation_gbm) + ggtitle(label = "gbm explanation: sample 4") +
  theme(plot.title = element_text(size = 20, hjust = 0.3))

plot_features(explanation_nnet) + ggtitle(label = "nnet explanation: sample 4") +
  theme(plot.title = element_text(size = 20, hjust = 0.3))

plot_features(explanation_xgb) + ggtitle(label = "xgb explanation: sample 4") +
  theme(plot.title = element_text(size = 20, hjust = 0.3))