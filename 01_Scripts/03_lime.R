library(lime)
library(vip)

set.seed(5431)
local_obs_1 <- testData[sample(1:nrow(testData),4, replace = FALSE),]

set.seed(5430)
local_obs_2 <- testData[sample(1:nrow(testData),4, replace = FALSE),]


explainer_mars <- lime(trainData, mars.fit, n_bins = 5)
explainer_xgb <- lime(trainData, xgb.fit, n_bins = 5)

class(explainer_mars)

explanation_mars <- explain(
    x = local_obs_1,
    explainer = explainer_mars,
    n_permutations = 5000,
    dist_fun = "manhattan",
    kernel_width = .75,
    n_features = 10,
    feature_select = "lasso_path",
    labels = "Yes"
)

plot_features(explanation_mars)

explanation_xgb <- explain(
    x = local_obs_1,
    explainer = explainer_xgb,
    n_permutations = 5000,
    dist_fun = "manhattan",
    kernel_width = 3,
    n_features = 10,
    feature_select = "lasso_path",
    labels = "Yes"
)

explanation_xgb <- explain(
    x = local_obs,
    explainer = explainer_xgb,
    n_permutations = 5000,
    dist_fun = "manhattan",
    kernel_width = 3,
    n_features = 10,
    feature_select = "lasso_path",
    labels = "Yes"
)

plot_features(explanation_xgb)
