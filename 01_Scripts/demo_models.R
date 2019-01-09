

vip(glmnet.fit, fill = "#2b8cbe") + ggtitle("glmnet")
vip(gbm.fit, fill = "#2b8cbe") + ggtitle("GBM")
vip(nnet.fit, fill = "#2b8cbe") + ggtitle("nnet") 
vip(mars.fit, fill = "#2b8cbe") + ggtitle("MARS") 
vip(xgb.fit, fill = "#2b8cbe") + ggtitle("xgBoost") 