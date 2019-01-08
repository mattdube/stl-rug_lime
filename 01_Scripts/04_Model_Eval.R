library(tibble)


model_list <- list("xgb" = xgb.fit, "GradientBoosting" = gbm.fit, "MARS" = mars.fit,
                   "NeuralNet" = nnet.fit)

resamples <- resamples(model_list)
dotplot(resamples, metric="ROC", main = "Area Under Curve with 95% CI")
bwplot(resamples)
densityplot(resamples, metric="ROC")


gbmcm <- confusionMatrix(gbm.predict, testData$Churn, positive = "Yes")
gbm_accuracy <- c(gbmcm$overall[c(1,3,4)],gbmcm$byClass[7])

glmnetcm <- confusionMatrix(glmnet.predict, testData$Churn, positive = "Yes")
glmnet_accuracy <- c(glmnetcm$overall[c(1,3,4)],glmnetcm$byClass[7])

nnet_cm <- confusionMatrix(nnnet.predict, testData$Churn, positive = "Yes")
nnet_accuracy <- c(nnet_cm$overall[c(1,3,4)],nnet_cm$byClass[7])

mars_cm<- confusionMatrix(mars.predict, testData$Churn, positive = "Yes")
mars_accuracy <- c(mars_cm$overall[c(1,3,4)],mars_cm$byClass[7])

xgb_cm <- confusionMatrix(xgb.predict, testData$Churn, positive = "Yes")
xgb_accuracy <- c(xgb_cm$overall[c(1,3,4)], xgb_cm$byClass[7])


models <- c("GBM", "glmenet", "nnet", "MARS", "xgb")

accuracysummary <- bind_rows(GBM = gbm_accuracy, glmnet = glmnet_accuracy, nnet = nnet_accuracy, 
                             MARS = mars_accuracy, xGB = xgb_accuracy)


accuracysummary2 <- add_column(accuracysummary, "Model" = models, .before = "Accuracy")

accuracysummary2

str(gbm.predict)
str( testData$Churn)
