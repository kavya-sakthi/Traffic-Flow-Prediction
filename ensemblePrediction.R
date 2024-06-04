library(caret)

knn <- read.csv("knnPredictions.csv")
svm <- read.csv("svmPredictions.csv")
rf <- read.csv("rfPredictions.csv")
xgb <- read.csv("xgbPredictions.csv")
accuracy <- read.csv("Accuracy.csv")

predictedValuesdf <- data.frame(check = rep(NA, nrow(knn)))
predictedValuesdf$actual <- knn$testData.Traffic.Situation
predictedValuesdf$knn <- knn$predictedTrafficSituation
predictedValuesdf$svm <- svm$predictedTrafficSituation
predictedValuesdf$rf <- rf$predictedTrafficSituation
predictedValuesdf$xgb <- xgb$predictedTrafficSituation
predictedValuesdf <- predictedValuesdf[-1]

write.csv(predictedValuesdf, "predictedTraffic.csv")

logKnn <- data.frame(log(knn$predictedTrafficSituation))
logRf <- data.frame(log(rf$predictedTrafficSituation))
logSvm <- data.frame(log(svm$predictedTrafficSituation))
logXgb <- data.frame(log(xgb$predictedTrafficSituation))

meanPredicted <- data.frame((logKnn$log.knn.predictedTrafficSituation.+logRf$log.rf.predictedTrafficSituation.+logSvm$log.svm.predictedTrafficSituation.)/3)
ensemblePrediction <- as.integer(exp(meanPredicted$X.logKnn.log.knn.predictedTrafficSituation....logRf.log.rf.predictedTrafficSituation....))
actualPrediction <- predictedValuesdf$actual

conf_matrix <- confusionMatrix(table(actualPrediction, ensemblePrediction))
conf_matrix
conf_matrix$overall['Accuracy']


