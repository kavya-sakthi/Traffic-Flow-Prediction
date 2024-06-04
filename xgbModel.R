#library imports
library(ggplot2)
library(xgboost)
library(caret) #confusion matrix

#import dataset of one month traffic data
numericData <- read.csv("numericDataset.csv")
testData <- read.csv("test.csv")
trainData <- read.csv("train.csv")

#summary of test and training dataset
summary(testData)
summary(trainData)

#XGBoost
#Full dataset --> numericReplacedData
#Testing dataset --> test
#Training dataset --> train

numericData$Traffic.Situation <- as.factor(numericData$Traffic.Situation)
xgb_model <- xgboost(data = as.matrix(trainData[, !(names(trainData) %in% "Traffic.Situation")]), 
                     label = trainData$Traffic.Situation,
                     nrounds = 100, # Number of boosting rounds
                     verbose = TRUE) 
testDataFrame <- as.matrix(testData[, -which(names(testData) == "Traffic.Situation")])
predictedTrafficSituation <- predict(xgb_model, testDataFrame)
actualTrafficSituation = as.factor(testData$Traffic.Situation)
predictedTrafficSituation <- factor(round(predictedTrafficSituation), levels = levels(actualTrafficSituation))
conf_matrix <- confusionMatrix(actualTrafficSituation, predictedTrafficSituation)
conf_matrix
conf_matrix$overall['Accuracy']
accuracy <- read.csv("Accuracy.csv", header = TRUE)
accuracy <- rbind(accuracy, c("xgb", conf_matrix$overall['Accuracy']))
write.csv(accuracy, "Accuracy.csv", row.names = FALSE)
heatmap(conf_matrix$table,
        Rowv = NA,
        Colv = NA,
        col = colorRampPalette(c("white", "blue"))(100), # Choose a color gradient
        scale = "none",
        xlab = "predictedFactors",
        ylab = "testFactors",
        main = "Heatmap of Matrix",
        labRow = c(1,2,3,4),
        labCol = c(1,2,3,4))
predictedData = data.frame(testData$Traffic.Situation, predictedTrafficSituation)
write.csv(predictedData, "xgbPredictions.csv")