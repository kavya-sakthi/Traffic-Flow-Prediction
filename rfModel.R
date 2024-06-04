#library imports
library(ggplot2)
library(randomForest)
library(caret) #confusion matrix

#import dataset of one month traffic data
numericData <- read.csv("numericDataset.csv")
testData <- read.csv("test.csv")
trainData <- read.csv("train.csv")

#summary of test and training dataset
summary(testData)
summary(trainData)

#random forest
rfModel <- randomForest(Traffic.Situation ~ ., data = trainData)
importance(rfModel)
varImpPlot(rfModel) 
predictedTrafficSituation <- predict(rfModel, testData, type= "response")
predictedTrafficSituation <- round(predictedTrafficSituation)
predictedTrafficSituation <- factor(predictedTrafficSituation, levels = c("1", "2", "3", "4"))
actualTrafficSituation <- factor(testData$Traffic.Situation, levels = c("1", "2", "3", "4"))
conf_matrix <- confusionMatrix(table(actualTrafficSituation, predictedTrafficSituation))
conf_matrix
conf_matrix$overall['Accuracy']
accuracy <- read.csv("Accuracy.csv", header = TRUE)
accuracy <- rbind(accuracy, c("rf", conf_matrix$overall['Accuracy']))
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
write.csv(predictedData, "rfPredictions.csv")