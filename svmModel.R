#library imports
library(ggplot2)
library(e1071) 
library(caret) #confusion matrix

#import dataset of one month traffic data
numericData <- read.csv("numericDataset.csv")
testData <- read.csv("test.csv")
trainData <- read.csv("train.csv")

classifier = svm(formula = Traffic.Situation ~ ., data = trainData, type = 'C-classification', kernel = 'linear') 
predictedFactors = predict(classifier, newdata = testData) 
testFactors <- factor(testData$Traffic.Situation, levels = c("1", "2", "3", "4"))
conf_matrix <- confusionMatrix(table(testFactors, predictedFactors))
conf_matrix
conf_matrix$overall['Accuracy']
accuracy <- read.csv("Accuracy.csv", header = TRUE)
accuracy <- rbind(accuracy, c("svm", conf_matrix$overall['Accuracy']))
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
write.csv(predictedData, "svmPredictions.csv")

