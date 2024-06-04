#library imports
library(lattice) #used for correlation heatmap
library(reshape2) #used for melt funtion in heatmap
library(ggplot2)

#import dataset of one month traffic data
data <- read.csv("numericDataset.csv")

#summary of orignal dataset
summary(data)

#splitting training and testing data
train <- data %>% dplyr::sample_frac(0.70)
write.csv(train, "train.csv", row.names = FALSE)
test  <- dplyr::anti_join(data, train, by = 'id')
write.csv(test, "test.csv", row.names = FALSE)

empty <- data.frame(model = c(""), accuracy = c(""))
write.csv(empty, "Accuracy.csv", row.names = FALSE)

#xtest <- subset(test, select = -c(Traffic.Situation))
#ytest <- subset(test, select = c(Traffic.Situation))
#xtrain <- subset(train, select = -c(Traffic.Situation))
#ytrain <- subset(train, select = c(Traffic.Situation))