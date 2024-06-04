#library imports
library(dplyr)
library(tidyr) #used for separate() function
library(lattice) #used for correlation heatmap
library(reshape2) #used for melt function in heatmap
library(ggplot2)

#import dataset of one month traffic data
data <- read.csv("oneMonthDataset.csv")
#add id column with serial number
data$id <- seq.int(nrow(data))

#summary of orignal dataset
summary(data)

#feature engineering
numericReplacedData <- data
dataColNames <- colnames(numericReplacedData)
#repace low, normal, high, heavy with numeric 1, 2, 3 and 4 for easier analysis
numericReplacedData %>% distinct(Traffic.Situation)
numericReplacedData %>% count(Traffic.Situation)
numericReplacedData$Traffic.Situation <- c(low = 1, normal = 2, high = 3, heavy = 4)[numericReplacedData$Traffic.Situation]
#replace days of week sunday to saturady with numeric values 1 to 7
numericReplacedData %>% distinct(Day.of.the.week)
numericReplacedData %>% count(Day.of.the.week)
numericReplacedData$Day.of.the.week <- c(Sunday = 1, Monday = 2, Tuesday = 3, Wednesday = 4, Thursday = 5, Friday = 6, Saturday = 7)[numericReplacedData$Day.of.the.week]
#separate time into hour minute and seconds (AM/PM is attachd with seconds in Part3 column)
numericReplacedData <- numericReplacedData %>% separate(Time, into = c('Hour', 'Minute', 'Part3'), sep = ':')
#separate seconds and AM/PM
numericReplacedData <- numericReplacedData %>% separate(Part3, into = c('Seconds', 'Part.Of.Day'), sep = ' ')
#replace Am and PM with numeric 0 nd 1 for easier analysis
numericReplacedData %>% distinct(Part.Of.Day)
numericReplacedData %>% count(Part.Of.Day)
numericReplacedData$Part.Of.Day <- c(AM = 0, PM = 1)[numericReplacedData$Part.Of.Day]
#check for distinct vales in newly created columns
numericReplacedData %>% distinct(Hour)
numericReplacedData %>% distinct(Minute)
numericReplacedData %>% distinct(Seconds)
numericReplacedData %>% distinct(Part.Of.Day)
#remove 'Seconds' column as there is only 1 distinct values
numericReplacedData$Seconds <- NULL
#convert 'hour' and 'minute' into numeric data type
numericReplacedData$Hour <- as.integer(numericReplacedData$Hour)
numericReplacedData$Minute <- as.integer(numericReplacedData$Minute)
#summary of modified dataset (after factoring all columns into numeric datatype)
summary(numericReplacedData)

#correlation and heat map for upadted dataset (numericReplacedData)
#find correlation between all numeric columns
corDataSet <- subset(numericReplacedData, select = -c( Date, Day.of.the.week, id))
correlationMatrix = round(cor(corDataSet), 3)
correlationMatrix
#plot heat map for the correlation matrix
meltedCorrelationMatrix <- melt(correlationMatrix) 
meltedCorrelationMatrix
ggplot(data = meltedCorrelationMatrix, aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile() + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4)

par(mfrow = c(2, 2))
hist(numericReplacedData$CarCount , main = "Car Count", xlab = "Weight",ylim = c(0,1000),col = "yellow",border = "blue")
hist(numericReplacedData$BikeCount , main = "Bike Count", xlab = "Weight",ylim = c(0,1000),col = "yellow",border = "blue")
hist(numericReplacedData$TruckCount , main = "Truck Count", xlab = "Weight",ylim = c(0,1000),col = "yellow",border = "blue")
hist(numericReplacedData$BusCount , main = "Bus Count", xlab = "Weight",ylim = c(0,1000),col = "yellow",border = "blue")

write.csv(numericReplacedData, "numericDataset.csv", row.names = FALSE)
