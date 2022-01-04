# Melissa Hunfalvay. Last Revised: 7-6-2021
# Data 630
# Credit Approval Dataset
# Assignment 3
# Professor Firdu

# 1. Introduction

#Loading the data in R and preview it

# Set working directory and read the data
setwd("/Users/melissahunfalvay/Documents/HUN/My Professional Development/Machine Learning Data 630/Assignments/Assignment 3") 
# display the file names in the current working directory
dir()

#Use the read.csv command to load the data into RStudio.  
cmc<-read.csv(file="cmc.csv", header=TRUE, sep=",", as.is = FALSE)

# Load packages
# Load the packsge into memory (each time you start the new session)
#install.packages("party")
library("party")
library (arules)
#install.packages("caret")
require(caret)
#install.packages("e1071")
require(e1071)
#install.packages("rpart.plot")
require (rpart.plot)

# 2. Exploratory analysis

View(cmc)
str(cmc)
summary(cmc)

# List variables have missing values
# Check for all variables
apply(cmc, 2, function (cmc) sum(is.na(cmc)))

# Exploratory analysis - Wifes' Age Variable type = int
# Descriptive Statistics
summary (cmc$WifeAge)
# Standard deviation 
sd(cmc$WifeAge)
# Mode  
Data_WifeAge <- cmc
names(sort(-table(Data_WifeAge$WifeAge)))[1]
#histogram
hist(cmc$WifeAge, main="Wifes'Age", xlab="Years", ylab = "Frequency") 
#box plot
boxplot(cmc$WifeAge, main="Wifes'Age", xlab="Years", ylab = "Frequency") # Outliers? Yes

# Exploratory analysis - Wifes' Education, Variable type = factor
# Change 1-4 levels to descriptive terms for easier analysis
# Change variable from int to factor
cmc$WifeEducation<-factor(cmc$WifeEducation, levels = 1:4, labels = c("Low", "Low-mod", "mod-high", "high"))
# show first 100 raw variables
head(cmc$WifeEducation, 100)
# Count the number of values within each category
table(cmc$WifeEducation)
# Percentage
table(cmc$WifeEducation)/length(cmc$WifeEducation)
# Bar plot
barplot(table(cmc$WifeEducation))

# Exploratory analysis - Husbands' Education, Variable type = factor
# Change 1-4 levels to descriptive terms for easier analysis
# Change variable from int to factor
cmc$HusbandEducation<-factor(cmc$HusbandEducation, levels = 1:4, labels = c("Low", "Low-mod", "mod-high", "high"))
# show first 100 raw variables
head(cmc$HusbandEducation, 100)
# Count the number of values within each category
table(cmc$HusbandEducation)
# Percentage
table(cmc$HusbandEducation)/length(cmc$HusbandEducation)
# Bar plot
barplot(table(cmc$HusbandEducation))

# Exploratory analysis - Number of Children, Variable type = int
# Descriptive Statistics
summary (cmc$NumChildren)
# Standard deviation 
sd(cmc$NumChildren)
# Mode  
Data_NumChildren <- cmc
names(sort(-table(Data_NumChildren$NumChildren)))[1]
#histogram
hist(cmc$NumChildren, main="Number of Children", xlab="Count", ylab = "Frequency") # Left Skewed
#box plot
boxplot(cmc$NumChildren, main="Number of Children", xlab="Count", ylab = "Frequency") # Outliers? Yes

# Exploratory analysis - Wifes' Religion, Variable type = factor
# Change 0:1 levels to descriptive terms for easier analysis
# Change variable from int to factor
cmc$WifeReligion<-factor(cmc$WifeReligion, levels = 0:1, labels = c("Non-Islamic", "Islamic"))
# show first 100 raw variables
head(cmc$WifeReligion, 100)
# Count the number of values within each category
table(cmc$WifeReligion)
# Percentage
table(cmc$WifeReligion)/length(cmc$WifeReligion)
# Bar plot
barplot(table(cmc$WifeReligion))

# Exploratory analysis - Wifes' Working, Variable type = factor
# Change 0:1 levels to descriptive terms for easier analysis
# Change variable from int to factor
cmc$WifeWorking<-factor(cmc$WifeWorking, levels = 0:1, labels = c("Yes", "No"))
# show first 100 raw variables
head(cmc$WifeWorking, 100)
# Count the number of values within each category
table(cmc$WifeWorking)
# Percentage
table(cmc$WifeWorking)/length(cmc$WifeWorking)
# Bar plot
barplot(table(cmc$WifeWorking))
pie(table(cmc$WifeWorking))

# Exploratory analysis - Husband Occupation, Variable type = factor
# Change 1-4 levels to descriptive terms for easier analysis
# Change variable from int to factor
cmc$HusbandOccupation<-factor(cmc$HusbandOccupation, levels = 1:4, labels = c("Low", "Low-mod", "mod-high", "high"))
# show first 100 raw variables
head(cmc$HusbandOccupation, 100)
# Count the number of values within each category
table(cmc$HusbandOccupation)
# Percentage
table(cmc$HusbandOccupation)/length(cmc$HusbandOccupation)
# Bar plot
barplot(table(cmc$HusbandOccupation))

# Exploratory analysis - Living Standard Index, Variable type = factor
# Change 1-4 levels to descriptive terms for easier analysis
# Change variable from int to factor
cmc$LivingStandardIndex<-factor(cmc$LivingStandardIndex, levels = 1:4, labels = c("Low", "Low-mod", "mod-high", "high"))
# show first 100 raw variables
head(cmc$LivingStandardIndex, 100)
# Count the number of values within each category
table(cmc$LivingStandardIndex)
# Percentage
table(cmc$LivingStandardIndex)/length(cmc$LivingStandardIndex)
# Bar plot
barplot(table(cmc$LivingStandardIndex))

# Exploratory analysis - Media Exposure, Variable type = factor
# Change 0:1 levels to descriptive terms for easier analysis
# Change variable from int to factor
cmc$MediaExposure<-factor(cmc$MediaExposure, levels = 0:1, labels = c("Good", "Not Good"))
# show first 100 raw variables
head(cmc$MediaExposure, 100)
# Count the number of values within each category
table(cmc$MediaExposure)
# Percentage
table(cmc$MediaExposure)/length(cmc$MediaExposure)
# Bar plot
barplot(table(cmc$MediaExposure))

# Exploratory analysis - Contraceptive Method, Variable type = factor
# Change 0:1 levels to descriptive terms for easier analysis
# Change variable from int to factor
# cmc$ContraceptiveMethod<-factor(cmc$ContraceptiveMethod, levels = 1:3, labels = c("No Use", "Long Term", "Short Term"))
# cmc$ContraceptiveMethod=as.factor(cmc$ContraceptiveMethod)

# Experiment for accuracy
# Changing variable to if contraceptivementhod was use: 1(No-use), 0(use - short or long term)
cmc[which(cmc$ContraceptiveMethod>1),"ContraceptiveMethod"] = 0
cmc$ContraceptiveMethod=as.factor(cmc$ContraceptiveMethod)

# show first 100 raw variables
head(cmc$ContraceptiveMethod, 100)
# Count the number of values within each category
table(cmc$ContraceptiveMethod)
# Percentage
table(cmc$ContraceptiveMethod)/length(cmc$ContraceptiveMethod)
# Bar plot
barplot(table(cmc$ContraceptiveMethod))

# 3. Data Pre-processing

# a) change categorical and binary variable to factors
# done above as part of the labeling changes
str(cmc)

# b) Explore numeric variables for data sanity check
# Wife's age (numerical) - check range
summary(cmc$WifeAge) 
hist((cmc$WifeAge))
# Number of children ever born (numerical) - check range
summary(cmc$NumChildren) 
hist((cmc$NumChildren))
boxplot(cmc$NumChildren) # shows outliers, explore more
max(cmc$NumChildren)
cmc[which(cmc$NumChildren>15),"WifeAge"] # 48 year old, legitimate

# 4. Model Development

# a.	Training and test data
set.seed(1234)
ind <- sample(2, nrow(cmc), replace = TRUE, prob = c(0.7, 0.3))
train.data <- cmc[ind == 1, ]
test.data <- cmc[ind == 2, ]
str(train.data)
str(test.data)

# b. Model is a variable where model object is stored
model<-ctree(ContraceptiveMethod~., train.data)
# Output the model
print(model)                               
# Pruning: Output the model from node 2
nodes(model, 2) 

# c.	Visualize the tree 
plot(model)
plot(model, type="simple")
plot(model,type="extended")

# Additional plot options
plot(model, inner_panel = node_inner(model, fill = c("green", "blue"), id=TRUE))

plot(model, inner_panel = node_inner(model, fill = c("green", "blue"), id=TRUE), drop_terminal = FALSE)

# simpler version of plot
plot(model, type="simple",           # no terminal plots
     inner_panel=node_inner(model,
                            abbreviate = TRUE,            # short variable names
                            pval = TRUE,                 # no p-values
                            id = FALSE),                  # no id of node
     terminal_panel=node_terminal(model, 
                                  abbreviate = TRUE,
                                  digits = 1,             # few digits on numbers
                                  fill = c("white"),      # make box white not gray
                                  id = FALSE)
)

# d.Pruning: Set the maximum tree depth
modelA<-ctree(ContraceptiveMethod~., train.data, control=ctree_control(maxdepth = 5 ))
plot(modelA)
sum(predict(modelA)== train.data$ContraceptiveMethod)/length(train.data$ContraceptiveMethod)  # classification accuracy                    

# e.	Confusion matrix Training data
trainPred <- predict(model, newdata = train.data, method="ContraceptiveMethod")  # predictions for the train data
confusionMatrix(trainPred, train.data$ContraceptiveMethod, dnn=c("predicted", "actual")) # additional statistics
sum(predict(model)== train.data$ContraceptiveMethod)/length(train.data$ContraceptiveMethod)  # classification accuracy

# f.	Evaluate the model on test data
model<-ctree(ContraceptiveMethod~., test.data)
testPred <- predict(model, newdata = test.data, method="ContraceptiveMethod")  # predictions for the test data

# g.	Confusion matrix Test data
confusionMatrix(testPred, test.data$ContraceptiveMethod, dnn=c("predicted", "actual")) # additional statistics
sum(testPred== test.data$ContraceptiveMethod)/length(test.data$ContraceptiveMethod)            # classification accuracy
