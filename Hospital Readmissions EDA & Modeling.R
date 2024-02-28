#A2 - Catarina Jesus, Student id: 26810706
#Visualizing & Analyzing Data with R: Methods & Tools - DAT-5323 - BMBAN1
#April 8th 2023

## Set the working directory
setwd("~/Documents/Spring Semester/Part 2/Visualizing & Analyzing Data with R/A2 assignment")

# Libs
library(readr)
library(ggplot2)
library(ggthemes)
library(leaflet)
library(dplyr)
library(tidyr)
library(tidyverse)
library(corrplot)
library(vtreat)
library(ModelMetrics)
library(knitr)
library(caret)
library(randomForest)


# Options
options(scipen=999)

# Load the data sources
hosptrain <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A2_Hospital_Readmission/caseData/diabetesHospitalInfoTrain.csv')
medstrain <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A2_Hospital_Readmission/caseData/diabetesMedsTrain.csv')
patienttrain <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A2_Hospital_Readmission/caseData/diabetesPatientTrain.csv')
hosptest <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A2_Hospital_Readmission/caseData/diabetesHospitalInfoTest.csv')
medstest <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A2_Hospital_Readmission/caseData/diabetesMedsTest.csv')
patienttest <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A2_Hospital_Readmission/caseData/diabetesPatientTest.csv')


#See nulls 
colSums(is.na(hosptrain))
colSums(is.na(medstrain))
colSums(is.na(patienttrain))
colSums(is.na(hosptest))
colSums(is.na(medstest))
colSums(is.na(patienttest))

#See blanks
colSums(hosptrain == "")
colSums(medstrain == "")
colSums(patienttrain == "")
colSums(hosptest == "")
colSums(medstest == "")
colSums(patienttest == "")

#Left join of the hospital train database with meds train database
semifinal <- merge(hosptrain, medstrain, by = "tmpID")

#Left join of the previous database with patient train database
final <- merge(semifinal, patienttrain, by = "tmpID")

##############################################################################
#EDA for Train datasets

#Drop columns
final1 <- final[, -c(2,3,4, 6:44)]

# Group by time_in_hospital
final1 <- final1 %>%
  group_by(time_in_hospital) %>%
  summarise(total_count=n(),
            .groups = 'drop')
final1

#Create a barplot
ggplot(final1, aes(x = factor(time_in_hospital), y = total_count)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  geom_text(aes(label = total_count), vjust = -0.5) +
  labs(x = "Time in Hospital", y = "Number of patients") +
  ggtitle("Number of patients per times in the Hospital") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#Second EDA

#Eliminate columns
final2 <- final[, -c(1:39, 42:44)]

#Create bins of age column
final2$AgeBin <- cut(final2$age, 
                               breaks = c(30, 40, 60, 80, Inf), 
                               labels = c("30-40", "41-60", "61-80", ">80"))

# Count occurrences of each combination of AgeBin and Gender
final2 <- table(final2$AgeBin, final2$gender)
final2 <- as.data.frame(final2)
final2

#Change name of the columns
colnames(final2)[1] <- "Age"
colnames(final2)[2] <- "Gender"
colnames(final2)[3] <- "Population"

#Create barplot
ggplot(final2, aes(x = Age, y = Population, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Population), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Patients by Age and Gender", x = "Age", y = "Patients") +
  scale_fill_manual(values = c("pink", "lightblue")) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#Third EDA

#Eliminate columns
final3 <- final[, -c(1:40,43,44)]

#Create bins of age column
final3$AgeBin <- cut(final3$age, 
                               breaks = c(30, 40, 60, 80, Inf), 
                               labels = c("30-40", "41-60", "61-80", ">80"))

final3

# Group by ageBin and calculate mean of weight
final3 <- final3 %>%
  group_by(AgeBin) %>%
  mutate(mean2 = mean(wgt))

# Calculate mean of weight by ageBin using aggregate function
mean_wgt1 <- aggregate(wgt ~ AgeBin, final3, mean)

# round the values to 0 decimal points
mean_wgt1$wgt <- round(mean_wgt1$wgt, 0)

# Create the bar plot
ggplot(mean_wgt1, aes(x = AgeBin, y = wgt)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  geom_text(aes(label = wgt), vjust = -0.5) +
  labs(x = "Age", y = "Weight") +
  ggtitle("Average weight by Age") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

#Fourth EDA

#Eliminate columns
final4 <- final[, -c(1:6,8:39,42,43,44)]

#Create bins of age column
final4$AgeBin <- cut(final4$age, 
                     breaks = c(30, 40, 60, 80, Inf), 
                     labels = c("30-40", "41-60", "61-80", ">80"))

# Group by ageBin and gender and calculate sum of lab procedures
final4 <- final4 %>%
  group_by(AgeBin, gender) %>%
  mutate(sum1 = sum(num_lab_procedures))
final4

# Calculate mean of weight by ageBin using aggregate function
sum_labproc <- aggregate(num_lab_procedures ~ AgeBin + gender, final4, sum)

#Create barplot
ggplot(sum_labproc, aes(x = AgeBin, y = num_lab_procedures, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = num_lab_procedures), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Number of lab procedures by Age and Gender", x = "Age", y = "Number of lab procedures") +
  scale_fill_manual(values = c("pink", "lightblue")) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


##########################################################################
## Machine Learning Model
#Logistic Regression Model

#The following columns were dropped because of the following reasons:
# 1. diag_1_desc, diag_2_desc and diag_3_desc don´t give relevant information for analysis, each patient has a different diagnostic description 
# 2. The columns with all of the different medication are related with medication for diabetes type 2 and there is a column "diabetesMed" that already says if the patient takes diabetes medication
# Drop columns
final <- final[, -c(14:16,19:36)]
names(final)

#Following we need to identify the column that we want to predict - targetVar in the data frame "final"
#The informativeVars are the columns that we want to use to make predictions about the target variable
targetVar       <- names(final)[23]
informativeVars <- names(final)[c(1:22)] 

#The first line of code sets a seeds for reproducibility 
#The second line of code assigns the variable "idx" to a random sample of 10% of the row indices in the "final" data frame with the sample function. The nrow function is used to get the number of rows in the dataframe
#The third line of code assigns the variable "prepData" to create a new data frame with the random 10% of the rows that were select on the previous "idx" 
#The fourth line of code assigns the variable "nonPrepData" to create a new data frame with the random 90% of the rows that were not included in "idx" 
set.seed(1234)
idx         <- sample(1:nrow(final),.1*nrow(final))
prepData    <- final[idx,]
nonPrepData <- final[-idx,]

#The designTreatments function is used to create a matrix of treatment combinations for a set of predictors
#The function returns all of possible. combinations of the levels of predictors
# Design a "C"ategorical variable plan 
plan <- designTreatmentsC(prepData, 
                          informativeVars,
                          targetVar, 1)

# The function returns a data frame that contains the treated predictors for modeling 
treatedX <- prepare(plan, nonPrepData)

#The first line of code sets a seeds for reproducibility 
#The second line of code assigns the variable "idx" to a random sample of 80% of the row indices in the "treatedX" data frame with the sample function. The nrow function is used to get the number of rows in the data frame
#The third line of code assigns the variable "train" to create a new data frame with the random 80% of the rows that were select on the previous "idx" 
#The fourth line of code assigns the variable "validation" to create a new data frame with the random 20% of the rows that were not included in "idx" 
#This creates 2 data frames, one for model training and one for model validation which allows the development and evaluation of a predictive model using the training set and then evaluation of the performance using the validation set
# Partition to avoid over fitting
set.seed(2022)
idx        <- sample(1:nrow(treatedX),.8*nrow(treatedX))
train      <- treatedX[idx,]
validation <- treatedX[-idx,]

# Fit a logistic regression model
fit <- glm(readmitted_y ~., data = train, family ='binomial')
summary(fit)

#Perform a stepwise regression. The backwards means that the function starts will all predictors and iteratively removes one predictor at a time until the optimal subset of predictors is reached
# Backward Variable selection to reduce chances of multi-colinearity
bestFit <- step(fit, direction='backward')

#saveRDS(bestFit, 'bestFit.rds')
summary(bestFit)

# Compare model size
length(coefficients(fit))
length(coefficients(bestFit))

# This will predict from the "bestFit" model for each observation in the "validation" data frame 
teamPreds <- predict(bestFit,  validation, type='response')
tail(teamPreds)

# Classify - use a threhold value which converts the predicted variables into binary predictors 
cutoff      <- 0.5
teamClasses <- ifelse(teamPreds >= cutoff, 1,0)

#This will display the results of the probabilities 
#Actual - actual target values from the non-prepossessed data frame for the validation set 
#id - identifier for each observation in the validation set 
#classes - binary predictions for each observation in the validation set 
#probs - predicted probabilities for the positive class for each observation in the validation set 
results <- data.frame(actual  = nonPrepData[-idx,]$readmitted_y,
                      id   = nonPrepData[-idx,]$tmpID,
                      classes = teamClasses,
                      probs   = teamPreds)
head(results)

# Get a confusion matrix
#The objective is to summarize the perfomance of the binary classification model by comparing the binary predictions to the actual target variables values 
confMat <- ConfusionMatrix(results$classes, results$actual)

# What is the accuracy?
#### VALUE = 66% of accuracy
Accuracy(results$classes, results$actual)

###############################################################################
## RandomForest Model

# Identify the informative and target
targetVar       <- names(final)[23]
informativeVars <- names(final)[c(1:22)] 

#### SAMPLE
# Segment the prep data
set.seed(1234)
idx         <- sample(1:nrow(final),.1*nrow(final))
trainDat    <- final[idx,]
testDat <- final[-idx,]


# Design a "C"ategorical variable plan 
plan <- designTreatmentsC(trainDat, 
                          informativeVars,
                          targetVar, 1)

treatedTrain <- prepare(plan, trainDat)
treatedTest  <- prepare(plan, testDat)

treatedTrain$readmitted_y <- as.factor(treatedTrain$readmitted_y)

# Fit a random forest model with Caret
downSampleFit <- train(readmitted_y ~ .,
                       data = treatedTrain,
                       method = "rf",
                       verbose = FALSE,
                       ntree = 3,
                       tuneGrid = data.frame(mtry = 1))

downSampleFit


# Too see probabilities
predProbs   <- predict(downSampleFit,  
                       treatedTrain, 
                       type = c("prob"))
head(predProbs)

# To get classes with 0.50 cutoff
predClasses <- predict(downSampleFit,  treatedTrain)

# Confusion Matrix; MLmetrics has the same function but use CARET in this example.
caret::confusionMatrix(predClasses, as.factor(treatedTrain$readmitted_y))

# Other interesting model artifacts
varImp(downSampleFit)
plot(varImp(downSampleFit), top = 20)

# Add more trees to the forest with the randomForest package (caret takes a long time bc its more thorough)
moreVoters <- randomForest(as.factor(readmitted_y) ~ .,
                           data  = treatedTrain, 
                           ntree = 500,
                           mtry  = 1)

# Confusion Matrix, compare to 3 trees ~63% accuracy
trainClass <- predict(moreVoters, treatedTrain)
confusionMatrix(trainClass, as.factor(treatedTrain$readmitted_y))

# Let's optimize # of trees 
someVoters <- randomForest(as.factor(readmitted_y) ~ .,
                           data = treatedTrain, 
                           ntree=100,
                           mtyr = 1)

# Confusion Matrix
trainClass <- predict(someVoters, treatedTrain)
confusionMatrix(trainClass, as.factor(treatedTrain$readmitted_y))

### Now let's apply to the validation test set
threeVotes        <- predict(downSampleFit, treatedTest)
fiveHundredVoters <- predict(moreVoters,    treatedTest)
oneHundredVoters  <- predict(someVoters,    treatedTest)


#Accuracy Comparison from MLmetrics and natural occurence in the test set
Accuracy(testDat$readmitted_y, threeVotes)
Accuracy(testDat$readmitted_y, fiveHundredVoters)
Accuracy(testDat$readmitted_y, oneHundredVoters)
proportions(table(testDat$readmitted_y))

#############################################################################
### Second part of the exercise 

#Left join of the hospital train database with meds train database
test1 <- merge(hosptest, medstest, by = "tmpID")

#Left join of the previous database with patient train database
testfinal <- merge(test1, patienttest, by = "tmpID")
ncol(testfinal)
names(testfinal)

# Drop columns
testfinal <- testfinal[, -c(14:16,19:36)]

## Logistic model for the test dataframe

#New dataframe with the same features as the test of the LGM
testnew <- prepare(plan, testfinal)

nrow(testnew)

#The purpose is to get the best 100 probabilities
readmissionsprob <- predict(bestFit, testnew, type ='response')
head(readmissionsprob)

# Classify 
cutofffinal      <- 0.5
patientreadminTest <- ifelse(readmissionsprob >= cutofffinal, 1,0)

#### ASSESS
# Organize w/Actual
resultstest <- data.frame(actual  = testfinal$readmitted_y,
                      id   = testfinal$tmpID,
                      classes = patientreadminTest,
                      probs   = readmissionsprob)
head(resultstest)


# Get a confusion matrix
(confMat <- ConfusionMatrix(resultstest$classes, resultstest$actual))

# What is the accuracy?
#### VALUE = 65,48% of accuracy
Accuracy(resultstest$classes, resultstest$actual)

############################################################################################

# Sort the dataframe by value_column in descending order and select the top 100 records
top_values <- resultstest %>% arrange(desc(probs)) %>% head(100)

top_values

#Change name of the columns
colnames(testfinal)[1] <- "id"

#Left join of the top_values dataframe with testfinal dataframe
finaltable_test <- merge(top_values, testfinal, by = "id")
head(finaltable_test)

# Save the data frame as a CSV file
write.csv(finaltable_test, "finaldata.csv")

# Drop columns
final_top_patients <- top_values[, -c(1,3)]

#Change name of the columns
colnames(final_top_patients)[1] <- "ID"
colnames(final_top_patients)[2] <- "Probability"

# Save the data frame as a CSV file - top 100 patients with ID and probability
write.csv(final_top_patients, "Top Patients.csv", row.names = FALSE)

###EDA of finaltable_test

# function to find most repeated value in a column
most_repeated <- function(col) {
  freq_table <- table(col)
  names(freq_table)[which.max(freq_table)]
}

# apply function to each column of dataframe
most_repeated_values <- apply(finaltable_test, 2, most_repeated)

# print results
most_repeated_values


## First EDA

#Eliminate columns
finaltable_test1 <- subset(finaltable_test, select = c("id", "time_in_hospital"))

# Group by time_in_hospital
finaltable_test1 <- finaltable_test1 %>%
  group_by(time_in_hospital) %>%
  summarise(total_count=n(),
            .groups = 'drop')
finaltable_test1

#Create a barplot

ggplot(finaltable_test1, aes(x = factor(time_in_hospital), y = total_count)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  geom_text(aes(label = total_count), vjust = -0.5) +
  labs(x = "Time in Hospital", y = "Number of patients") +
  ggtitle("Number of patients per times in the Hospital") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#Second EDA

#Eliminate columns
finaltable_test2 <- subset(finaltable_test, select = c("age", "gender"))

#Create bins of age column
finaltable_test2$AgeBin <- cut(finaltable_test2$age, 
                               breaks = c(30, 40, 60, 80, Inf), 
                               labels = c("30-40", "41-60", "61-80", ">80"))

# Count occurrences of each combination of AgeBin and Gender
finaltable_test2 <- table(finaltable_test2$AgeBin, finaltable_test2$gender)
finaltable_test2 <- as.data.frame(finaltable_test2)
finaltable_test2

#Change name of the columns
colnames(finaltable_test2)[1] <- "Age"
colnames(finaltable_test2)[2] <- "Gender"
colnames(finaltable_test2)[3] <- "Population"

#Create barplot
ggplot(finaltable_test2, aes(x = Age, y = Population, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Population), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Patients by Age and Gender", x = "Age", y = "Patients") +
  scale_fill_manual(values = c("pink", "lightblue")) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#Third EDA

#Eliminate columns
finaltable_test3 <- subset(finaltable_test, select = c("age", "wgt"))

#Create bins of age column
finaltable_test3$AgeBin <- cut(finaltable_test3$age, 
                               breaks = c(30, 40, 60, 80, Inf), 
                               labels = c("30-40", "41-60", "61-80", ">80"))

finaltable_test3

# Group by ageBin and calculate mean of weight
finaltable_test3 <- finaltable_test3 %>%
  group_by(AgeBin) %>%
  mutate(mean1 = mean(wgt))

# Calculate mean of weight by ageBin using aggregate function
mean_wgt <- aggregate(wgt ~ AgeBin, finaltable_test3, mean)

# round the values to 0 decimal points
mean_wgt$wgt <- round(mean_wgt$wgt, 0)

# Create the bar plot
ggplot(mean_wgt, aes(x = AgeBin, y = wgt)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  geom_text(aes(label = wgt), vjust = -0.5) +
  labs(x = "Age", y = "Weight") +
  ggtitle("Weight by Age") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

#Fourth EDA

#Eliminate columns
finaltable_test4 <- subset(finaltable_test, select = c("age", "num_lab_procedures", "gender"))

#Create bins of age column
finaltable_test4$AgeBin <- cut(finaltable_test4$age, 
                     breaks = c(30, 40, 60, 80, Inf), 
                     labels = c("30-40", "41-60", "61-80", ">80"))

# Group by ageBin and gender and calculate sum of lab procedures
finaltable_test4 <- finaltable_test4 %>%
  group_by(AgeBin, gender) %>%
  mutate(sum2 = sum(num_lab_procedures))
finaltable_test4

# Calculate sum of number of lab procedures by ageBin using aggregate function
sum_labproc1 <- aggregate(num_lab_procedures ~ AgeBin + gender, finaltable_test4, sum)

#Create barplot
ggplot(sum_labproc1, aes(x = AgeBin, y = num_lab_procedures, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = num_lab_procedures), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Number of lab procedures by Age and Gender", x = "Age", y = "Number of lab procedures") +
  scale_fill_manual(values = c("pink", "lightblue")) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#########################################################################################
#From the patients with higher probability of being readmited, how many times on average they visit to the hospital

#Eliminate columns
finaltable_test5 <- subset(finaltable_test, select = c("probs", "time_in_hospital"))

#Create bins of probs column
finaltable_test5$probsBin <- cut(finaltable_test5$probs, 
                                 breaks = c(0.69, 0.75, 0.80, 0.85, 0.90, Inf), 
                                 labels = c("69%-75%", "75%-80%", "80%-85%", "90%-95%", ">95%"))


# Group by probsBin and calculate sum of times in hospital
finaltable_test5 <- finaltable_test5 %>%
  group_by(probsBin) %>%
  mutate(sum1 = sum(time_in_hospital))

# Calculate sum of time in hospital by probsBin using aggregate function
sum_tih <- aggregate(time_in_hospital ~ probsBin, finaltable_test5, sum)

# Create the bar plot
ggplot(sum_tih, aes(x = probsBin, y = time_in_hospital)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  geom_text(aes(label = time_in_hospital), vjust = -0.5) +
  labs(x = "Probability to be readmitted", y = "Time in Hospital") +
  ggtitle("Time in Hospital by Probability to be readmitted") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

#From the patients with higher probability of being readmited, how many medications they take

#Eliminate columns
finaltable_test6 <- subset(finaltable_test, select = c("probs", "num_medications"))

#Create bins of probs column
finaltable_test6$probsBin <- cut(finaltable_test6$probs, 
                                 breaks = c(0.68, 0.75, 0.80, 0.85, 0.90, Inf), 
                                 labels = c("69%-75%", "75%-80%", "80%-85%", "90%-95%", ">95%"))


# Group by probsBin and calculate sum of number of medication needed
finaltable_test6 <- finaltable_test6 %>%
  group_by(probsBin) %>%
  mutate(sum2 = sum(num_medications))

# Calculate sum of number of medication by probBin using aggregate function
sum_med <- aggregate(num_medications ~ probsBin, finaltable_test6, sum)

# Create the bar plot
ggplot(sum_med, aes(x = probsBin, y = num_medications)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  geom_text(aes(label = num_medications), vjust = -0.5) +
  labs(x = "Probability to be readmitted", y = "Number of medication") +
  ggtitle("Number of medication by Probability to be readmitted") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

#From the patients with higher probability of being readmited, how many diagnoses they have

#Eliminate columns
finaltable_test7 <- subset(finaltable_test, select = c("probs", "number_diagnoses"))

#Create bins of probs column
finaltable_test7$probsBin <- cut(finaltable_test7$probs, 
                                 breaks = c(0.69, 0.75, 0.80, 0.85, 0.90, Inf), 
                                 labels = c("69%-75%", "75%-80%", "80%-85%", "90%-95%", ">95%"))


# Group by probsBin and calculate sum of number of diagnoses
finaltable_test7 <- finaltable_test7 %>%
  group_by(probsBin) %>%
  mutate(sum3 = sum(number_diagnoses))

# Calculate sum of number of diagnoses by probBin using aggregate function
sum_dig <- aggregate(number_diagnoses ~ probsBin, finaltable_test7, sum)

# Create the bar plot
ggplot(sum_dig, aes(x = probsBin, y = number_diagnoses)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  geom_text(aes(label = number_diagnoses), vjust = -0.5) +
  labs(x = "Probability to be readmitted", y = "Number of diagnoses") +
  ggtitle("Number of diagnoses by Probability to be readmitted") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

#Diabetes type 2 is the most dangerous type of diabetes, which means they have too much sugar in the blood
# These types of people depend on insulin which causes more costs
#1st check from the probabilities which bin is more likely to take medicines which indicates they are type 2
#2nd check from the probabilities which bin is more likely to have a higher average of blood sugar - A1 Cresult
#3rd check the quantity of insulin they need to have - glucose serum

#Probability bin that are more likely to take medicines 

#Eliminate columns
finaltable_test8 <- subset(finaltable_test, select = c("probs", "diabetesMed"))

#Create bins of probs column
finaltable_test8$probsBin <- cut(finaltable_test8$probs, 
                                 breaks = c(0.69, 0.75, 0.80, 0.85, 0.90, Inf), 
                                 labels = c("69%-75%", "75%-80%", "80%-85%", "90%-95%", ">95%"))

# Convert diabetesMed column to 1/0 bins
finaltable_test8$diabetesMedbin <- ifelse(finaltable_test8$diabetesMed == "Yes", 1, 0)

#Group by probsBins and count the number of "Yes" and "No" of taking medication and make a bar plot 
finaltable_test8 %>% 
  group_by(probsBin) %>% 
  count(diabetesMedbin) %>% 
  mutate(diabetesMedbin = factor(diabetesMedbin, labels = c("No diabetes medication", "Yes diabetes medication"))) %>% 
  ggplot(aes(x = probsBin, y = n, fill = diabetesMedbin)) +
  geom_bar(position = "fill", stat = "identity") +
  labs(x = "Probability Bin", y = "Proportion", fill = "Diabetes Medication") +
  ggtitle("Proportion of Patients by Probability Range and Diabetes Medication") +
  scale_fill_manual(values = c("lightblue", "lightpink")) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#Probability bin that are more likely to have a higher average of blood sugar - A1C result

#Eliminate columns
finaltable_test9 <- subset(finaltable_test, select = c("probs", "A1Cresult"))

#Create bins of probs column
finaltable_test9$probsBin <- cut(finaltable_test9$probs, 
                                 breaks = c(0.69, 0.75, 0.80, 0.85, 0.90, Inf), 
                                 labels = c("69%-75%", "75%-80%", "80%-85%", "90%-95%", ">95%"))

#Create bins for A1Cresult column
finaltable_test9 <- finaltable_test9 %>%
  mutate(A1CBins = case_when(
    A1Cresult == "None" ~ 1,
    A1Cresult == ">7" ~ 2,
    A1Cresult == ">8" ~ 3,
    TRUE ~ NA_integer_
  ))

#Group by probsBins and count the number of "None", ">7" and ">8" of A1Cresult and make a bar plot 
finaltable_test9 %>%
  group_by(probsBin) %>%
  count(A1CBins) %>%
  ggplot(aes(x = probsBin, y = n, fill = factor(A1CBins))) +
  geom_bar(position = "fill", stat = "identity") +
  labs(x = "Probability Bin", y = "Proportion", fill = "A1C Result") +
  ggtitle("Proportion of Patients by Probability Range and A1C Results") + 
  scale_fill_manual(values = c("lightblue", "#FFA07A","lightpink" ),
                    labels = c("None", ">7", ">8")) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#Probability bin that are more likely to need insulin - glucose serum

#Eliminate columns
finaltable_test10 <- subset(finaltable_test, select = c("probs", "max_glu_serum"))

#Create bins of probs column
finaltable_test10$probsBin <- cut(finaltable_test10$probs, 
                                 breaks = c(0.69, 0.75, 0.80, 0.85, 0.90, Inf), 
                                 labels = c("69%-75%", "75%-80%", "80%-85%", "90%-95%", ">95%"))

#Create bins for max_glu_serum column
finaltable_test10 <- finaltable_test10 %>%
  mutate(gluserumBins = case_when(
    max_glu_serum == "None" ~ 1,
    max_glu_serum == "Norm" ~ 2,
    max_glu_serum == ">200" ~ 3,
    max_glu_serum == ">300" ~ 4,
    TRUE ~ NA_integer_
  ))

#Group by probsBins and count the number of "None", "Norm", ">200" and ">300" of glucose serum and make a bar plot 
finaltable_test10 %>%
  group_by(probsBin) %>%
  count(gluserumBins) %>%
  ggplot(aes(x = probsBin, y = n, fill = factor(gluserumBins))) +
  geom_bar(position = "fill", stat = "identity") +
  labs(x = "Probability Bin", y = "Proportion", fill = "Glucose Serum") +
  ggtitle("Proportion of Patients by Probability Range and Glusose Serum") + 
  scale_fill_manual(values = c("lightblue", "#FFA07A","#D3D3D3", "lightpink"),
                    labels = c("None", "Norm", ">200", ">300")) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#Probability bin that are more likely to be discharged home

#Eliminate columns
finaltable_test11 <- subset(finaltable_test, select = c("probs", "discharge_disposition_id"))

#Create bins of probs column
finaltable_test11$probsBin <- cut(finaltable_test11$probs, 
                                  breaks = c(0.69, 0.75, 0.80, 0.85, 0.90, Inf), 
                                  labels = c("69%-75%", "75%-80%", "80%-85%", "90%-95%", ">95%"))

#Only the patients that were discharged home
finaltable_test11 <- subset(finaltable_test11, 
                            discharge_disposition_id == "Discharged to home")
finaltable_test11

# Group by probsBin and discharge_disposition_id, and count the number of cases
finaltable_test11 <- finaltable_test11 %>%
  group_by(probsBin, discharge_disposition_id) %>%
  summarise(n = n()) %>%
  ungroup()

# Create a bar plot
ggplot(finaltable_test11, aes(x = probsBin, y = n, fill = discharge_disposition_id)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("lightblue")) +
  labs(x = "ProbsBin", y = "Patients", fill = "Discharge Disposition") +
  ggtitle("Number of Patients by Probability Range and Discharge Disposition") + 
  theme_minimal() +
  geom_text(aes(label = n), position = position_dodge(width = 0.9), vjust = -0.5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


