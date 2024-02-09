################################################################################
# KAGGLE COMPETITION
# Titanic - Machine Learning from Disaster
# Competition URL: https://www.kaggle.com/competitions/titanic
# Made with ♥︎ by Alberto Frison
# Created on February 2023
################################################################################

################################################################################
# Data
# Download Data from Competition URL and store it into a Data folder into your R project
# Study the train.csv file passengers data and make a ML predicting model to predict which passenger will survive in the test.csv file
################################################################################

################################################################################
# Clean the environment and Load Libraries
rm (list = ls())
# install.packages('randomForest')
library(randomForest)

################################################################################
# Access and Clean the Training Data Files 
titanic_train <- read.csv (file ="Data/train.csv", stringsAsFactors = FALSE, header = TRUE) # keep strings as strings
head (titanic_train) # always check the good read.csv execution
tail (titanic_train) # always check the good read.csv execution

# Access the Test Data Files
titanic_test <- read.csv (file ="Data/test.csv", stringsAsFactors = FALSE, header = TRUE) # keep strings as strings
str (titanic_train) 
str (titanic_test) # no Survived info

median(titanic_train$Age, na.rm = TRUE) #28 - na.rm to remove missing values
median(titanic_test$Age, na.rm = TRUE) #27 - na.rm to remove missing values

# we will combine the two datasets to clean them together, in order to keep track which entry is which (training or test set) we add a column to store if a row was originally coming from the training or the test set
titanic_train$IsTrainingSet <- TRUE
titanic_test$IsTrainingSet <- FALSE

# in order to combine the datasets we need to make sure that the structure (names and number cols of the header must match)
ncol(titanic_train) # 13 columns
ncol(titanic_test) # 12 columns

# so let's check how the headers of each of the datasets are called
names (titanic_train)
names (titanic_test) # Survived is missing in the test dataset...
titanic_test$Survived <- NA # ... and we add it so we can then bind it to the training set

# we are about to merge / bind the two data sets, let's just see how many rows there are
nrow(titanic_train) # 891
nrow(titanic_test) # 418

titanic_full <- rbind (titanic_train, titanic_test) # SQL union - a vertical Join of the two data sets
nrow(titanic_full) # 1309 
891 + 418 - 1309 # 0 so the math checks out

table (titanic_full$IsTrainingSet) # let's check if the combination went well

################################################################################
# Cleaning the Missing Values
table (titanic_full$Pclass) # Pclass is all compiled
table (titanic_full$Sex) # Sex is all compiled
table (titanic_full$SibSp) # Sibling Spouses aboard is all compiled
table (titanic_full$Parch) # Parent and Children aboard is all compiled

# EMBARKED
table (titanic_full$Embarked) # we have no boarding port for two people... so we substitute the missing values of the missing port with the most common port, the mode
titanic_full[titanic_full$Embarked == '', "Embarked"] <-'S' # replace with the MODE the two missing values
table (titanic_full$Embarked) # check that the replacement was ok

# AGE
table(is.na(titanic_full$Age)) # we substitute for now the missing value of Age with the median
titanic_full[is.na(titanic_full$Age), "Age"] <- median(titanic_full$Age, na.rm =TRUE) 
table(is.na(titanic_full$Age)) # now all passengers have an age

# FARE
table(is.na(titanic_full$Fare)) # one passenger has no Fare value
titanic_full[is.na(titanic_full$Fare), "Fare"] <- median(titanic_full$Fare, na.rm =TRUE) 
table(is.na(titanic_full$Fare))


# CAST STRINGS INTO CATEGORIES
titanic_full$Pclass <- as.factor(titanic_full$Pclass) 
titanic_full$Sex <- as.factor(titanic_full$Sex) 
titanic_full$Embarked <- as.factor(titanic_full$Embarked) 
str(titanic_full)


################################################################################
# now that we have cleaned the entire dataset, we can split back the two data sets into train and test
titanic_train <- titanic_full[titanic_full$IsTrainingSet == TRUE,]
titanic_train$Survived <- as.factor(titanic_train$Survived) 

nrow(titanic_train)
str (titanic_train)

titanic_test <- titanic_full[titanic_full$IsTrainingSet == FALSE,]
nrow(titanic_test)
str (titanic_test)


################################################################################
# BUILDING A PREDICTIVE MODEL USING RANDOM FOREST

# we will use random forest
# we will make a predictive model of SURVIVED using the Passenger Class, the Sex, the Age, Sibling and Spouse in the titanic as well as Parents and Children, and the port of Embarcation
# as formula will build the relations
survive_formula <- as.formula("Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked") 

# random forest prediction on titanic_train based on the survive_formula
# we are skipping for now 70/30 split
# we are skipping cross validation
# we used just the median to predict the missing value of Age, Fare, Embarked
# ntree - 500 (defeult)
# mtry - sqrt (7)
# node size - minimum sample per node 1% of the size of the training set
titanic_model <- randomForest (formula = survive_formula, data = titanic_train, ntree = 500, mtry = 3, nodesize = 0.01 * nrow (titanic_test))

# so now lets predict
features_equiation <- "Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
Survived <- predict(titanic_model, newdata = titanic_test) # Survived is the name of the colunm 

PassengerId <- titanic_test$PassengerId
output.df <- as.data.frame(PassengerId)
output.df$Survived <- Survived

write.csv (output.df, "Data/Kaggle_Submission.csv", row.names = FALSE)