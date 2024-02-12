################################################################################
# KAGGLE COMPETITION
# Titanic - Machine Learning from Disaster
# Competition URL: https://www.kaggle.com/competitions/titanic
# Source video from Data Science Dojo - www.youtube.com/@Datasciencedojo and David Langer on YouTube
# Made with ♥︎ by Alberto Frison
# Created on February 2023
################################################################################

################################################################################
# Data
# Download Data from Competition URL and store it into a Data folder into your R project
# Study the train.csv file passengers data and make a ML predicting model to predict which passenger will survive in the test.csv file
################################################################################

################################################################################
# TO DO LIST
# 01. Review the entire exercise with EDA (not yet done)
# 02. Add more features (Titles, Decks, Fare $, Family Size, ...) and combinations 
# 03. Improve the randomforest with cross validation (and 70/30)
# 04. Watch Videos on https://www.youtube.com/@ProfNandoDF/search?query=random%20forest
################################################################################




################################################################################
# install.packages('randomForest')
# install.packages ('tidyverse')
# install.packages ('caret')
# install.packages ('doSNOW')
library(tidyverse)
library(randomForest)
library (caret)
library (doSNOW)

# Clean the environment and Load Libraries
rm (list = ls())



################################################################################
# Access and Clean the Training Data Files 
################################################################################

titanic_train <- read.csv (file ="Data/train.csv", stringsAsFactors = FALSE, header = TRUE) # keep strings as strings
head (titanic_train) # always check the good read.csv execution
tail (titanic_train) # always check the good read.csv execution

# Access the Test Data Files
titanic_test <- read.csv (file ="Data/test.csv", stringsAsFactors = FALSE, header = TRUE) # keep strings as strings
str (titanic_train) 
str (titanic_test) # no Survived info

###############################################################################
# Combination of the two datasets for mutual cleaning
################################################################################

# we will combine the two datasets to clean them together, in order to keep track which entry is which (training or test set) we add a column to store if a row was originally coming from the training or the test set
# titanic_train$IsTrainingSet <- TRUE
# titanic_test$IsTrainingSet <- FALSE

# in order to combine the datasets we need to make sure that the structure (names and number cols of the header must match)
ncol(titanic_train) # 12 columns
ncol(titanic_test) # 11 columns

# so let's check how the headers of each of the datasets are called
names (titanic_train)
names (titanic_test) # Survived is missing in the test dataset...
titanic_test$Survived <- NA # ... and we add it so we can then bind it to the training set

# we are about to merge / bind the two data sets, let's just see how many rows there are
nrow(titanic_train) # 891
nrow(titanic_test) # 418

titanic_train$Survived <- as.factor(titanic_train$Survived)
titanic_test$Survived <- as.factor(titanic_test$Survived)

titanic_full <- rbind (titanic_train, titanic_test) # SQL union - a vertical Join of the two data sets
nrow(titanic_full) # 1309 --- 891 + 418 - 1309 # 0 so the math checks out


################################################################################
# Section - Basic Data Analysis - 01
################################################################################

# Survival Rates per Class -  Hypotheses richer classes had higher survival rate
titanic_full[1:891,] %>%
  ggplot(aes(x = as.factor(Pclass), fill = factor(Survived))) +
  geom_histogram(width = 0.5, stat = "count") +
  xlab ("Pclass") +
  ylab ("Total Count") +
  labs (fill = "Survived")

# Survival Rates per Sex -  Hypotheses women had higher survival rate
titanic_full[1:891,] %>%
  ggplot(aes(x = as.factor(Sex), fill = factor(Survived))) +
  geom_histogram(stat = "count") +
  xlab ("Sex") +
  ylab ("Total Count") +
  labs (fill = "Survived")


# Survival Rates per Parents and Children 
titanic_full[1:891,] %>%
  ggplot(aes(x = as.factor(Parch), fill = factor(Survived))) +
  geom_histogram(width = 0.5, stat = "count") +
  xlab ("Parch") +
  ylab ("Total Count") +
  labs (fill = "Survived")


# Distribution per Age
hist(x = as.numeric(titanic_full[1:891,]$Age))


################################################################################
# FEATURE ENGENEERING
################################################################################
# TITLES
# Function to extract Titles from Names

extract_Title <- function (name) {
  name <- as.character(name)
  
  if (length (grep ("Miss.", name)) > 0 ) {
    return ("Miss.")
  } else if (length (grep ("Master.", name)) > 0 ) {
    return ("Master.")
  } else if (length (grep ("Mrs.", name)) > 0 ) {
    return ("Mrs.")
  } else if (length (grep ("Miss.", name)) > 0 ) {
    return ("Miss.")
  } else if (length (grep ("Mlle.", name)) > 0 ) {
    return ("Miss.")
  } else if (length (grep ("Mme.", name)) > 0 ) {
    return ("Mrs.")
  } else if (length (grep ("Ms.", name)) > 0 ) {
    return ("Mrs.")
  } else if (length (grep ("Mr.", name)) > 0 ) {
    return ("Mr.")
  } else if (length (grep ("Dr.", name)) > 0 ) {
    return ("Dr.")
  } else if (length (grep ("Rev.", name)) > 0 ) {
    return ("Priest.")
  } else if (length (grep ("Don.", name)) > 0 ) {
    return ("Priest.")
  } else if (length (grep ("Dona.", name)) > 0 ) {
    return ("Mrs.")
  }  else if (length (grep ("Col.", name)) > 0 ) {
    return ("Military.")
  } else if (length (grep ("Capt.", name)) > 0 ) {
    return ("Military.")
  } else if (length (grep ("Major.", name)) > 0 ) {
    return ("Military.")
  } else return ("Other.")
}
################################################################################


# TITANIC FULL
# TITLES
titles <- NULL
for (i in 1:nrow (titanic_full)) {
  titles <- c (titles, extract_Title(titanic_full[i,"Name"])) 
}
titanic_full$Title <- as.factor (titles) 

# FAMILY SIZE
titanic_full$FamilySize <- as.factor(as.numeric(titanic_full$SibSp) + as.numeric(titanic_full$Parch) + 1)

# check results
head(titanic_full)
tail(titanic_full)


################################################################################
# Section - Basic Data Analysis - 02
################################################################################

# Survival Rates per Title, split by Class
titanic_full[1:891,] %>%
  ggplot(aes(x = as.factor(Title), fill = factor(Survived))) +
 # geom_bar(binwidth = 0.5, stat = "count") +
  geom_bar(stat = "count") +
  facet_wrap(~ Pclass) +
  xlab ("Title") +
  ylab ("Total Count") +
  labs (fill = "Survived")

# Survival Rates per Class, split by Title
titanic_full[1:891,] %>%
  ggplot(aes(x = as.factor(Pclass), fill = factor(Survived))) +
  geom_bar(binwidth = 0.5, stat = "count") +
  facet_wrap(~ Title) +
  xlab ("Pclass") +
  ylab ("Total Count") +
  labs (fill = "Survived")

# Survival Rates per Sex, split by Class
titanic_full[1:891,] %>%
  ggplot(aes(x = as.factor(Sex), fill = factor(Survived))) +
  geom_bar(binwidth = 0.5, stat = "count") +
  facet_wrap(~ Pclass) +
  xlab ("Sex") +
  ylab ("Total Count") +
  labs (fill = "Survived")
#Note: it is counter intuitive to see that males in second class are worse of than males in first and third class

# Survival Rates per Age, split by Class, Sex
summary (titanic_full[1:891,]$Age)

titanic_full[1:891,] %>%
  ggplot(aes(x = Age, fill = Survived)) +
  geom_histogram(binwidth = 10) +
  facet_wrap(~ Sex + Pclass) +
  xlab ("Age") +
  ylab ("Total Count") +
  labs (fill = "Survived")
#Note: women and children first seems correct, attention that pattern always get worse in 3rd class

# Let's see if Master. is a good proxy of the age of a boy
titanic_full[1:891,] %>%
  filter  (Title =="Master.") %>%
  select(Age) %>%
    summary()
# IT IS 

# Let's see if Miss. is a good proxy of the age of a woman
titanic_full[1:891,] %>%
  filter  (Title =="Miss.") %>%
  select(Age) %>%
  summary()
# IT IS NOT

# let' see visually how Misses do in terms of survival
titanic_full[1:891,] %>%
  filter  (Title == "Miss.") %>%
    filter ((SibSp == 0) & (Parch == 0)) %>% # travelling alone - also change the ggtitle here below
      ggplot(aes(x = Age, fill = as.factor(Survived))) +
      geom_histogram(binwidth = 5) +
      facet_wrap(~ Pclass) +
      # ggtitle ("Survival Rates for Miss. by Age and Pclass") +
      ggtitle ("Survival Rates for Miss. by Age and Pclass (TRAVELLING ALONE)") +
      xlab("Age")
      ylab ("Total Count") +
      ggtitle ("Age for Miss. by Pclass") +
      labs (fill = "Survived")
# Note: Misses are females across all ages, in Class 1 and 2 normally survive, in 3rd class it is more of a 50:50 except for the older (i.e. 30ish)
# Note: also note that there are much more females in 3rd class so we need to be attentive on how we predict this class
    
titanic_full[1:891,] %>%
  filter  (Title =="Master.") %>%
    ggplot(aes(x = as.factor (Title), fill = Survived)) +
    geom_bar(binwidth = 0.5, stat = "count") +
    facet_wrap(~ Pclass) +
    xlab ("Title") +
    ylab ("Total Count") +
    labs (fill = "Survived")


# SIBSP
# Survival Rates per Sibling & Spouses Aboard
titanic_full[1:891,] %>%
  select (SibSp) %>%
  summary()
# it could be treated as factor then

titanic_full[1:891,] %>%
  ggplot(aes(x = as.factor(SibSp), fill = factor(Survived))) +
  geom_histogram(width = 1.0, stat = "count") +
  facet_wrap(~ Pclass) +
  xlab ("Sibsp") +
  ylab ("Total Count") +
  labs (fill = "Survived")
# looks like that the larger the family in terms of Siblings or Spouses, the worse


# PARCH
# Survival Rates per Parents and Children Aboard
titanic_full[1:891,] %>%
  select (Parch) %>%
  summary()
# it could be treated as factor then

titanic_full[1:891,] %>%
  ggplot(aes(x = as.factor(Parch), fill = factor(Survived))) +
  geom_histogram(width = 1.0, stat = "count") +
  facet_wrap(~ Pclass) +
  xlab ("Parch") +
  ylab ("Total Count") +
  labs (fill = "Survived")
# looks like that the larger the family in terms of Parent and Children, the worse

titanic_full[1:891,] %>%
  ggplot(aes(x = as.factor(FamilySize), fill = factor(Survived))) +
  geom_histogram(width = 1.0, stat = "count") +
  facet_wrap(~ Sex + Pclass) +
  xlab ("FamilySize") +
  ylab ("Total Count") +
  labs (fill = "Survived")
# looks like that the larger the family in terms, the worse


################################################################################
# TICKET
str(titanic_full[1:891,]$Ticket)

# let's look at the first digit of the TICKET - we added to the training set and see how it describes the data
titanic_full$ticket_first_char <- ifelse (titanic_full$Ticket == "", " ", substr (titanic_full$Ticket, 1,1))

titanic_full[1:891,] %>%
  ggplot(aes(x = as.factor(ticket_first_char), fill = factor(Survived))) +
  geom_histogram(width = 1.0, stat = "count") +
  facet_wrap(~ Title) + # loop variables here (Pclass, Embarked), to see if the ticket is correlated with some other variable
  xlab ("Ticket (firts letter)") +
  ylab ("Total Count") +
  labs (fill = "Survived")
# there is not much signal in the Ticket - so probably we will not use it into the model
# occam razor tells us that simpler things (i.e. logistic regression) are better than more complicated models


################################################################################
# FARES

summary (titanic_full[1:891,]$Fare)
# some people did not pay any fare at all
# 50% of the people paid less than £14.something, but the mean is double the median, suggesting that data is really skewed to the right

titanic_full[1:891,] %>%
  ggplot(aes(x = Fare, fill = factor(Survived))) +
  geom_histogram(width = 5.0) +
  facet_wrap(~ Embarked) + # loop variables here (Pclass, Embarked), to see if the ticket is correlated with some other variable
  xlab ("Fare") +
  ylab ("Total Count") +
  labs (fill = "Survived")
# it does not look like that the Fare does have lots of prective value


################################################################################
# CABIN

summary (titanic_full[1:891,]$Cabin)
str (titanic_full[1:891,]$Cabin)

titanic_full$Cabin <- as.character(titanic_full$Cabin)
titanic_full[1:891,]$Cabin # looks that some people have multiple cabins, also the first letter can tell the deck?

# replace the missing cabin with "U" for unknown
titanic_full[which (titanic_full$Cabin == ""), "Cabin"] <- "U"
unique (titanic_full$Cabin)

titanic_full$Cabin_First_Char <- as.factor (substr(titanic_full$Cabin,1,1))
str(titanic_full$Cabin_First_Char)
levels (titanic_full$Cabin_First_Char)

titanic_full[1:891,] %>%
  ggplot(aes(x = as.factor(Cabin_First_Char), fill = factor(Survived))) +
  geom_bar() +
  facet_wrap(~ Embarked) + # loop variables here (Pclass, Embarked), to see if the ticket is correlated with some other variable
  xlab ("Cabin (First Char)") +
  ylab ("Total Count") +
  labs (fill = "Survived")

# A-E looks like first Class
# E-U look like third Class
# Note: Pclass is already very predictive of the Survival

################################################################################
# EMBARKED
# Survival Rates per Port of Embarkation 
titanic_full[1:891,] %>%
  ggplot(aes(x = as.factor(Embarked), fill = factor(Survived))) +
  geom_histogram(stat = "count") +
  facet_wrap(~ Pclass) +
  xlab ("Embarked") +
  ylab ("Total Count") +
  labs (fill = "Survived")

################################################################################
# RANDOM FOREST TRAINING
################################################################################
# 01
# just with PClass and Title

rf.train.01 <- titanic_full[1:891, c("Pclass", "Title")]
rf.label <- as.factor (titanic_full[1:891,]$Survived)

set.seed (1234)
rf.01 <- randomForest (x = rf.train.01, y = rf.label, importance = TRUE, ntree = 1000)
rf.01
varImpPlot(rf.01)

##
# OOB:
# I will do a 1000 times and randomly select a bunch of rows and columns each time (sampling with replacement), some rows and column will never be selected, those will be left out will be used to test the accuracy of the model
# On average I trained 1000 trees and we are getting a (1 - 20.88%) Accuracy
# Confusion Matrix:
# True Value vs Predicted Values
# 534 were predicted to Die (and they died) - OK
# 15 were predicted to Die (and they lived) - ERROR
# 171 were predicted to Live (and they died) - ERROR
# 171 were predicted to Live (and they lived) - OK

# Comment:
# when people perish , the model is 98% correct (1- 0.027)
# when poeple live, it is more of a 50:50
# it makes sens as the distribution of the people is skewed as more are dead
table (rf.label)

# the far right the dot is in the next chart, the more important.
varImpPlot(rf.01)
# so Title is very much a predictive feature!


################################################################################
# 02
# with PClass and Title and SibSp

rf.train.02 <- titanic_full[1:891, c("Pclass", "Title", "SibSp")]
rf.label <- as.factor (titanic_full[1:891,]$Survived)

set.seed (1234)
rf.02 <- randomForest (x = rf.train.02, y = rf.label, importance = TRUE, ntree = 1000)
rf.02
varImpPlot(rf.02)

# Error Rate is decreased by
(0.2088 - 0.1908)*100

# we reduced our error rate in predicting the living
(0.0273224 - 0.1056466)*100
# at the cost of increasing our error rate in predicting the dead
(0.5000000 - 0.3274854)*100
# Attention that in any application error type 1 and 2 have a different imporance, for example telling somebody that is sick while he is not or that is fine while he is sick

################################################################################
# 03
# with PClass and Title and Parch

rf.train.03 <- titanic_full[1:891, c("Pclass", "Title", "Parch")]
rf.label <- as.factor (titanic_full[1:891,]$Survived)

set.seed (1234)
rf.03 <- randomForest (x = rf.train.03, y = rf.label, importance = TRUE, ntree = 1000)
rf.03
varImpPlot(rf.03)
# small improvement from the 01 model, Parch looks like less predictive than SibSp

################################################################################
# 04
# with PClass and Title plus combination of SibSp and Parch

rf.train.04 <- titanic_full[1:891, c("Pclass", "Title", "SibSp", "Parch")]
rf.label <- as.factor (titanic_full[1:891,]$Survived)

set.seed (1234)
rf.04 <- randomForest (x = rf.train.04, y = rf.label, importance = TRUE, ntree = 1000)
rf.01
rf.04
varImpPlot(rf.04)
# Error rate decreased from 20.88% to 18.41%

################################################################################
# 05
# with PClass and Title plus the Family Size

rf.train.05 <- titanic_full[1:891, c("Pclass", "Title", "FamilySize")]
# remember to use FamilySize as Factor

str(rf.train.05)
rf.label <- as.factor (titanic_full[1:891,]$Survived)

set.seed (1234)
rf.05 <- randomForest (x = rf.train.05, y = rf.label, importance = TRUE, ntree = 1000)
rf.01
rf.05
varImpPlot(rf.05)
# OOB estimate gets us the lowest error rate 17.96%
# FamilySize matters!


################################################################################
# 06
# with PClass, Title plus the Family Size and SibSp

rf.train.06 <- titanic_full[1:891, c("Pclass", "Title", "SibSp","FamilySize")]
rf.label <- as.factor (titanic_full[1:891]$Survived)

set.seed (1234)
rf.06 <- randomForest (x = rf.train.06, y = rf.label, importance = TRUE, ntree = 1000)
rf.05
rf.06
varImpPlot(rf.06)
# adding SibSp to FamilySize the situation get worse

################################################################################
# 07
# with PClass, Title plus the Family Size and Parch

rf.train.07 <- titanic_full[1:891, c("Pclass", "Title", "Parch","FamilySize")]
rf.label <- as.factor (titanic_full[1:891,]$Survived)

set.seed (1234)
rf.07 <- randomForest (x = rf.train.07, y = rf.label, importance = TRUE, ntree = 1000)
rf.06
rf.07
varImpPlot(rf.07)
# adding Parch to FamilySize the situation get worse from SibSp

################################################################################
# WRAP UP
rf.05 # is the best model so far in terms of accuracy
1-0.1796 # 82.04% accuracy (in the training set)




################################################################################
# CROSS VALIDATION
################################################################################
# Cross Validation is a way to do things: 1) maximise the value of the (limited) training data and 2) improve the performance of the prediction with unseen data
# Random Forrest is cool because it gives you the estimate of the error rate


# Make subset of Test Dataset to match with the requirements of rf.05
titanic_test_sub <- titanic_full[892:1309, c("Pclass", "Title", "FamilySize")]

# Make a Prediction
rf.05.prediction <- predict(rf.05, titanic_test_sub)

# Store into a df
output.df <- data.frame(PassengerId = rep(892:1309), Survived = rf.05.prediction)

# Store into a csv for saving and upload into Kaggle (remeber to version it)
write.csv (output.df, "Data/Kaggle_Submission_V16.csv", row.names = FALSE)

# ==> Submit to Kagle https://www.kaggle.com/competitions/titanic/submissions


# Back test of the submission
rf.05
1-0.1796
# back test the overall accuracy in kaggle is 0.7799 (on the test set) vs 0.8204 (on the training set)


################################################################################
# entering CARET
################################################################################

# Research has shown that 10-fold Cross Validation repeated 10 times is the best place to start


set.seed (2348)
cv.10.folds <- createMultiFolds()


################################################################################
# Here the approach from Data Science Dojo and David Langer differs
################################################################################
# Cleaning the Missing Values
table (titanic_full$Pclass) # Pclass is all compiled
table (titanic_full$Sex) # Sex is all compiled
table (titanic_full$SibSp) # Sibling Spouses aboard is all compiled
table (titanic_full$Parch) # Parent and Children aboard is all compiled
table (titanic_full$Title) # Title association function as worked well


# EMBARKED
table (titanic_full$Embarked) # we have no boarding port for two people... so we substitute the missing values of the missing port with the most common port, the mode
titanic_full[titanic_full$Embarked == '', "Embarked"] <-'S' # replace with the MODE the two missing values
table (titanic_full$Embarked) # check that the replacement was ok

################################################################################
# AGE - basic option 
# table(is.na(titanic_full$Age)) # we substitute for now the missing value of Age with the median
# titanic_full[is.na(titanic_full$Age), "Age"] <- median(titanic_full$Age, na.rm =TRUE) 
# table(is.na(titanic_full$Age)) # now all passengers have an age

# AGE - Advanced Option, we fit a lm model to predict missing Fare values 
titanic_full[is.na(titanic_full$Age), "Age"] # 263 missing values in Age in the set
nrow(titanic_full[is.na(titanic_full$Age),]) # 263 indeed

# there are two types of lm: the online gradient descent variant (not covered) and the least squares linear regression model (which is affected by outliers)
# before we apply this we have to filter out the 

boxplot (titanic_full$Age) # see that Age is affected by many outliers, R stores these values in "$stats vector
boxplot.stats(titanic_full$Age)
boxplot.stats(titanic_full$Age)$stats [1] # the first whisker
boxplot.stats(titanic_full$Age)$stats [2] # the first quartile
boxplot.stats(titanic_full$Age)$stats [3] # the median
boxplot.stats(titanic_full$Age)$stats [4] # the third quartile
boxplot.stats(titanic_full$Age)$stats [5] # the last whisker - anything above 66$ is an outlier to us

# so we filter out these items in a dynamical way
upper.whisker <- boxplot.stats(titanic_full$Age)$stats[5]
outlier.filter <- titanic_full$Age <= upper.whisker # this will work as an index

# titanic_full[outlier.filter,] # current filter of the non outliers Fare entries

# creation of the lm model
str(titanic_full)
age.equation = "Age ~ Pclass + Sex + Fare + SibSp + Parch + Embarked + Title" # we do not use Survive, as our test data will not have it...

age.model <- lm (
  formula = age.equation,
  data = titanic_full[outlier.filter,]
)


# Prediction section, but not every column is needed to predict.
age.row <- titanic_full[is.na(titanic_full$Age),c("Pclass", "Sex", "Fare", "SibSp", "Parch", "Embarked", "Title")] # query all the rows that have no Fares and give me just the columns I need
age.prediction <- predict (age.model, newdata = age.row)

titanic_full[is.na(titanic_full$Age), "Age"] <- age.prediction # let's substitute the missing Fare value with the predicted value
titanic_full$Age # check






################################################################################
# FARE - Basic Option, substitute the missing values of Fare with the median
# table(is.na(titanic_full$Fare)) # one passenger has no Fare value
# titanic_full[is.na(titanic_full$Fare), "Fare"] <- median(titanic_full$Fare, na.rm =TRUE) 
# table(is.na(titanic_full$Fare))

# FARE - Advanced Option, we fit a lm model to predict missing Fare values 
titanic_full[is.na(titanic_full$Fare), "Fare"] # just one missing Fare in the entire dataset

boxplot (titanic_full$Fare) # see that Fare is affected by many outliers, R stores these values in "$stats vector
boxplot.stats(titanic_full$Fare)
boxplot.stats(titanic_full$Fare)$stats [1] # the first whisker
boxplot.stats(titanic_full$Fare)$stats [2] # the first quartile
boxplot.stats(titanic_full$Fare)$stats [3] # the median
boxplot.stats(titanic_full$Fare)$stats [4] # the third quartile
boxplot.stats(titanic_full$Fare)$stats [5] # the last whisker - anything above 65$ is an outlier to us

# so we filter out these items in a dynamical way
upper.whisker <- boxplot.stats(titanic_full$Fare)$stats[5]
outlier.filter <- titanic_full$Fare <= upper.whisker

# titanic_full[outlier.filter,] # current filter of the non outliers Fare entries

# creation of the lm model
str(titanic_full)
fare.equation = "Fare ~ Pclass + Sex + Age + SibSp + Parch + Embarked + Title" # we do not use Survive, as our test data will not have it...

fare.model <- lm (
  formula = fare.equation,
  data = titanic_full[outlier.filter,]
)


# Prediction section, but not every column is needed to predict.
fare.row <- titanic_full[is.na(titanic_full$Fare),c("Pclass", "Sex", "Age", "SibSp", "Parch", "Embarked", "Title")] # query all the rows that have no Fares and give me just the columns I need
fare.prediction <- predict (fare.model, newdata = fare.row)

titanic_full[is.na(titanic_full$Fare), "Fare"] <- fare.prediction # let's substitute the missing Fare value with the predicted value
titanic_full[1044,] # check


# FARE PER PERSON
titanic_full$FarePerPerson <- as.numeric(titanic_full$Fare) / as.numeric(titanic_full$FamilySize)

################################################################################
# CAST STRINGS INTO CATEGORIES
titanic_full$Pclass <- as.factor(titanic_full$Pclass) 
titanic_full$Sex <- as.factor(titanic_full$Sex) 
titanic_full$Embarked <- as.factor(titanic_full$Embarked) 
titanic_full$Title <- as.factor(titanic_full$Title) 
# titanic_full$FamilySize <- as.numeric(titanic_full$FamilySize) 
str(titanic_full)


################################################################################
# now that we have cleaned the entire dataset, we can split back the two data sets into train and test
titanic_train <- titanic_full[titanic_full$IsTrainingSet == TRUE,]
titanic_train$Survived <- as.factor(titanic_train$Survived) 

nrow(titanic_train)
str (titanic_train)

titanic_test <- titanic_full[titanic_full$IsTrainingSet == FALSE,]
nrow(titanic_test)


################################################################################
# BUILDING A PREDICTIVE MODEL USING RANDOM FOREST

# we will use random forest
# we will make a predictive model of SURVIVED using the Passenger Class, the Sex, the Age, Sibling and Spouse in the titanic as well as Parents and Children, and the port of Embarcation
# as formula will build the relations
str (titanic_test)
survive_formula <- as.formula("Survived ~ Pclass + Sex + Age + SibSp + Parch + Title + FamilySize")

# random forest prediction on titanic_train based on the survive_formula
# (-) we are skipping for now 70/30 split
# (-) we are skipping cross validation
# (+) we no longer used the median to predict the missing value of Age, Fare, Embarked, BUT actually fitted an lm model
# (+) we added a first new Feature (Title, FamilySize)
# ntree - 500 (defeult)
# mtry - sqrt (7) 7 is the number of predictors
# node size - minimum sample per node 1% of the size of the training set
titanic_model <- randomForest (formula = survive_formula, data = titanic_train, ntree = 5000, mtry = 3, nodesize = 0.1 * nrow (titanic_test))

# so now lets predict
#features_equation <- "Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title" # review why this is not used
Survived <- predict(titanic_model, newdata = titanic_test) # Survived is the name of the column and it contains the result of the prediction

# let's create the data.frame to store the two columns, the PassengerId and the Survived (0/1) result
PassengerId <- titanic_test$PassengerId
output.df <- as.data.frame(PassengerId)
output.df$Survived <- Survived





################################################################################
################################################################################
################################################################################
# BACKUP

# ################################################################################
# # Research in names of Miss. Mr. Mrs. Master and other Titles
# # This whole section is DEPRECATED deprecated as we will use a FUNCTION to do this
# 
# titanic_train$Title <- NA
# 
# # Miss.
# misses <- which (str_detect(titanic_train$Name, "Miss."))
# titanic_train[misses,]$Title <- "Miss."
# 
# # Mr. Misters before Mrs otherwise it will overwrite the Mrs !!!
# mr <- which (str_detect(titanic_train$Name, "Mr."))
# titanic_train[mr,]$Title <- "Mr."
# 
# # Mrs.
# mrs <- which (str_detect(titanic_train$Name, "Mrs."))
# titanic_train[mrs,]$Title <- "Mrs."
# 
# # Master.
# masters <- which (str_detect(titanic_train$Name, "Master."))
# titanic_train[masters,]$Title <- "Master."
# 
# # Rev.
# rev <- which (str_detect(titanic_train$Name, "Rev."))
# titanic_train[rev,]$Title <- "Rev."
# 
# # Dr.
# dr <- which (str_detect(titanic_train$Name, "Dr."))
# titanic_train[dr,]$Title <- "Dr."
# 
# # Dme. to Mrs.
# mme <- which (str_detect(titanic_train$Name, "Mme."))
# titanic_train[mme,]$Title <- "Mrs."
# 
# # Ms. to Mrs.
# ms <- which (str_detect(titanic_train$Name, "Ms."))
# titanic_train[ms,]$Title <- "Mrs."
# 
# # Mlle. to Mrs.
# mlle <- which (str_detect(titanic_train$Name, "Mlle."))
# titanic_train[mlle,]$Title <- "Mrs."
# 
# # all Others to Others
# others <- which (is.na(titanic_train$Title))
# titanic_train[others,]$Title <- "Other."
# 
# 
# ################################################################################

