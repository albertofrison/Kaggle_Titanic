################################################################################
# KAGGLE COMPETITION
# Titanic - Machine Learning from Disaster
# Competition URL: https://www.kaggle.com/competitions/titanic
# Source video from Data Science Dojo - www.youtube.com/@Datasciencedojo
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
# Source: David Langer on YouTube
# 02. Add more features (Titles, Decks, Fare $, Family Size, ...) and combinations 
# 03. Improve the randomforest with cross validation (and 70/30)
################################################################################




################################################################################
# install.packages('randomForest')
# install.packages ('tidyverse')
library(tidyverse)
library(randomForest)

# Clean the environment and Load Libraries
rm (list = ls())



################################################################################
# Access and Clean the Training Data Files 
titanic_train <- read.csv (file ="Data/train.csv", stringsAsFactors = FALSE, header = TRUE) # keep strings as strings
head (titanic_train) # always check the good read.csv execution
tail (titanic_train) # always check the good read.csv execution

# Access the Test Data Files
titanic_test <- read.csv (file ="Data/test.csv", stringsAsFactors = FALSE, header = TRUE) # keep strings as strings
str (titanic_train) 
str (titanic_test) # no Survived info


################################################################################
################################################################################
# EDA Section

# Survival Rates per Class -  Hypotheses richer classes had higher survival rate
titanic_train %>%
  ggplot(aes(x = as.factor(Pclass), fill = factor(Survived))) +
  geom_histogram(width = 0.5, stat = "count") +
  xlab ("Pclass") +
  ylab ("Total Count") +
  labs (fill = "Survived")


# Survival Rates per Sex -  Hypotheses women had higher survival rate
titanic_train %>%
  ggplot(aes(x = as.factor(Sex), fill = factor(Survived))) +
  geom_histogram(stat = "count") +
  xlab ("Sex") +
  ylab ("Total Count") +
  labs (fill = "Survived")


# Survival Rates per Parents and Children 
titanic_train %>%
  ggplot(aes(x = as.factor(Parch), fill = factor(Survived))) +
  geom_histogram(width = 0.5, stat = "count") +
  xlab ("Parch") +
  ylab ("Total Count") +
  labs (fill = "Survived")


# Survival Rates per Port of Embarkation 
titanic_train %>%
  ggplot(aes(x = as.factor(Embarked), fill = factor(Survived))) +
  geom_histogram(width = 0.5, stat = "count") +
  xlab ("Embarked") +
  ylab ("Total Count") +
  labs (fill = "Survived")

# Distribution per Age
hist(x = as.numeric(titanic_train$Age))


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

################################################################################
# Feature Engineering

# TITLES
# PASSENGER TITLES
titles <- NULL
for (i in 1:nrow (titanic_train)) {
  titles <- c (titles, extract_Title(titanic_train[i,"Name"])) 
}
titanic_train$Title <- as.factor (titles) 
tail(titanic_train)

# FAMILY SIZE
titanic_train$FamilySize <- as.numeric(titanic_train$SibSp) + as.numeric(titanic_train$Parch) + 1
tail(titanic_train)


# Survival Rates per Title, split by Class
titanic_train %>%
  ggplot(aes(x = as.factor(Title), fill = factor(Survived))) +
  geom_bar(binwidth = 0.5, stat = "count") +
  facet_wrap(~ Pclass) +
  xlab ("Title") +
  ylab ("Total Count") +
  labs (fill = "Survived")

# Survival Rates per Class, split by Title
titanic_train %>%
  ggplot(aes(x = as.factor(Pclass), fill = factor(Survived))) +
  geom_bar(binwidth = 0.5, stat = "count") +
  facet_wrap(~ Title) +
  xlab ("Pclass") +
  ylab ("Total Count") +
  labs (fill = "Survived")

# Survival Rates per Sex, split by Class
titanic_train %>%
  ggplot(aes(x = as.factor(Sex), fill = factor(Survived))) +
  geom_bar(binwidth = 0.5, stat = "count") +
  facet_wrap(~ Pclass) +
  xlab ("Sex") +
  ylab ("Total Count") +
  labs (fill = "Survived")
#Note: it is counter intuitive to see that males in second class are worse of than males in first and third class

# Survival Rates per Age, split by Class, Sex
summary (titanic_test$Age)
titanic_train %>%
  ggplot(aes(x = Age, fill = Survived)) +
  geom_histogram(binwidth = 10) +
  facet_wrap(~ Sex + Pclass) +
  xlab ("Age") +
  ylab ("Total Count") +
  labs (fill = "Survived")
#Note: women and children first seems correct, attention that pattern always get worse in 3rd class


# Let's see if Master. is a good proxy of the age of a boy
titanic_train %>%
  filter  (Title =="Master.") %>%
  select(Age) %>%
    summary()
# IT IS 

# Let's see if Miss. is a good proxy of the age of a woman
titanic_train %>%
  filter  (Title =="Miss.") %>%
  select(Age) %>%
  summary()
# IT IS NOT

# let' see visually how Misses do in terms of survival
titanic_train %>%
  filter  (Title == "Miss.") %>%
    filter ((SibSp == 0) & (Parch == 0)) %>% # travelling alone - also change the ggtitle here below
      ggplot(aes(x = Age, fill = Survived)) +
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
    
titanic_train %>%
  filter  (Title =="Master.") %>%
    ggplot(aes(x = as.factor (Title), fill = Survived)) +
    geom_bar(binwidth = 0.5, stat = "count") +
    facet_wrap(~ Pclass) +
    xlab ("Title") +
    ylab ("Total Count") +
    labs (fill = "Survived")





# SIBSP
# Survival Rates per Sibling & Spouses Aboard
titanic_train %>%
  select (SibSp) %>%
  summary()
# it could be treated as factor then

titanic_train %>%
  ggplot(aes(x = as.factor(SibSp), fill = factor(Survived))) +
  geom_histogram(width = 1.0, stat = "count") +
  facet_wrap(~ Pclass) +
  xlab ("Sibsp") +
  ylab ("Total Count") +
  labs (fill = "Survived")
# looks like that the larger the family in terms of Siblings or Spouses, the worse


# PARCH
# Survival Rates per Parents and Children Aboard
titanic_train %>%
  select (Parch) %>%
  summary()
# it could be treated as factor then

titanic_train %>%
  ggplot(aes(x = as.factor(Parch), fill = factor(Survived))) +
  geom_histogram(width = 1.0, stat = "count") +
  facet_wrap(~ Pclass) +
  xlab ("Parch") +
  ylab ("Total Count") +
  labs (fill = "Survived")
# looks like that the larger the family in terms of Parent and Children, the worse


titanic_train %>%
  ggplot(aes(x = as.factor(FamilySize), fill = factor(Survived))) +
  geom_histogram(width = 1.0, stat = "count") +
  facet_wrap(~ Sex + Pclass) +
  xlab ("FamilySize") +
  ylab ("Total Count") +
  labs (fill = "Survived")





################################################################################
################################################################################
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
# Section - combination of the two datasets for mutual cleaning

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
nrow(titanic_full) # 1309 --- 891 + 418 - 1309 # 0 so the math checks out

table (titanic_full$IsTrainingSet) # let's check if the combination went well


################################################################################
################################################################################
# ADDING NEW FEATURES

# TITLES
titles <- NULL
for (i in 1:nrow (titanic_full)) {
  titles <- c (titles, extract_Title(titanic_full[i,"Name"])) 
}
titanic_full$Title <- as.factor (titles) 
tail(titanic_full)

# FAMILY SIZE
titanic_full$FamilySize <- as.numeric(titanic_full$SibSp) + as.numeric(titanic_full$Parch) + 1
tail(titanic_full)


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
survive_formula <- as.formula("Survived ~ Pclass + Sex + Age + SibSp + Parch + FarePerPerson + Embarked + Title + FamilySize")

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

# Saving the results into csv file to be updated in Kaggle - remember to version it
write.csv (output.df, "Data/Kaggle_Submission_V14.csv", row.names = FALSE)
