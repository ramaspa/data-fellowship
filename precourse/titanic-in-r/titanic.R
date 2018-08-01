# The purpose of this code is to predict and assign survival 
# values to Titanic passengers with unknown survival statistics
# based on data analysis. 

# Set my working directory to one where my files are stored.
setwd("~/Documents/data-fellowship/titanic-in-r")

# Import my test and training datasets and save them in variables
library(readxl)
test <- read_excel("~/Documents/data-fellowship/titanic-in-r/test.xlsx")
train <- read_excel("~/Documents/data-fellowship/titanic-in-r/train.xlsx")

#######################################################
# Explore the data
#######################################################

# Start by making sure our data types are correct

# First the training data
# Check out what data types are included
str(train)

# Convert Pclass and Survived to factors in the training data
train$Pclass <- as.factor(train$Pclass)
train$Survived <- as.factor(train$Survived)

# Convert names to strings in the training data
train$Name <- as.character(train$Name)

# Now the test data
# Convert Pclass and Survived to factors in the test data
test$Pclass <- as.factor(test$Pclass)

# Convert names to strings in the test data
test$Name <- as.character(test$Name)

# Now let's look for duplicates

# Check there are 891 unique names
length(unique(train$Name))

# Check there are 418 unique names
length(unique(test$Name))

# Let's explore the data a bit
table(train$Survived)

prop.table(table(train$Survived))

# Load ggplot2
install.packages("ggplot2")
library(ggplot2)

ggplot(train, aes(x = Sex, fill = factor(Survived))) +
  geom_bar(width = 0.5) +
  xlab("Sex") +
  ylab("Total count") + 
  labs(fill = "Survived")

#######################################################
# Create my first model
#######################################################

# Create a new column in train
train$ModelPrediction <- "TBD"

# Check the column was created
head(train)

# Assisgn a value of 0 to each observation in ModelPrediction
train$ModelPrediction <- 0

# Check ModelPrediction is same type as Survived
str(train$ModelPrediction)

# Calculate the percentage accuracy of your current predictions
sum(train$ModelPrediction == train$Survived)/nrow(train)

# Reset ModelPrediction value to 0
train$ModelPrediction <- "TBD"

# Assign Survived value of 0 for every Male, and value of 1 for every Female in train data
train$ModelPrediction[train$Sex == "male"] <- 0
train$ModelPrediction[train$Sex == "female"] <- 1

# Check the Survived values have been created
head(train)

# Check the new model percentage accuracy of your current predictions
sum(train$ModelPrediction == train$Survived)/nrow(train)

# Use table function to check whether Pclass could be a good predictor of survival rates
table(train$Pclass, train$Survived)

# Use another table function to compare the Pclass and Survived rates
prop.table(table(train$Pclass, train$Survived))

# Use ggplot to create a bar graph of 
ggplot(train, aes(x = Sex, fill = Survived)) +
  geom_bar(width = 0.5) +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("Sex") +
  ylab("Total count") +
  labs(fill = "Survived")

#######################################################
# My second model
#######################################################

# Reset ModelPrediction to 0
train$ModelPrediction <- 0

# Assign value of 1 to all Females, except Females in Pclass 3
train$ModelPrediction[train$Sex == "female" & (train$Pclass == "1" | train$Pclass == "2")] <- 1

# Calculate my new model percentage accuracy
sum(train$ModelPrediction == train$Survived)/nrow(train)

# Create a visualisation that helps show the relationship between Pclass, Sex, Age, and Survival
ggplot(train, aes(x = Age, fill = Survived)) +
  facet_wrap(~Sex + Pclass) +
  geom_bar(width = 10) +
  xlab("Age") +
  ylab("Total count")

#######################################################
# My third model
#######################################################

# Reset ModelPrediction to 0
train$ModelPrediction <- 0

# Assign value of 1 to all Females, except Females in Pclass 3
train$ModelPrediction[train$Sex == "female" & (train$Pclass == "1" | train$Pclass == "2")] <- 1

# Assign value of 1 to all Males in Pclass 1 & 2, under the age of 18
train$ModelPrediction[(train$Pclass == "1" | train$Pclass == "2") & train$Sex == "male" & train$Age < 18] <- 1

# Calculate my new model percentage accuracy
sum(train$ModelPrediction == train$Survived)/nrow(train)

#######################################################
# My fourth model
#######################################################

# Check for other variable that could be a good indicator of survival
table(train$SibSp, train$Survived)
prop.table(table(train$SibSp, train$Survived))

# Reset ModelPrediction to 0
train$ModelPrediction <- 0

# Assign value of 1 to all Females, except Females in Pclass 3
train$ModelPrediction[train$Sex == "female" & (train$Pclass == "1" | train$Pclass == "2")] <- 1

# Assign value of 1 to all Males in Pclass 1 & 2, under the age of 18
train$ModelPrediction[(train$Pclass == "1" | train$Pclass == "2") & train$Sex == "male" & train$Age < 18] <- 1

# Assign value of 1 for all female passengers with no siblings
train$ModelPrediction[train$Sex == "female" & train$SibSp == "0"] <- 1

# Calculate my new model percentage accuracy
sum(train$ModelPrediction == train$Survived)/nrow(train)

#######################################################
# Test data submission
#######################################################

# Let's create a new dataframe called "submission"
# Start it with test's PassengerId column
submission <- data.frame(test$PassengerId)

# View the first few rows of your new dataframe
head(submission)

# Rename the column header to match the 
# submission guidelines
names(submission)[names(submission) == "test.PassengerId"] <- "PassengerId"

# Check the name has changed
head(submission)

# Let's start assigning survival values to our test data
# like we did with our training data by creating a column
# called Survived that assumes everyone perished.
test$Survived <- 0

# Check our column has been created appropriately
head(test)

# Next, we can again say all females survive
test$Survived[test$Sex == "female"] <- 1

# Again, check whether our dataframe looks as it should
head(test)

# Now, assume all females in third class perish
test$Survived[test$Pclass == "3"] <- 0

# Assume all males in first and second class under 
# 18 surived
test$Survived[(test$Pclass == "1" | test$Pclass == "2") & test$Sex == "male" & test$Age < 18] <- 1

# Assume all female passengers with no siblings survived
test$Survived[test$Sex == "female" & test$SibSp == "0"] <- 1

# Glance over your new predictions and see 
# if they seem reasonable
View(test)

# Copy your new predictions to a column called
# Survived in your "submission" dataframe
submission$Survived <- test$Survived

# Export your predictions to csv so you can upload them
# to the competition!
write.csv(submission, file = "titanic_in_r_submission.csv")