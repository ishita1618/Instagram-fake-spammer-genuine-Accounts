# Load libraries
library(ggplot2)
library(dplyr)
library(readr)
library(caret)
library(corrplot)
library(tidyr)
library(e1071)

# Load datasets
train <- read.csv("C:/Users/ishit/Downloads/train.csv")
test <- read.csv("C:/Users/ishit/Downloads/test.csv")

# View summary
summary(train)
summary(test)


#profile pic plot
profile_pic_plot <- ggplot(train, aes(x=as.factor(profile.pic))) +
  geom_bar(fill='steelblue') +
  ggtitle("Profile Picture Distribution") +
  xlab("Has Profile Picture") +
  ylab("Count")
print(profile_pic_plot)

#Followers and Following Analysis
X.followers <- ggplot(train, aes(x = private, y = X.followers)) +
  geom_boxplot(fill = 'lightgreen') +
  ggtitle("Followers Distribution by Account Type") +
  xlab("Account Type") +
  ylab("Number of Followers")

# Print the plot
print(X.followers)

X.followers <- ggplot(train, aes(x = X.follows, y = X.followers)) +
  geom_boxplot(fill = 'lightgreen') +
  ggtitle("Followers Distribution by Account Type") +
  xlab("Follows") +
  ylab("Number of Followers")


print(X.followers)

#Username and Full Name Patterns

train$username_digits <- nchar(gsub("\\D", "", train$name..username))

# boxplot using an appropriate categorical variable
username_digits_plot <- ggplot(train, aes(x = X.follows, y = username_digits)) +
  geom_boxplot(fill = 'lightblue') +
  ggtitle("Digits in Username by Account Type") +
  xlab("Follows") +
  ylab("Number of Digits in Username")


print(username_digits_plot)

#Private vs Public Accounts
# Bar plot for Private vs Public Accounts
private_plot <- ggplot(train, aes(x = as.factor(private), fill = as.factor(fake))) +  
  geom_bar(position = "dodge") +
  ggtitle("Private vs Public Accounts") +
  xlab("Account Type (0 = Public, 1 = Private)") +
  ylab("Count")

# Print the bar plot
print(private_plot)

# Boxplot for Posts Distribution by Account Type (Public vs Private)
X.posts_plot <- ggplot(train, aes(x = as.factor(private), y = X.posts)) +  # Corrected variable name
  geom_boxplot(fill = 'violet') +
  ggtitle("Posts Distribution by Account Type") +
  xlab("Account Type (0 = Public, 1 = Private)") +
  ylab("Number of Posts")

# Print the box plot
print(X.posts_plot)

#bio length distribution
train$bio_length <- train$description.length
bio_plot <- ggplot(train, aes(x=as.factor(fake), y=bio_length)) +
  geom_boxplot(fill='orange') +
  ggtitle("Bio Length Distribution by Account Type") +
  xlab("Account Type") +
  ylab("Bio Length")
print(bio_plot)
#Data preprocessing and modeling
# Feature Engineering from Bio and Full Name
train$fullname_length <- train$nums.length.fullname
train$username_digits <- train$nums.length.username
train$desc_length <- train$description.length

# Feature Selection
train_model <- train %>%
  select(X.followers, X.follows, X.posts, profile.pic, private, username_digits, bio_length, fullname_length, desc_length, fake)

# Encode target variable
train_model$fake <- as.factor(train_model$fake)

# Data Splitting
set.seed(100)
index <- createDataPartition(train_model$fake, p=0.8, list=FALSE)
train_data <- train_model[index, ]
test_data <- train_model[-index, ]

table(train_model$profile.pic, train_model$fake)
table(train_model$private, train_model$fake)
#svm
svm_model <- train(fake ~ ., data=train_model, method='svmLinear')
svm_pred <- predict(svm_model, train_model)
confusionMatrix(svm_pred, train_model$fake)



