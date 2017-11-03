# importing data
train <- read.table("train.csv", h = TRUE, sep=",")
test <- read.table("test.csv", h=TRUE, sep = ",")
full <- rbind(train[,-2], test)

# next line prints test table
#test

# descriptive statistics
str(full)
# counting frequencies
survived <- table(train$Survived) # writing it into local variable
prop.table(survived) # finding percent of survived 
classes <- table(train$Pclass) # counting passengers of different classes
prop.table(classes) # finding percent of classes
sex <- table(train$Sex)
prop.table(sex)

# crosstables
survivedofsex <- table(train$Sex, train$Survived)
prop.table(survivedofsex)
mosaicplot(survivedofsex) # makes a plot

averageage <- mean(train$Age, na.rm = TRUE)
averageageofdead <- mean(train$Age[train$Survived==0], na.rm = TRUE)
averageageofsurvived <- mean(train$Age[train$Survived==1], na.rm = TRUE)

colSums(is.na(full))
embarked <- table(train$Embarked)
train$Embarked[train$Embarked==""]="S"

# correlation
train_set <- train_set[complete.cases(train_set),]
cor(clear_train$Fare, clear_train$Pclass)

#data types
str(train)
apply(train, 2, function(x) length(unique(x)))

train$Survived <- as.factor(train$Survived)
train$Pclass <- as.factor(train$Pclass)
train$Sex <- as.factor(train$Sex)
train$Embarked <- as.factor(train$Embarked)
test$Pclass <- as.factor(test$Pclass)
str(train)

#making some nice-looking plots
install.packages("ggplot2")
library(ggplot2)
ggplot(train, aes(Pclass, fill=Survived)) + geom_bar()
ggplot(train, aes(Pclass, fill=Survived)) + geom_bar(position = "fill")
ggplot(train, aes(Pclass, fill=Survived)) + geom_bar() + facet_wrap(~Sex)

# machine learning starts there!
split <- runif(dim(train)[1]) > 0.3
train_set <- train[split,]
test_set <- train[!split,]

# classification tree
library(rpart)
fit_TD <- rpart(Survived~Age+Sex+Pclass, data = train_set, method = "class")
fit_TD
plot(fit_TD)
text(fit_TD)
install.packages("rpart.plot")
library("rpart.plot")
rpart.plot(fit_TD, extra = 3, fallen.leaves = FALSE)

# ranking model
install.packages("caret")
library("caret")
DT_prediction <- predict(fit_TD, data=train_set, type = "class")
sum(DT_prediction==train_set$Survived)/nrow(train_set)
library(caret)
install.packages("e1071")
library("e1071")
confusionMatrix(DT_prediction, train_set$Survived, positive = "1")


# don't touch this code. IT WORKS!!!!
test_set$new <- predict(fit_TD, test_set, type = "class")
sum(test_set$new==test_set$Survived)/nrow(test_set)
# doesn't work
#ctrl <- trainControl(method = "repeatedcv", repeats = 5)
#grid <- expand.grid(k=3:9)
#Fit_knn <- train(Survived~Age+Sex+Pclass,
#                data = train_set,
#                method = "knn",
#                trControl = ctrl,
#                tuneGrid = grid
#                 )
#Fit_knn



Fit_logit <- glm(Survived~Age+Sex+Pclass+Embarked+Fare, family = binomial, data = train_set)
Fit_logit

summary(Fit_logit)
test_set$new <- predict(Fit_logit, test_set, type = "response")
sum(test_set$new==test_set$Survived)/nrow(test_set)
exp(Fit_logit$coefficients)
test$Survived <- predict(fit, data = test, type = "response")

test$Survived <- predict(fit_TD, test, type = "class")
result <- test[,c("PassengerId", "Survived")]
write.csv(result, file = "Titanic_prediction.csv", row.names = FALSE, quote = FALSE)
