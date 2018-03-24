library(caret)
library(rpart)
library(e1071)
library(randomForest)
set.seed(1)

train.raw <- read.csv('pml-training.csv', na.strings=c("NA","#DIV/0!",""))
test.raw <- read.csv('pml-testing.csv', na.strings=c("NA","#DIV/0!",""))

# Remove first 7 columns (no useful data) and removed columns with NAs
train.clean <- train.raw[, 8:ncol(train.raw)]
train.clean <- train.clean[, colSums(is.na(train.clean)) == 0] 

train <- createDataPartition(train.clean$classe, p=0.70, list=F)
train.clean <- train.clean[train, ]
validate <- train.clean[-train, ]

plot(train.clean$classe, main="Class Breakdown of Training Set", xlab="Classe", ylab="Count")

dt.model <- rpart(classe ~ ., data = train.clean, method = "class")
dt.predict <- predict(dt.model, validate, type = "class")
confusionMatrix(validate$classe, dt.predict)

rf.model <- randomForst(classe ~ ., data = train.clean, method = "class")
rf.predict <- predict(rf.model, validate, type = "class")
confusionMatrix(validate$classe, rf.predict)

# Clean testing data set and predict
test.clean <- test.raw[, 8:ncol(test.raw)]
test.clean <- test.clean[, colSums(is.na(test.clean)) == 0] 
results <- predict(rf.model, test.clean[, -length(names(test.clean))])
