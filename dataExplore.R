library(readr)
creditcard <- read_csv("~/Downloads/creditcard.csv")

creditcard$Class <- as.factor(creditcard$Class)

# ensure the results are repeatable
set.seed(100)
# load the library
library(mlbench)
library(caret)

# calculate correlation matrix
correlationMatrix <- cor(creditcard[,1:30])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)

sample <- credit
#Rank Features By Importance
set.seed(200)
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(Class~., data=creditcard, method="lvq", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)
