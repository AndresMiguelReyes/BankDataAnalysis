options(warn=-1)
rm(list=ls())

BankTwoThirdsTrain = read.csv("BankTwoThirdsTrain.csv")
BankOneThirdTest = read.csv("BankOneThirdTest.csv")

if("X" %in% colnames(BankTwoThirdsTrain))
{
  XLoc = which(colnames(BankTwoThirdsTrain) == "X")
  BankTwoThirdsTrain = BankTwoThirdsTrain[,-XLoc]
}
if("X" %in% colnames(BankOneThirdTest))
{
  XLoc = which(colnames(BankOneThirdTest) == "X")
  BankTwoThirdsTrain = BankOneThirdTest[,-XLoc]
}
#### RandomForest Shit ####

library(randomForest)
library(caret)

# number of pred
bankTrainNum = ncol(BankTwoThirdsTrain) - 1

# make a default bagged model
bag.BankTwoThirdsTrain = randomForest(y~.-duration-year,data=BankTwoThirdsTrain,mtry=bankTrainNum,importance=T)
print(bag.BankTwoThirdsTrain)

# test bagged model on test set
yhat.bag = predict(bag.BankTwoThirdsTrain,newdata=BankOneThirdTest)
print( confusionMatrix(yhat.bag, BankOneThirdTest$y) )
aUnder = auc(as.numeric(yhat.bag), as.numeric(BankOneThirdTest$y) )
print(aUnder)
bag.table = table(yhat.bag, BankOneThirdTest$y)
bag.misclass = (bag.table[1,1] + bag.table[2,2])/sum(bag.table)
print(bag.table)
print(bag.misclass)

# we can control how many trees we grow by using ntrees, lets try growing 25 trees
print("bag with 25 trees:")
n = 25
tr.downsample = randomForest(y~.-duration,data=BankTwoThirdsTrain,mtry=bankTrainNum,ntree=n)
yhat.bag = predict(tr.downsample,newdata=BankOneThirdTest)
print( confusionMatrix(yhat.bag, BankOneThirdTest$y) )
aUnder = auc(as.numeric(yhat.bag), as.numeric(BankOneThirdTest$y) )
print(aUnder)
bag.table = table(yhat.bag, BankOneThirdTest$y)
bag.misclass = (bag.table[1,1] + bag.table[2,2])/sum(bag.table)
print(bag.table)
print(bag.misclass)

# b/c bagging is just rf where m = p, we can change m to see how a def rf would be on test set
# # def value of m in rf is (p/3) for classifcation trees and sqrt(p) for regression trees, lets try 4
print("rf with m=4")
yhat.bag = predict(bag.BankTwoThirdsTrain,newdata=BankOneThirdTest)
print( confusionMatrix(yhat.bag, BankOneThirdTest$y) )
Under = auc(as.numeric(yhat.bag), as.numeric(BankOneThirdTest$y) )
print(aUnder)
bag.table = table(yhat.bag, BankOneThirdTest$y)
bag.misclass = (bag.table[1,1] + bag.table[2,2])/sum(bag.table)
print(bag.table)
print(bag.misclass)
print("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
print(importance(bag.BankTwoThirdsTrain))
varImpPlot(tr.downsample)
