options(warn=-1)
rm(list=ls())

BankTwoThirdsTrain = read.csv("BankTwoThirdsTrain.csv")
BankOneThirdTest = read.csv("BankOneThirdTest.csv")


#### KNN ####
library(class)
library(MASS)
library(pROC)
library(caret)


train.X = cbind(BankTwoThirdsTrain$euribor3m,BankTwoThirdsTrain$month,BankTwoThirdsTrain$duration,BankTwoThirdsTrain$default)
test.X = cbind(BankOneThirdTest$euribor3m,BankOneThirdTest$month,BankOneThirdTest$duration,BankOneThirdTest$default)
Y.Train =  BankTwoThirdsTrain$y
knn.pred=knn(train.X,test.X,Y.Train,k=101)
print( confusionMatrix(knn.pred, BankOneThirdTest$y) )
#knn.table = table(knn.pred, BankOneThirdTest$y)
# print("misclass using knn: ")
# knn.misclass = (knn.table[1,1] + knn.table[2,2])/sum(knn.table)
# print(knn.table)
# print(knn.misclass)
# print("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
aUnder = auc(as.numeric(knn.pred), as.numeric(BankOneThirdTest$y) )
print(aUnder)
plot(roc(as.numeric(knn.pred), as.numeric(BankOneThirdTest$y)), legacy.axes = TRUE, main= "KNN")
# 
# # find the best the k for knn using cv
# shell.exec("https://www.youtube.com/watch?v=gAPnl-46iyQ")