options(warn=-1)
rm(list=ls())

BankTwoThirdsTrain = read.csv("BankTwoThirdsTrain.csv")
BankOneThirdTest = read.csv("BankOneThirdTest.csv")

library(dummies)
Num = sapply(BankTwoThirdsTrain, is.numeric)
#nonNumNames = names(BankTwoThirdsTrain[,-Num])

#new_BankTwoThirdsTrain = BankTwoThirdsTrain[sapply(BankTwoThirdsTrain,is.factor)]
nonNumNames = names( BankTwoThirdsTrain[sapply(BankTwoThirdsTrain,is.factor)] )
newBankTwoThirdsTrain = dummy.data.frame(BankTwoThirdsTrain, names = nonNumNames)

BankTwoThirdsTrain.pca = prcomp(newBankTwoThirdsTrain,scale=TRUE,retx=TRUE)
#
plot(BankTwoThirdsTrain.pca$x[,1],BankTwoThirdsTrain.pca$x[,2],pch=19)

# how much do we reduce the dimensionality by?
print( dim(BankTwoThirdsTrain.pca$x) )
print( dim(BankTwoThirdsTrain) )


# plot the pca
# only 4/5 of the predictors in the space capture most of the variance
plot(BankTwoThirdsTrain.pca$sdev^2)
abline(v=12)
