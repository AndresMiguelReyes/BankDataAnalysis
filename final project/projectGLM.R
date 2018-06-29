options(warn=-1)
rm(list=ls())

BankTwoThirdsTrain = read.csv("BankTwoThirdsTrain.csv")
BankOneThirdTest = read.csv("BankOneThirdTest.csv")
BankTrainPermanent = read.csv("BankTrainPermanent.csv")
BankTestPermanent = read.csv("BankTestPermanent.csv")

# this stupid X thing keeps showing up in my data, not in documentation
# probably catches a ride somewhere in pca
if("X" %in% colnames(BankTwoThirdsTrain))
{
  XLoc = which(colnames(BankTwoThirdsTrain) == "X")
  BankTwoThirdsTrain = BankTwoThirdsTrain[,-XLoc]
}
if("X" %in% colnames(BankTestPermanent))
{
  XLoc = which(colnames(BankTestPermanent) == "X")
  BankTestPermanent = BankTestPermanent[,-XLoc]
}
if("X" %in% colnames(BankTrainPermanent))
{
  XLoc = which(colnames(BankTrainPermanent) == "X")
  BankTrainPermanent = BankTrainPermanent[,-XLoc]
}

XLoc = which(colnames(BankTwoThirdsTrain) == "duration")
BankTwoThirdsTrain = BankTwoThirdsTrain[,-XLoc]
 
XLoc = which(colnames(BankOneThirdTest) == "duration")
BankOneThirdTest = BankOneThirdTest[,-XLoc]

#### GLM AND LDA ####
library(MASS)
library(pROC)
library(caret)

## FIRST GLM TO FIND WHAT'S IMPORTANT
# oops i deleted it

Bank.glm = glm(y~.-cons.conf.idx-previous-campaign-loan-housing-education-age-duration
               -pdays-nr.employed ,family=binomial,data=BankTwoThirdsTrain)
probs = predict(Bank.glm, BankOneThirdTest, type = "response")
predict.glm = numeric(length(probs))
predict.glm[probs > 0.5] = 1
print("misclass using glm: ")
predict.glm2 = as.factor(predict.glm)
predict.glm2 = ifelse(predict.glm2 == 1, "yes", "no")
print( confusionMatrix(as.factor(predict.glm2), BankOneThirdTest$y) )
testTable = table(predict.glm, BankOneThirdTest$y)
glm.misclass = (testTable[1,1] + testTable[2,2])/sum(testTable)
print(testTable)
print(glm.misclass)
print(summary(Bank.glm))
aUnder = auc(as.numeric(predict.glm),as.numeric(BankOneThirdTest$y) )
print(aUnder)


Bank.lda = lda(y~.-cons.conf.idx-poutcome-previous-campaign-loan-housing-education
               -pdays-nr.employed,data=BankTwoThirdsTrain)
lda.pred = predict(Bank.lda,newdata=BankOneThirdTest)
lda.table = table(lda.pred$class, BankOneThirdTest$y)
print("misclass using lda: ")
print( confusionMatrix(lda.pred$class, BankOneThirdTest$y) )
lda.misclass = (lda.table[1,1] + lda.table[2,2])/sum(lda.table)
print(lda.table)
print(lda.misclass)
print("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
aUnder = auc(as.numeric(lda.pred$class),as.numeric(BankOneThirdTest$y) )
print(aUnder)

# New things that are important
# default -> unknown
# contact -> telephone
# months
# duration
# poutcome
# consumer price index 
# euribor3m
# year
## job -> blue-collar
## pdays
## nr.employed
### job -> student
### day of the week -> wed


# Old one may be useful, probably not
# contact #
# month #
# duration #
# pdays ##
# emp.var.rate #
# cons.price.idx #
# euribor3m #
# nr.employed ##
# year
## job
## default
