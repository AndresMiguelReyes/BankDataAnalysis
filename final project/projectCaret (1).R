options(warn=-1)
rm(list=ls())

BankTwoThirdsTrain = read.csv("BankTwoThirdsTrain.csv")
BankOneThirdTest = read.csv("BankOneThirdTest.csv")

# this stupid X thing keeps showing up in my data, not in documentation
# probably catches a ride somewhere in pca
if("X" %in% colnames(BankTwoThirdsTrain))
{
  XLoc = which(colnames(BankTwoThirdsTrain) == "X")
  BankTwoThirdsTrain = BankTwoThirdsTrain[,-XLoc]
}
#### CARET ####
library(caret)
Bank.downsample = downSample(x=BankTwoThirdsTrain[,-21],y=BankTwoThirdsTrain[,"y"],list=FALSE,yname="y")
trCtrl = trainControl(method="cv",classProbs=TRUE,summaryFunction=twoClassSummary)


tr.downsample = train(y~.,data=Bank.downsample,method="rf",nTree=1,
                      tuneLength=1,metric="Spec",trControl=trCtrl)
tr.downsample.pred = predict(tr.downsample,BankOneThirdTest)
downsampleConfusion = confusionMatrix(tr.downsample.pred,BankOneThirdTest$y)

# Pruned Tree Using Caret?

