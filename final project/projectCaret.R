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
library(pROC)

#### Downsampled RandForest with all Variables ####
Bank.downsample = downSample(x=BankTwoThirdsTrain[,-21],y=BankTwoThirdsTrain[,"y"],list=FALSE,yname="y")
trCtrl = trainControl(method="cv",classProbs=TRUE,summaryFunction=twoClassSummary)

tr.downsample = train(y~.,data=Bank.downsample,method="rf",nTree=100,
                      tuneLength=10,metric="Spec",trControl=trCtrl,do.trace= 10)
tr.downsample.pred = predict(tr.downsample,BankOneThirdTest)
tr.downsample.pred.prob = predict(tr.downsample,BankOneThirdTest,type="prob")
downsampleConfusion = confusionMatrix(tr.downsample.pred,BankOneThirdTest$y)
print(downsampleConfusion)

# Area Under curve
downsample.aUnder = auc(as.numeric(tr.downsample.pred), as.numeric(BankOneThirdTest$y) )
print(downsample.aUnder)

# ROC for downsample
downsample.roc = roc(as.numeric(tr.downsample.pred.prob$yes), as.numeric(BankOneThirdTest$y))
plot(downsample.roc, legacy.axes = TRUE, print.thres="best", print.thres.best.method="closest.topleft", main= "RF Using Caret and DownSampling")

downsampImp = varImp(tr.downsample,main="Downsampled top 10 variables")
plot(downsampImp,top=10)

#### Upsampled RandForest with all Variables ####
Bank.upsample = upSample(x=BankTwoThirdsTrain[,-21],y=BankTwoThirdsTrain[,"y"],list=FALSE,yname="y")
trCtrl = trainControl(method="cv",classProbs=TRUE,summaryFunction=twoClassSummary)


tr.upsample = train(y~.,data=Bank.upsample,method="rf",nTree=100,
                    tuneLength=10,metric="Spec",trControl=trCtrl,do.trace= 10)
tr.upsample.pred = predict(tr.upsample,BankOneThirdTest)
tr.upsample.pred.prob = predict(tr.upsample,BankOneThirdTest,type="prob")
upsampleConfusion = confusionMatrix(tr.upsample.pred,BankOneThirdTest$y)
print(upsampleConfusion)

# Area Under curve
aUnder = auc(as.numeric(tr.upsample.pred), as.numeric(BankOneThirdTest$y) )
print(aUnder)

# ROC for upsample
upsample.roc = roc(as.numeric(tr.upsample.pred.prob$yes), as.numeric(BankOneThirdTest$y))
plot(upsample.roc, legacy.axes = TRUE, print.thres="best", print.thres.best.method="closest.topleft", main= "RF Using Caret and upSampling")

upsampImp = varImp(tr.upsample)
plot(upsampImp,top=10,main="Upsampled top 10 variables")

shell.exec("https://www.youtube.com/watch?v=qRFhNZNu_xw")