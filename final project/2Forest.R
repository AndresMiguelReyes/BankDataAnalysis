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


shell.exec("https://www.youtube.com/watch?v=qRFhNZNu_xw")

# Confusion Matrix and Statistics
# 
# Reference
# Prediction    no   yes
# no  11855   893
# yes   719  1011
# Accuracy : 0.8887          
# 95% CI : (0.8834, 0.8937)
# No Information Rate : 0.8685          
# P-Value [Acc > NIR] : 1.134e-13       
# 
# Kappa : 0.4929          
# Mcnemar's Test P-Value : 1.641e-05       
# 
# Sensitivity : 0.9428          
# Specificity : 0.5310          
# Pos Pred Value : 0.9299          
# Neg Pred Value : 0.5844          
# Prevalence : 0.8685          
# Detection Rate : 0.8188          
# Detection Prevalence : 0.8805          
# Balanced Accuracy : 0.7369          
# 
# 'Positive' Class : no              
# 
# Area under the curve: 0.7572