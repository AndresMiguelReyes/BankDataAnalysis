options(warn=-1)
rm(list=ls())

BankTwoThirdsTrain = read.csv("BankTwoThirdsTrain.csv")
BankOneThirdTest = read.csv("BankOneThirdTest.csv")

XLoc = which(colnames(BankTwoThirdsTrain) == "duration")

BankTwoThirdsTrain = BankTwoThirdsTrain[,-XLoc]

XLoc = which(colnames(BankOneThirdTest) == "duration")
BankOneThirdTest = BankOneThirdTest[,-XLoc]

# this stupid X thing keeps showing up in my data, not in documentation
# probably catches a ride somewhere in pca
if("X" %in% colnames(BankTwoThirdsTrain))
{
  XLoc = which(colnames(BankTwoThirdsTrain) == "X")
  BankTwoThirdsTrain = BankTwoThirdsTrain[,-XLoc]
}

#### TREE MODEL SHIT ####
library(tree)
library(caret)

# tree
mycontrol = tree.control(nobs=nrow(BankTwoThirdsTrain),mincut = 16,minsize = 32,mindev = 4.4e-16)
tree.y = tree(y ~ .,data=BankTwoThirdsTrain,control = mycontrol)
tree.y2 = tree(y ~ .,data=BankTwoThirdsTrain)

# plot the tree
#plot(tree.y)
plot(tree.y2)
text(tree.y2,pretty=0)

cat("\n++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
#get our training accuracy on the models
#print(tree.y) #this is kind of dumb b/c its tooo big
#print( summary(tree.y) )
cat("\n++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
#print(tree.y2) #nice model, but I don't really need it b.c I used text
print( summary(tree.y2) )
cat("\n++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")

# get testing error and confusion matrix for the 2/3 split using a big unpruned tree
tree.pred = predict(tree.y,BankOneThirdTest,type="class")
print( confusionMatrix(tree.pred, BankOneThirdTest$y) )
# tree.table = table(tree.pred, BankOneThirdTest$y)
# testMisclass = (tree.table[1,1] + tree.table[2,2])/sum(tree.table)
# print(tree.table)
# print(testMisclass)
aUnder = auc(as.numeric(tree.pred), as.numeric(BankOneThirdTest$y) )
print(aUnder)
plot.roc(roc(as.numeric(tree.pred), as.numeric(BankOneThirdTest$y) ),main="Big Tree")
cat("\n++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
tree.pred2 = predict(tree.y2,BankOneThirdTest,type="class")
print( confusionMatrix(tree.pred2, BankOneThirdTest$y) )
# tree.table2 = table(tree.pred2, BankOneThirdTest$y)
# testMisclass2 = (tree.table2[1,1] + tree.table2[2,2])/sum(tree.table2)
# print(tree.table2)
# print(testMisclass2)
cat("\n++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")

# we need to try tree minus unimportant factors
# also try diff test/train splits

# we will now use Cross Validation to prune out the unimportant things from the big tree
# we are using misclass as our guide (maybe there is one for getting more yes)
cv.y = cv.tree(tree.y,FUN=prune.misclass)
#print(names(cv.y))
#print( cv.y )

# pick smallest test error
minTreeSize = cv.y$size[which(cv.y$dev == min(cv.y$dev))]
print(minTreeSize)
#from the smallest test error pick the smallest size
minTreeSizeVal = min(minTreeSize)
print(minTreeSizeVal)

par(mfrow=c(1,2))
plot(cv.y$size,cv.y$dev,type="b")
plot(cv.y$k,cv.y$dev,type="b")
par(mfrow=c(1,1))

prune.bankTrain=prune.misclass(tree.y,best=11)#,best=minTreeSizeVal)
plot(prune.bankTrain) # wont work in the script for some reason
text(prune.bankTrain,pretty=0)
prune.pred=predict(prune.bankTrain,BankOneThirdTest,type="class")
print( confusionMatrix(prune.pred, BankOneThirdTest$y) )
aUnder = auc(as.numeric(prune.pred), as.numeric(BankOneThirdTest$y) )
print(aUnder)
plot.roc(roc(as.numeric(prune.pred), as.numeric(BankOneThirdTest$y) ),main="Pruned Tree")
# prune.table = table(prune.pred, BankOneThirdTest$y)
# prune.misclass = (prune.table[1,1] + prune.table[2,2])/sum(prune.table)
# print(prune.table)
# print(prune.misclass)