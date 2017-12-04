# Turn off warning because R is being a little baby
options(warn=-1)
rm(list=ls())

# still want to do NN and use oversampling/ undersampling
# i probably have enough data to under sample

################################################################################
################################################################################
                        ### DATA CLEAN UP/ SET UP ###
################################################################################
################################################################################

Bank = read.csv("bank-additional-full.csv",header=T,sep=";")
BankSolo = read.csv("bank-full.csv",header=T,sep=";")

BankNames = names(Bank)
BankSoloNames = names(BankSolo)
BankSoloNamesLength = length(BankSoloNames)
BankNamesLength = length(BankNames)
BankNamesDiff = setdiff(BankNames,BankSoloNames)

# Disconnect between paper,UCI, and txt included in file
# What do we do? 
# Found the other data set with previously augmented economic data
# Only data based on Portugal what role does the WORLD ECONOMY play on
# the individual decisions of customers? 
# We can augment metrics to gauge world economy shit to the data set
# given the month and year, but we don't have that so I'll have to make it

cat("The data set contains ",BankSoloNamesLength," without the augmented data\n")
cat("The data set contains ",BankNamesLength," with the augmented data\n")
cat("The variables that were augmented to the data set are:\n",BankNamesDiff,"\n")

# Attempting to make a year and month column for the data / confirming
# does data's "month LAST contacted" == "the month the customer WAS contacted"?
# if so we can step through unique months and use a 12 month counter to iterate
# years starting @ may 2008 - ???
# I wrote ??? b/c more discrepencies exist in the data
# paper says until Jun 2012, txt given with the data says until Nov 2011
# however actually using the data shows that it is only a span of 10 months

BankRows = nrow(Bank)
i = 1
j = 1
year = 2008
yearCol = numeric(length = BankRows)

# find the last unique consumer confidence index. B/c searching 
# online I saw that the consumer confidence index is a monthly
# measurement. We will use the consumer confidence intdex as a way
# to measure each unique month. Meaning that if we find the last 
# unique consumer confidence index we will find the last month measured in
# data set
LastConst = tail(unique(Bank$cons.conf.idx),n=1)

#Find the index of the last consumer confidence index
LastIdx = min(which(Bank$cons.conf.idx == LastConst))

# We need to make sure that the consumer confidence interval is truly a measure
# for each unique month in the data set. In the while loop we will find the
# index of the first element not equal to the previous month and consumer conf idx
# in both the month and cons conf idx arrays of the bank data set.

# # # After running this once I confirmed that cons conf idx is # # #
# # # truly a way to measure unique months in the data set, for # # #
# # # computation's sake I will use the same loop to set the yr # # #

# To set the year of the data I will use the existing while loop to 
# step through each month. The while loop seperates each month into it's
# own seperate subarray, I will set the year by those subarrays, Initializing
# the year at 2008 then iterating by 1 everytime month== "dec"

while( (i < LastIdx)  )
{
  tempi = min(which(Bank$cons.conf.idx[i:BankRows] != Bank$cons.conf.idx[i]))
  tempj = min(which(Bank$month[j:BankRows] != Bank$month[j]))

  oldj = j  
  
  if(Bank$month[j] == "dec")
  {
    year = year + 1  
  }
  
  if ( (i == 1) & (j == 1) )
  {
    i = tempi
    j = tempj
  }
  else
  {
    i = i + tempi - 1
    j = j + tempj - 1
  }
  
  # set the year of the subarray
  yearCol[oldj : j] = year
  
  # get the month in text to print and check if the months are in order
  month = levels(Bank$month)[Bank$month[j]]
  
  cat(i," , ",j,"\n")
  cat(Bank$cons.conf.idx[i]," , ",month," , ",yearCol[i],"\n")
}

# last subarray isnt completely filled, we broke while loop before reaching the
# last index of the data frame. We will fill in the remaining elements of
# the last subarray.
yearCol[j:BankRows] = year

# add the year column to our data set
Bank$year = yearCol

# set up a training data frame
train = which(Bank$year != 2010)
BankTrain = Bank[train,]

# set up a permanent testing data frame
BankTestPermanent = Bank[-train,]

# but how are we going to actually split it up?
# let's print out the number of calls made in each year of our training set
calls08 = length(which(BankTrain$year != 2009))
cat("number of calls made in 2008: ",calls08,"\n")

calls09 = length(which(BankTrain$year != 2008))
cat("number of calls made in 2009: ",calls09,"\n")

# Okay so each year doesn't have the same amount of calls going out, which would make sense because
# 2008 starts in May and the campaign in 2009 started in March. So do we omit months that aren't in every
# recorded year? 
train08 = which(BankTrain$year != 2009)
train09 = which(BankTrain$year != 2008)

print("for the 2008 marketing campaign:")
print(table(BankTrain$y[train08]))
print("for the 2009 marketing campaign:")
print(table(BankTrain$y[train09]))

# We are never going to look at the data collected in 2010, this will be our PERMANENT 
# test set. I will not run the data on the 2010 test set until the end of the project.
# we will now split the training set into another training set, I will take 2/3 of the 
# data from each of the months in 2008 and 2009. The remaining data will be my working
# test set.
trainRows = nrow(BankTrain)

BankTrain08 = BankTrain[train08,]
BankTrain09 = BankTrain[train09,]

train08Rows = nrow(BankTrain08)
train09Rows = nrow(BankTrain09)

train08months = unique(BankTrain08$month)
train09months = unique(BankTrain09$month)

cat("\n++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")

BankTwoThirdsTrain = Bank[0,]
BankOneThirdTest = Bank[0,]

for(j in 1:length(train08months))
{
  MonthObserv = BankTrain08[BankTrain08$month == train08months[j],]
  nMonthObserv = nrow(MonthObserv)
  
  cat(levels(train08months)[train08months[j]]," , ",nMonthObserv,"\n")
  
  monthSplit = floor( (2/3) * nMonthObserv )
  monthTrainRows = sample(nMonthObserv,monthSplit)
  monthTrainTemp = MonthObserv[monthTrainRows,]
  monthTestTemp = MonthObserv[-monthTrainRows,]
  
  #rbind test and train dfs to empty dfs
  BankTwoThirdsTrain = rbind(BankTwoThirdsTrain,monthTrainTemp)
  BankOneThirdTest = rbind(BankOneThirdTest,monthTestTemp)
  
  cat(nrow(monthTrainTemp)," , ",nrow(monthTestTemp),"\n")
  
}

cat("Size of train after 08",nrow(BankTwoThirdsTrain),"\n")
cat("Size of test after 08",nrow(BankOneThirdTest),"\n")

for(j in 1:length(train09months))
{
  MonthObserv = BankTrain09[BankTrain09$month == train09months[j],]
  nMonthObserv = nrow(MonthObserv)
  
  cat(levels(train09months)[train09months[j]]," , ",nMonthObserv,"\n")
  
  monthSplit = floor( (2/3) * nMonthObserv )
  monthTrainRows = sample(nMonthObserv,monthSplit)
  monthTrainTemp = MonthObserv[monthTrainRows,]
  monthTestTemp = MonthObserv[-monthTrainRows,]
  
  #rbind test and train dfs to empty dfs
  BankTwoThirdsTrain = rbind(BankTwoThirdsTrain,monthTrainTemp)
  BankOneThirdTest = rbind(BankOneThirdTest,monthTestTemp)
  
  cat(nrow(monthTrainTemp)," , ",nrow(monthTestTemp),"\n")
  
}
cat("Size of train after 09",nrow(BankTwoThirdsTrain),"\n")
cat("Size of test after 09",nrow(BankOneThirdTest),"\n")

################################################################################
################################################################################



################################################################################
################################################################################
                          ### TREE MODEL SHIT ###
################################################################################
################################################################################

#### THIS IS ACTUALL ALL WRONG, HAVE TO GO BACK AND REPLACE BANK INST WITH BANKTRAIN
library(tree)

# tree
mycontrol = tree.control(nobs=nrow(BankTwoThirdsTrain),mincut = 1,minsize = 2,mindev = 2.2e-16)
tree.y = tree(y ~ .,data=BankTwoThirdsTrain,control = mycontrol)
tree.y2 = tree(y ~ .,data=BankTwoThirdsTrain)

# plot the tree
plot(tree.y)
plot(tree.y2)
text(tree.y2,pretty=0)

cat("\n++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
#get our training accuracy on the models
#print(tree.y) #this is kind of dumb b/c its tooo big
print( summary(tree.y) )
cat("\n++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
#print(tree.y2) #nice model, but I don't really need it b.c I used text
print( summary(tree.y2) )
cat("\n++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")

# get testing error and confusion matrix for the 2/3 split using a big unpruned tree
tree.pred = predict(tree.y,BankOneThirdTest,type="class")
tree.table = table(tree.pred, BankOneThirdTest$y)
testMisclass = (tree.table[1,1] + tree.table[2,2])/sum(tree.table)
print(tree.table)
print(testMisclass)
cat("\n++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")

# get testing error and confusion matrix for the 2/3 split using a default tree
tree.pred2 = predict(tree.y2,BankOneThirdTest,type="class")
tree.table2 = table(tree.pred2, BankOneThirdTest$y)
testMisclass2 = (tree.table2[1,1] + tree.table2[2,2])/sum(tree.table2)
print(tree.table2)
print(testMisclass2)
cat("\n++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")

# we need to try tree minus unimportant factors
# also try diff test/train splits

# we will now use Cross Validation to prune out the unimportant things from the big tree
# we are using misclass as our guide (maybe there is one for getting more yes)
cv.y = cv.tree(tree.y,FUN=prune.misclass)
print(names(cv.y))
print( cv.y )

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

prune.bankTrain=prune.misclass(tree.y,best=9)#,best=minTreeSizeVal)
#plot(prune.bankTrain) # wont work in the script for some reason
#text(prune.bankTrain,pretty=0)
prune.pred=predict(prune.bankTrain,BankOneThirdTest,type="class")
prune.table = table(prune.pred, BankOneThirdTest$y)
prune.misclass = (prune.table[1,1] + prune.table[2,2])/sum(prune.table)
print(prune.table)
print(prune.misclass)

# make a test bag function later & a test tree func ^
##### bagged model ####
library(randomForest)

# number of pred
bankTrainNum = ncol(BankTwoThirdsTrain) - 1

# make a default bagged model
bag.BankTwoThirdsTrain = randomForest(y~.,data=BankTwoThirdsTrain,mtry=bankTrainNum,importance=T)
print(bag.BankTwoThirdsTrain)

# test bagged model on test set
yhat.bag = predict(bag.BankTwoThirdsTrain,newdata=BankOneThirdTest)
bag.table = table(yhat.bag, BankOneThirdTest$y)
bag.misclass = (bag.table[1,1] + bag.table[2,2])/sum(bag.table)
print(bag.table)
print(bag.misclass)

# we can control how many trees we grow by using ntrees, lets try growing 25 trees
print("bag with 25 trees:")
n = 25
bag.BankTwoThirdsTrain2 = randomForest(y~.,data=BankTwoThirdsTrain,mtry=bankTrainNum,ntree=n)
yhat.bag = predict(bag.BankTwoThirdsTrain,newdata=BankOneThirdTest)
bag.table = table(yhat.bag, BankOneThirdTest$y)
bag.misclass = (bag.table[1,1] + bag.table[2,2])/sum(bag.table)
print(bag.table)
print(bag.misclass)

# b/c bagging is just rf where m = p, we can change m to see how a def rf would be on test set
# def value of m in rf is (p/3) for classifcation trees and sqrt(p) for regression trees, lets try 4
print("rf with m=4")
bag.BankTwoThirdsTrain = randomForest(y~.,data=BankTwoThirdsTrain,mtry=4,importance=T)
yhat.bag = predict(bag.BankTwoThirdsTrain,newdata=BankOneThirdTest)
bag.table = table(yhat.bag, BankOneThirdTest$y)
bag.misclass = (bag.table[1,1] + bag.table[2,2])/sum(bag.table)
print(bag.table)
print(bag.misclass)
print("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")

################################################################################
################################################################################

################################################################################
################################################################################
                            ### YEAH BOI KNN ###
################################################################################
################################################################################
library(class)
train.X = cbind(BankTwoThirdsTrain$euribor3m,BankTwoThirdsTrain$month,BankTwoThirdsTrain$duration,BankTwoThirdsTrain$default)
test.X = cbind(BankOneThirdTest$euribor3m,BankOneThirdTest$month,BankOneThirdTest$duration,BankOneThirdTest$default)
Y.Train =  BankTwoThirdsTrain$y
knn.pred=knn (train.X,test.X,Y.Train,k=101)
knn.table = table(knn.pred, BankOneThirdTest$y)
print("misclass using knn: ")
knn.misclass = (knn.table[1,1] + knn.table[2,2])/sum(knn.table)
print(knn.table)
print(knn.misclass)
print("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
################################################################################
################################################################################

################################################################################
################################################################################
                            ### LDA AND GLM ###
################################################################################
################################################################################
Bank.glm = glm(y~.-cons.conf.idx-poutcome-previous-campaign-loan-housing-education
               ,family=binomial,data=BankTwoThirdsTrain)
probs = predict(Bank.glm, BankOneThirdTest, type = "response")
predict.glm = numeric(length(probs))
predict.glm[probs > 0.5] = 1
print("misclass using glm: ")
testTable = table(predict.glm, BankOneThirdTest$y)
glm.misclass = (testTable[1,1] + testTable[2,2])/sum(testTable)
print(testTable)
print(glm.misclass)

Bank.lda = lda(y~.-cons.conf.idx-poutcome-previous-campaign-loan-housing-education,
               data=BankTwoThirdsTrain)
lda.pred = predict(Bank.lda,newdata=BankOneThirdTest)
lda.table = table(lda.pred$class, BankOneThirdTest$y)
print("misclass using lda: ")
lda.misclass = (lda.table[1,1] + lda.table[2,2])/sum(lda.table)
print(lda.table)
print(lda.misclass)
print("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
################################################################################
################################################################################

################################################################################
# Turn warnings back on
options(warn=0)