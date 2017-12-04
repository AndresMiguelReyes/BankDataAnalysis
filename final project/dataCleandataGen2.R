options(warn=-1)
rm(list=ls())

Bank = read.csv("bank-additional-full.csv",header=T,sep=";")

# duration should not be used because we don't know it before the phone call
XLoc = which(colnames(Bank) == "duration")
Bank = Bank[,-XLoc]

# generate a year column because I want to build an overall model for the term deposit telemarketing

BankRows = nrow(Bank)
i = 1
j = 1
year = 2008
yearCol = numeric(length = BankRows)

LastConst = tail(unique(Bank$cons.conf.idx),n=1)
LastIdx = min(which(Bank$cons.conf.idx == LastConst))

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
}

# last subarray isnt completely filled, we broke while loop before reaching the
# last index of the data frame. We will fill in the remaining elements of
# the last subarray.
yearCol[j:BankRows] = year

# add the year column to our data set
Bank$year = yearCol

# split the data into each year
train08 = which(Bank$year != 2009)
train09 = which(Bank$year != 2008)
train10 = which( (Bank$year != 2008) && (BankTrainPermanent$year != 2009) )

BankTrain08 = Bank[train08,]
BankTrain09 = Bank[train09,]
BankTrain10 = Bank[train10,]

train08months = unique(BankTrain08$month)
train09months = unique(BankTrain09$month)
train10months = unique(BankTrain10$month)

BankTwoThirdsTrain = Bank[0,]
BankOneThirdTest = Bank[0,]

# iterate through each year and for each month take 2/3 of the data from each month as a training set
for(j in 1:length(train08months))
{
  MonthObserv = BankTrain08[BankTrain08$month == train08months[j],]
  nMonthObserv = nrow(MonthObserv)
  
  monthSplit = floor( (2/3) * nMonthObserv )
  monthTrainRows = sample(nMonthObserv,monthSplit)
  monthTrainTemp = MonthObserv[monthTrainRows,]
  monthTestTemp = MonthObserv[-monthTrainRows,]
  
  #rbind test and train dfs to empty dfs
  BankTwoThirdsTrain = rbind(BankTwoThirdsTrain,monthTrainTemp)
  BankOneThirdTest = rbind(BankOneThirdTest,monthTestTemp)
}

for(j in 1:length(train09months))
{
  MonthObserv = BankTrain09[BankTrain09$month == train09months[j],]
  nMonthObserv = nrow(MonthObserv)
  
  monthSplit = floor( (2/3) * nMonthObserv )
  monthTrainRows = sample(nMonthObserv,monthSplit)
  monthTrainTemp = MonthObserv[monthTrainRows,]
  monthTestTemp = MonthObserv[-monthTrainRows,]
  
  #rbind test and train dfs to empty dfs
  BankTwoThirdsTrain = rbind(BankTwoThirdsTrain,monthTrainTemp)
  BankOneThirdTest = rbind(BankOneThirdTest,monthTestTemp)
}

for(j in 1:length(train10months))
{
  MonthObserv = BankTrain10[BankTrain10$month == train10months[j],]
  nMonthObserv = nrow(MonthObserv)
  
  monthSplit = floor( (2/3) * nMonthObserv )
  monthTrainRows = sample(nMonthObserv,monthSplit)
  monthTrainTemp = MonthObserv[monthTrainRows,]
  monthTestTemp = MonthObserv[-monthTrainRows,]
  
  #rbind test and train dfs to empty dfs
  BankTwoThirdsTrain = rbind(BankTwoThirdsTrain,monthTrainTemp)
  BankOneThirdTest = rbind(BankOneThirdTest,monthTestTemp)
}

write.csv(BankTwoThirdsTrain,file="BankTwoThirdsTrain.csv")
write.csv(BankOneThirdTest,file="BankOneThirdTest.csv")