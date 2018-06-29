# Turn off warning because R is being a little baby
options(warn=-1)

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
# We can augment metrics to gauge world economy stuff to the data set
# given the month and year, but we don't have that so I'll have to make it

cat("The data set contains ",BankSoloNamesLength," without the augmented data\n")
cat("The data set contains ",BankNamesLength," with the augmented data\n")
cat("The variables that were augmented to the data set are:\n",BankNamesDiff,"\n")

# Attempting to make a year and month column for the data / confirming
# does data's "month LAST contacted" == "the month the customer WAS contacted"?
# if so we can step through unique months and use a 12 month counter to iterate
# years starting @ may 2008 - ???
# I wrote ??? b/c more discrepencies exist in the data
# paper says until Jun 2012, txt given with the data says until Nov 2010
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
BankTrain = Bank[train,]

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

# Does the year of the marketing campaign have any impact on the percentage of ppl buying the term deposits?
# I only have 2 years, I can't really say

# Turn warnings back on
options(warn=0)
