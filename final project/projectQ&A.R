#setwd('./project')
Bank = read.csv("bank-additional-full.csv",header=TRUE,sep=";")
BankNames = length(names(Bank))

BankSolo = read.csv("bank-full.csv",header=TRUE,sep=";")
BankSoloNames = length(names(BankSolo))

cat("Without the augmented economic data: ",BankSoloNames,"\n")
cat("With the augmented economic data: ",BankNames,"\n")

nameDiff = setdiff(names(Bank),names(BankSolo))
cat("The new columns augmented to the data frame are: ",nameDiff)

