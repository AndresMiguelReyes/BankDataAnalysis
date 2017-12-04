options(warn=-1)
#rm(list=ls())

BankTwoThirdsTrain = read.csv("BankTwoThirdsTrain.csv")
BankOneThirdTest = read.csv("BankOneThirdTest.csv")
# BankTrainPermanent = read.csv("BankTrainPermanent.csv")
# BankTestPermanent = read.csv("BankTestPermanent.csv")
BankTrainPermanent$month = ordered(BankTrainPermanent$month, levels=c("mar","apr","may","jun","jul","aug","sep","oct","nov","dec"))

# this stupid X thing keeps showing up in my data, not in documentation
# probably catches a ride somewhere in pca
if("X" %in% colnames(BankTwoThirdsTrain))
{
  XLoc = which(colnames(BankTwoThirdsTrain) == "X")
  BankTwoThirdsTrain = BankTwoThirdsTrain[,-XLoc]
}

#### plots ####
Bank08 = BankTrainPermanent[BankTrainPermanent$year == 2008,]
Bank08yes = Bank08[Bank08$y == "yes",]
barTable08 = table(outcome=Bank08yes[,"y"],Bank08yes[,"month"])
barplot(barTable08,xlab="months",ylab="customers who bought term deposit",main="Buyers Over Months 2008 Campaign")

Bank09 = BankTrainPermanent[BankTrainPermanent$year == 2009,]
Bank09yes = Bank09[Bank09$y == "yes",]
barTable09 = table(outcome=Bank09yes[,"y"],Bank09yes[,"month"])
barplot(barTable09,xlab="months",ylab="customers who bought term deposit",main="Buyers Over Months 2009 Campaign")
#print(levels(BankTrainPermanent[,"month"]))
Bank10 = BankTestPermanent
Bank10yes = Bank10[Bank10$y == "yes",]
barTable10 = table(outcome=Bank10yes[,"y"],Bank10yes[,"month"])
barplot(barTable10,xlab="months",ylab="customers who bought term deposit",main="Buyers Over Months 2010 Campaign")

Bankyes = rbind(Bank08yes,Bank09yes)
Bankyes = rbind(Bankyes,Bank10yes)
barTableEuri = table(outcome=Bankyes[,"y"],Bankyes[,"euribor3m"])
barplot(barTableEuri,xlab="euribor3m",ylab="customers who bought term deposit",main="euribor3m vs. Buyers")
barTableEuri = table(outcome=Bankyes[,"y"],Bankyes[,"cons.price.idx"])
barplot(barTableEuri,xlab="cons.price.idx",ylab="customers who bought term deposit",main="Consumer Price Index vs. Buyers")
#barplot(barTable09,xlab="months",ylab="customers who bought term deposit",main="Buyers Over Months 2009 Campaign")
plot(Bankyes$euribor3m,Bankyes$duration)
duration.lm = lm(duration~euribor3m,data=BankTrainPermanent)
print(summary(duration.lm))


# pdf 344 partial influence plot 
# boosting