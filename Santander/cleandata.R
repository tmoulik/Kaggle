require(caret)
require(corrplot)
require(Rtsne)
require(xgboost)
require(stats)
require(knitr)
require(ggplot2)
require(dplyr)

mytrain <- train
mytest <- test

dim(train)
dim(test)


mytrain2 <- mytrain
dim(mytrain2)

mysig1 <- subset(mytrain2,mytrain2$TARGET==1)
mybkg1 <- subset(mytrain2,mytrain2$TARGET==0)
 
# #check which columns have zero or very little data for signal and background
# #and reject them
# # Find out how many elements are non-zero and add them
nbkg <- colSums(mybkg1!=0)/nrow(mybkg1)*100
nsig <- colSums(mysig1!=0)/nrow(mysig1)*100
cbind(sort((nsig)))
cbind(sort((nbkg)))
namesb <- names(subset(nbkg,nbkg<5.0 & names(nbkg)!="TARGET"))
namess <- names(subset(nsig,nsig==0))

mytrain2[,namess] <- list(NULL)

# 
mysig2 <- subset(mytrain2,mytrain2$TARGET==1)
mybkg2 <- subset(mytrain2,mytrain2$TARGET==0)
# 

zero.var = nearZeroVar(mytrain2, saveMetrics=TRUE)
# nzvv <- subset(zero.var,(zeroVar==TRUE | freqRatio==0 & percentUnique<0.002) & rownames(zero.var)!="TARGET")
nzvv <- subset(zero.var,(zeroVar==TRUE))
zero.var[order(zero.var[,"percentUnique"]),]
remnames <- rownames(nzvv)
mytrain2[,remnames] <- list(NULL)
dim(mytrain2)

#Remove Identical columns which have identical values for background and signal
print("removing columns with identical values of signal and background")
mytrain4 <- mytrain2
sampuniq <- subset(zero.var,percentUnique<0.009)
namesuniq <- rownames(sampuniq)
samp <- subset(mytrain2,,select=c(names(mytrain2)%in%namesuniq))
sampbkg <- subset(samp,samp$TARGET==0)
sampsig <- subset(samp,samp$TARGET==1)
nc <- ncol(sampsig)-1
mat<- matrix(list(), ncol=1, nrow=26)
newnames <- c()
nummat <- 1
for (i in seq(1,nc,1)) {
cs <- unique(sampsig[,i])
cb <- unique(sampbkg[,i]) 
cs <- sort(cs)
cb <- sort(cb)
if (identical(cs,cb)==TRUE) {
print(namesuniq[i])
mytrain4[,namesuniq[i]] <- NULL
mytest[,namesuniq[i]] <- NULL
}
}

print("Removing correlated columns")
mytrain2 <- mytrain4
sample <- mytrain2
source('findcor.R')
corrnames <- colnames(sample[,hc])
mynames <- names(mytrain2) %in% corrnames
mytrain2[,c(mynames)] <- list(NULL)

dim(mytrain2)

#Cleaned Up sample
cleantrain <- mytrain2

print("Cleaning test sample")
#clean test sample
cleantest <- mytest
cleantest[,remnames] <- list(NULL)
cleantest[,namess] <- list(NULL)
cleantest[,c(mynames)] <- list(NULL)

print(dim(cleantest))
print(dim(cleantrain))

