#Script to plot comparison Histos between signal and background
# myimp <- read.csv("important_matrix.csv")
# keepnames <- myimp$Feature
# mynames <- names(cleantrain) %in% keepnames
#newtrain <- cleantrain[,mynames]
newtrain <- cleantrain
sig <- subset(newtrain,newtrain$TARGET==1)
bkg <- subset(newtrain,newtrain$TARGET==0)


sig$ID <- NULL
bkg$ID <- NULL
maxss <- apply(sig, 2, max) 
minss <- apply(sig, 2, min)
#sig <- as.data.frame(scale(sig, center = mins, scale = maxs - mins))
#maxsb <- apply(bkg, 2, max) 
#minsb <- apply(bkg, 2, min)
#bkg <- as.data.frame(scale(sig, center = mins, scale = maxs - mins))
par(mfrow = c(2,2))
for ( i in seq(143,144,1) ) {
#  newsig <- subset(sig,sig[,i]>0.0)
#  newbkg <- subset(bkg,bkg[,i]>0.0)
  newsig <- sig[,i]
  newbkg <- bkg[,i]
  if (maxss[i]==1 & minss[i]==0) {
     print(i,names(sig[,i]))
#    newsig <- scale(sig[,i],scale=T,center=T)
#    newbkg <- scale(bkg[,i],scale=T,center=T)
  }
  else {
    #newsig <- sig[,i]
    #newbkg <- bkg[,i]
  }
  nbr =100
  xname=names(newsig)
  min1=minss[i]
  max1=maxss[i]
  hist(newsig,freq=FALSE,breaks=nbr,main=xname,xlim=c(min1,max1))
  hist(newbkg,freq=FALSE,add=FALSE,col="lightgreen",breaks=nbr,main=xname,xlim=c(min1,max1))
#  hist(sig[,i],freq=FALSE,breaks=50,xlab=names(sig[i]))
#hist(bkg[,i],freq=FALSE,add=TRUE,col="lightgreen",breaks=50)}
}


