#samples = sample[,1:20]
#choose columns 171:190 and the last column which is the target column
#samples=sample[,c(171:190,300)]
#choose all columns except the last column
samples <- sample[,]
correlationMatrix <- cor(samples)
hc <- findCorrelation(correlationMatrix, cutoff=0.9999,verbose=TRUE)
hc <- sort(hc) 
# hc is the row indices of the correlation matrices which has a correlation with some other 
# field. It does not show the one which it has a correlation to. So only one of
# the pairs is shown and can be removed as below.
#newsample = samples[,c(hc[1:21])]
#corp <- cor(newsample[,])
#corrplot(corp)
which(correlationMatrix[,28]>0.99999)