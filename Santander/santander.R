# Remove ID and TARGET column from Training Data
mytrain.target <- cleantrain$TARGET
mytrain.ID <- cleantrain$ID
cleantrain$TARGET = NULL
cleantrain$ID = NULL
# Get the final number of rows and columns
dim(cleantrain)


# Remove the ID column of the test data
cleantest.id <- cleantest$ID
cleantest$ID <- NULL
dim(cleantest)

train.matrix = as.matrix(cleantrain)
mode(train.matrix) = "numeric"
test.matrix = as.matrix(cleantest)
mode(test.matrix) = "numeric"


# assign the y variable to the target column
y=as.matrix(mytrain.target)

source("callxgb.R")

# Get the feature importance matrix and plot it.
names <- dimnames(train.matrix)[[2]]
importance_matrix <- xgb.importance(names, model = bst)
# write.csv(importance_matrix,"importance_matrix.csv",row.names=FALSE)

# #keep the first 30 features ..and retrain
# keepnames <- importance_matrix$Feature[1:40]
# mynames <- names(cleantest) %in% keepnames
# test.matrix = as.matrix(cleantest[,mynames])
# mynames <- names(cleantrain) %in% keepnames
# train.matrix = as.matrix(cleantrain[,mynames])
# source("callxgb.R")

# predict test data using the trained model
pred <- predict(bst, test.matrix)  
predtrain <- predict(bst,train.matrix)
head(pred, 10)  
hist(pred,breaks=50,xlim=c(0.0,0.5))
dev.copy(png,"mypred.png")
dev.off()

AUC<-function(actual,predicted)
{
  library(pROC)
  auc<-auc(as.numeric(actual),as.numeric(predicted))
  auc 
}
#AUC(mytrain.target,predtrain)
prediction <- as.numeric(predtrain > 0.05)
confusionMatrix(prediction,mytrain.target)


#Loop over the rows of cleantest and the selected columns of data
# to find if its a sure background event or not. If found then exit
# the loop and put pred=0
newtest <- subset(cleantest,,select=c(names(cleantest) %in% namesuniq))
for (j in seq(1,ncol(newtest),1)) {
  getname <- names(newtest)[j]
  cb <- unique(sampbkg[,getname])
  cs <- unique(sampsig[,getname])
  print(getname)
#  print(cb)
#  print(cs)
  for (i in seq(1,nrow(newtest),1)) {
      if (newtest[i,getname]%in%cb & !(newtest[i,getname]%in%cs)) {
         pred[i]=0       
      }
      if (newtest[i,getname]%in%cs & !(newtest[i,getname]%in%cb)) {
       pred[i]=1
      }
  }
}

final <- data.frame(ID = cleantest.id, TARGET = pred)
write.csv(final,"submission.csv",row.names=FALSE)

