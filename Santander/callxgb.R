# set-up xgboost parameters
object = "binary:logistic"
#max.depth=4
max.depth=4
sub.samp=0.7
col.tree=0.85
#col.tree=0.75
eta = 0.03
nstop=10

#if over fitting observed reduce eta and increase nround
param1 <- list(early.stop.round=nstop, objective = object,booster="gbtree",max_depth = max.depth,"eta" = eta,colsample_bytree=col.tree,subsample=sub.samp,"eval_metric"="auc")

# set random seed, for reproducibility 
set.seed(1234)
# k-fold cross validation, with timing
nround.cv = 300
system.time( bst.cv <- xgb.cv(param=param1,data=train.matrix,label=y,nfold=5,nrounds=nround.cv,prediction=TRUE,verbose=TRUE) )

max.idx = which.max(bst.cv$dt[, test.auc.mean]) 
print(max.idx)
print(bst.cv$dt[max.idx,])
min.err = which.min(bst.cv$dt[, test.auc.std])


# real model fit training, with full data 
# Question : How do I optimise the nrounds.
#nr=max.idx
nr = 267
system.time(bst <- xgboost(param=param1,data=train.matrix,label=y,nrounds=nr,verbose=1) )


# xgbgrid = expand.grid(nrounds = 300,eta = c(0.03,0.04,0.06),
#                       max_depth = 4,
#                       gamma = 1,
#                       colsample_bytree=c(0.83,0.85,0.87),
#                       min_child_weight=1
# )
# 
# xgbtrcontrol = trainControl(
#   method = "cv",
#   number = 2,
#   verboseIter = TRUE,
#   returnData = FALSE,
#   returnResamp = "all",                                                        # save losses across all models
#   classProbs = TRUE,                                                           # set to TRUE for AUC to be computed
#   summaryFunction = twoClassSummary,
#   allowParallel = TRUE
# )
# 
# mm = as.factor(mytrain.target)
# levels(mm) <- make.names(levels(mm))
# xgb_train_1 = train(
#   x = train.matrix,
#   y = mm,
#   trControl = xgbtrcontrol,
#   tuneGrid = xgbgrid,
#   method = "xgbTree"
# )
# 
# ggplot(xgb_train_1$results, aes(x = as.factor(eta), y = max_depth, size = ROC, color = ROC)) + 
#   geom_point() + 
#   theme_bw() + 
#   scale_size_continuous(guide = "none")
