
# DIVIDE AND CONQUER
data_0 <- newData[which(newData$isResponse==0),]
data_1 <- newData[which(newData$isResponse==1),]
# SPLIT
split <- 0.7
n <- as.integer(5768*split)
m <- as.integer(93*split)
# TRAIN
train_index_0 <- sample(1:nrow(data_0), n, replace=F)
train_index_1 <- sample(1:nrow(data_1), m, replace=F)
# TEST
test_0 <- data_0[-train_index_0, ]
test_1 <- data_1[-train_index_1, ]
test <- rbind(test_0, test_1)


# UNDERSAMPLE
train_0 <- data_0[train_index_0,]
train_1 <- data_1[train_index_1,]
train <- rbind(train_0, train_1)

# BUILD THE UNWEIGHTED FOREST (MODEL1), USING UNDERSAMPLED DATA
set.seed(rnorm(1))
rforest1 <- randomForest(isResponse ~ .,
                         data=train,
                         ntree=500,
                         mtry=6,
                         replace = FALSE, 
                         strata = as.factor(train$isResponse), 
                         sampsize = c(as.integer(93*split)-1, as.integer(93*split)-1),
                         classwt=c('1'=0.01,'0'=0.99),
                         do.trace=F)


evaluation<-function(rforest, test){
  # TRAIN CONFUSION
  print("train confusion table:")
  print(rforest$confusion)
  
  # PREDICTION
  test$isResponse <- as.factor(test$isResponse)
  pred <- data.frame(predict=predict(rforest, newdata=test[,-2],
                                     type="response", norm.votes=TRUE), actual=test$isResponse)
  pred$isEqual = ifelse(pred$predict==pred$actual, 1, 0)
  accuracy=100*sum(pred$isEqual)/length(pred$isEqual)
  xtab <- table(actual=pred$actual, predict=pred$predict)
  err_rate_0 <- xtab[3]/(xtab[1]+xtab[3])
  err_rate_1 <- xtab[2]/(xtab[2]+xtab[4])
  print("test confusion table:")
  print(xtab)
  print("class.error:")
  print(paste("0:", err_rate_0))
  print(paste("1:", err_rate_1))
}

evaluation(rforest1, test)

# BUILD THE WEIGHTED FOREST (MODEL2), USING UNDERSAMPLED DATA
set.seed(rnorm(1))
rforest2 <- randomForest(isResponse ~ .,
                         data=train,
                         ntree=500,
                         mtry=6,
                         replace = FALSE, 
                         #importance = TRUE,
                         strata = as.factor(train$isResponse), 
                         sampsize = c(as.integer(93*split)-1, as.integer(93*split)-1),
                         classwt=c('1'=0.01,'0'=0.99),
                         do.trace=F)

evaluation(rforest2, test)

# OVERSAMPLE
sample_size <- 5768
train_index_0_over<-sample(train_index_0, sample_size, replace=T)
train_index_1_over<-sample(train_index_1, sample_size, replace=T)
train_0 <- data_0[train_index_0_over,]
train_1 <- data_1[train_index_1_over,]
train <- rbind(train_0, train_1)

# BUILD THE UNWEIGHTED FOREST (MODEL3), USING OVERSAMPLED DATA
set.seed(rnorm(1))
rforest3 <- randomForest(isResponse ~ .,
                         data=train,
                         ntree=500,
                         mtry=6,
                         replace = TRUE, 
                         #importance = TRUE,
                         strata = as.factor(train$isResponse), 
                         sampsize = c((sample_size-1), (sample_size-1)),
                         #classwt=c('1'=0.01,'0'=0.99),
                         do.trace=F)

evaluation(rforest3, test)

# BUILD THE WEIGHTED FOREST (MODEL4), USING OVERSAMPLED DATA
set.seed(rnorm(1))
rforest4 <- randomForest(isResponse ~ .,
                         data=train,
                         ntree=500,
                         mtry=6,
                         replace = TRUE, 
                         #importance = TRUE,
                         strata = as.factor(train$isResponse), 
                         sampsize = c((sample_size-1), (sample_size-1)),
                         classwt=c('1'=0.01,'0'=0.99),
                         do.trace=F)

evaluation(rforest4, test)


# PROBABILITY PREDICTION AND ROC CURVE
train_0 <- data_0[train_index_0,]
train_1 <- data_1[train_index_1,]
train <- rbind(train_0, train_1)

set.seed(rnorm(1))
rforest <- randomForest(isResponse ~ .,
                        data=train,
                        ntree=500,
                        mtry=6,
                        replace = FALSE, 
                        sampsize = c(as.integer(93*split)-1, as.integer(93*split)-1),
                        classwt=c('1'=0.01,'0'=0.99),
                        do.trace=F)
pred_prob <- data.frame(predict=predict(rforest, newdata=test[,-2], type="prob"), actual=test$isResponse)

library(ROCR)
pred <- prediction(pred_prob$predict.1, pred_prob$actual)

perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=T, 
     print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(1.2,1.2), avg="threshold", lwd=3)

table(actual=pred_prob$actual, predict=ifelse(pred_prob$predict.1>0.41, 1, 0))
