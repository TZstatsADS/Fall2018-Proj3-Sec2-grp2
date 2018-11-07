#########################################################
### Train a classification model with training features ###
#########################################################

### Author: Chengliang Tang
### Project 3


train_gbm <- function(feat_train, label_train, feature, par=NULL){
  
  ### Train a Gradient Boosting Model (GBM) using processed features from training images
  
  ### Input: 
  ###  -  features from LR images 
  ###  -  responses from HR images
  ### Output: a list for trained models
  
  ### load libraries
  library("gbm")
  
  ### creat model list
  modelList <- list()
  
  ### Train with gradient boosting model
  if(is.null(par)){
    depth <- 3
  } else {
    depth <- par$depth
  }
  
  
  ### the dimension of response arrat is * x 4 x 3, which requires 12 classifiers
  ### this part can be parallelized
  if(feature == 'default'|feature ==  'canny'|feature == 'diagonal'){
  for (i in 1:12){
    ## calculate column and channel
    c1 <- (i-1) %% 4 + 1
    c2 <- (i-c1) %/% 4 + 1
    featMat <- feat_train[, , c2]
    labMat <- label_train[, c1, c2]
    
    fit_gbm <- gbm.fit(x=featMat, y=labMat,
                       n.trees=200,
                       distribution="gaussian",
                       interaction.depth=depth, 
                       bag.fraction = 0.5,
                       verbose=FALSE)
    best_iter <- gbm.perf(fit_gbm, method="OOB", plot.it = FALSE)
    modelList[[i]] <- list(fit=fit_gbm, iter=best_iter)
  }
    return(modelList)
  }
    else {
    for (i in 1:54){
      featMat <- feat_train[,i]
      labMat <- label_train[,i]
      fit_gbm <- gbm.fit(x=featMat, y=labMat,
                         n.trees=200,
                         distribution="gaussian",
                         interaction.depth=depth, 
                         bag.fraction = 0.5,
                         verbose=FALSE)
      best_iter <- gbm.perf(fit_gbm, method="OOB", plot.it = FALSE)
      modelList[[i]] <- list(fit=fit_gbm, iter=best_iter)
    }
      return(modelList)
    }
}
  



### Here is the function for xgboost need modified
train_xgb<- function(feat_train, label_train, feature, par=NULL){
  
  ### Train a Gradient Boosting Model (GBM) using processed features from training images
  
  ### Input: 
  ###  -  features from LR images 
  ###  -  responses from HR images
  ### Output: a list for trained models
  
  ### load libraries
  # TODO load xgboost package
  library(xgboost)
  #library("gbm")
  
  ### creat model list
  modelList <- list()
  
  ### Train with gradient boosting model
  if(is.null(par)){
    depth <- 3
  } else {
    depth <- par$depth
  }
  range = 0
  if(feature == 'default'){
    range = 12
  }else if (feature == "hog"){
    range = 54
  }else if (feature == "modified"){
    range = 12
  }
  
  ### the dimension of response arrat is * x 4 x 3, which requires 12 classifiers
  ### this part can be parallelized
  for (i in 1:range){
    ## calculate column and channel
    c1 <- (i-1) %% 4 + 1
    c2 <- (i-c1) %/% 4 + 1
    featMat <- dat_train[, , c2]
    labMat <- label_train[, c1, c2]
    fit_gbm <- gbm.fit(x=featMat, y=labMat,
                       n.trees=200,
                       distribution="gaussian",
                       interaction.depth=depth, 
                       bag.fraction = 0.5,
                       verbose=FALSE)
    best_iter <- gbm.perf(fit_gbm, method="OOB", plot.it = FALSE)
    modelList[[i]] <- list(fit=fit_gbm, iter=best_iter)
  }
  
  return(modelList)
}



