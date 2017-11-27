#################################################################
##Modeling for GOALKEEPERS: Modeling of youth data (FIFA-2007) for experienced players of FIFA 2018
#################################################################

#******************************************
#CALIBRATING R & IMPORTING PACKAGES
#******************************************
#*******Getting and loading relevant packages*******
library(DMwR)         #for SMOTE - converting unbalanced to balanced data
library(caret)        #for evaluation of classification metrics here
library(ROCR)         #for ROC plots
library(rpart)        #for decision tree models
library(rpart.plot)   #for decsion tree model plots
library(randomForest) #for random forest models
library(adabag)       #for boosting models

#*******************************
#Modeling BUILDING - for GKP
#*******************************


#*******Calibration for my general code: 
#Renaming our original training data set to "Data" for convenience
Base_Data<-fifa_07_ADS_yng_18_exp_gkp
Base_Data$Target<-as.factor(Base_Data$good_flag)
Base_Data$good_flag<-NULL

#Distribution of good vs avg players
#Note: It's a highly unbalanced data (so, will be using SMOTE on train data after partition, and not in test data)
good_avg_dist<-table(Base_Data$Target)
good_avg_pct_dist<-prop.table(table(Base_Data$Target))
good_avg_dist


#**********Using Hold-out validation method for model building and comparison (naive right now but will use nested CV during final report)
#Stratified sampling into 50% training data and 50% test data
set.seed(1)
k <- 5
nmethod<-3
cvIndex <- createFolds(factor(Base_Data$Target), k, returnTrain = T)
models.err.gkp <- matrix(-1,k,nmethod, dimnames=list(paste0("Fold", 1:k), c("rpart","rf","adaboost")))

for(i in 1:k){ 
  
  trainData<-Base_Data[cvIndex[[i]],]
  testData<-Base_Data[-cvIndex[[i]],]
  
  
  #*******Model data treatment: Balancing the unbalanced data (only balancing train and not test)
  #SMOTE on train data: Removing variables which are doubtful or not important business wie or statitistically note logical
  set.seed(1)
  trainData<- SMOTE(Target ~ .,
                    data=subset(trainData,select=c(-ID,-name,-club,-eur_value,-eur_wage,-overall,-potential,-release_clause_eur,-international_reputation,-body_type,-league,-position,-gk)), 
                    perc.over = 2000,perc.under=100, k=5)
  
  #Ensuring variables of testData has same format as trainData (might have changed after SMOTE)
  logic_cols <- sapply(testData, is.logical)
  testData[,logic_cols] <- lapply(testData[,logic_cols], as.numeric)
  
  
  #*********Model building (3 different models - Trees, Random Forest, Adaboost)
  
  #------------------------------------------------------------
  #*****1. RPART*******
  #Note: Using built-in CV for repart to get optimal hyperparameter value (cp here)
  #------------------------------------------------------------
  library(rpart)
  
  #---Making decision tree model (fully grown on training data)
  DT_rpart <- rpart(Target~., 
                    trainData,                   
                    method="class", 
                    parms=list(split="gini"),
                    control=rpart.control(minbucket=1,cp=-1))
  
  
  #---Optimizing through Pruning using optimal CP (checking cptable for minimum "xerror" based on internal CV)
  cp_optimal<-DT_rpart$cptable[DT_rpart$cptable[,"xerror"]==min(DT_rpart$cptable[,"xerror"]),"CP"]
  DT_rpart <- prune(DT_rpart,cp=cp_optimal[1])
  
  #---Predicting test based on trained model tree (pruned)
  DT_rpart.pred <- predict(DT_rpart, newdata = testData, type = "class")
  DT_rpart.pred.P <- predict(DT_rpart, newdata = testData, type = "prob")
  
  #---Outputs (tree plot, confusion matrix, ROC)
  DT_rpart.plot.gkp<-rpart.plot(DT_rpart)
  
  DT_rpart.cf.gkp<-confusionMatrix(DT_rpart.pred,testData$Target,positive="1")
  
  myROC<-function(prob_plus_only,test_labels, my_color){
    pred=prediction(prob_plus_only,test_labels)
    perf=performance(pred,"tpr","fpr")
    plot(perf,col=my_color)
    abline(a= 0, b=1)
  }
  DT_rpart.ROC.gkp<-myROC(DT_rpart.pred.P[,2],testData$Target,"red")
  
  #---Evaluation of tree using trained model on test
  models.err.gkp[i,"rpart"] <- mean(testData$Target != DT_rpart.pred)
  
  
  #-------------------------------------------------------------------------------------
  #*******2. RANDOM FOREST*********
  #Note: Using thumb rule of mtry=sqrt(no. of predictors) for classfication as optimal hyperparameter value, whereas ntree stabilibises beyind 100
  #-------------------------------------------------------------------------------------
  library(randomForest)
  
  #---Making optimal rf (using thumb rule pertaining mtry for classification problem)
  rf <- randomForest(Target~., 
                     trainData,
                     ntree = 500, mtry = sqrt(ncol(trainData)-1))
  
  #---Predicting test based on trained optimal rf
  rf.pred <- predict(rf, newdata = testData, type = "class")
  rf.pred.P <- predict(rf, newdata = testData, type = "prob")
  
  #---Outputs (error plot, varImp, confusion matrix, ROC)
  rf.plot.gkp<-plot(rf)
  
  rf.varImp.gkp<-varImpPlot(rf)
  
  rf.cf.gkp<-confusionMatrix(rf.pred,testData$Target,positive="1")
  
  myROC<-function(prob_plus_only,test_labels, my_color){
    pred=prediction(prob_plus_only,test_labels)
    perf=performance(pred,"tpr","fpr")
    plot(perf,col=my_color)
    abline(a= 0, b=1)
  }
  rf.ROC.gkp<-myROC(rf.pred.P[,2],testData$Target,"orange")
  
  #---Evaluation of rf using trained model on test
  models.err.gkp[i,"rf"] <- mean(testData$Target != rf.pred)
  
  
  #-------------------------------------------------------------------------------------
  #*******3. ADABOOST*********
  #Note: Using weak learners enseamble, hence using maxdepth=1 germane to weak trees 
  #-------------------------------------------------------------------------------------
  library(adabag)
  
  #---Making adaboost model (using weak learner such that maxdepth=1)
  adaboost <- boosting(Target~., 
                       trainData,
                       coeflearn = "Breiman", mfinal = 100,
                       control=rpart.control(maxdepth = 1))
  
  #---Predicting test based on trained optimal rf
  adaboost.pred <- predict.boosting(adaboost, newdata = testData)$class
  adaboost.pred.P <- predict.boosting(adaboost, newdata = testData)$prob
  
  #---Outputs (first weak learner, varImp, confusion matrix, ROC)
  adaboost.plot.gkp<-rpart.plot(adaboost$trees[[1]])
  
  adaboost.varImp.gkp<-as.data.frame(adaboost$importance)                  
  
  adaboost.cf.gkp<-confusionMatrix(adaboost.pred,testData$Target,positive="1")
  
  myROC<-function(prob_plus_only,test_labels, my_color){
    pred=prediction(prob_plus_only,test_labels)
    perf=performance(pred,"tpr","fpr")
    plot(perf,col=my_color)
    abline(a= 0, b=1)
  }
  adaboost.ROC.gkp<-myROC(adaboost.pred.P[,2],testData$Target,"purple")
  
  #---Evaluation of adaboost using trained model on test
  models.err.gkp[i,"adaboost"] <- mean(testData$Target != adaboost.pred)
  
}

models.err.gkp
colMeans(models.err.gkp)


#################################################################
##Modeling for DEFENDERS: Modeling of youth data (FIFA-2007) for experienced players of FIFA 2018
#################################################################

#******************************************
#CALIBRATING R & IMPORTING PACKAGES
#******************************************
#*******Getting and loading relevant packages*******
library(DMwR)         #for SMOTE - converting unbalanced to balanced data
library(caret)        #for evaluation of classification metrics here
library(ROCR)         #for ROC plots
library(rpart)        #for decision tree models
library(rpart.plot)   #for decsion tree model plots
library(randomForest) #for random forest models
library(adabag)       #for boosting models

#*******************************
#Modeling BUILDING - for DEF
#*******************************


#*******Calibration for my general code: 
#Renaming our original training data set to "Data" for convenience
Base_Data<-fifa_07_ADS_yng_18_exp_def
Base_Data$Target<-as.factor(Base_Data$good_flag)
Base_Data$good_flag<-NULL

#Distribution of good vs avg players
#Note: It's a highly unbalanced data (so, will be using SMOTE on train data after partition, and not in test data)
good_avg_dist<-table(Base_Data$Target)
good_avg_pct_dist<-prop.table(table(Base_Data$Target))
good_avg_dist


#**********Using Hold-out validation method for model building and comparison (naive right now but will use nested CV during final report)
#Stratified sampling into 50% training data and 50% test data
set.seed(1)
k <- 5
nmethod<-3
cvIndex <- createFolds(factor(Base_Data$Target), k, returnTrain = T)
models.err.def <- matrix(-1,k,nmethod, dimnames=list(paste0("Fold", 1:k), c("rpart","rf","adaboost")))

for(i in 1:k){ 
  
  trainData<-Base_Data[cvIndex[[i]],]
  testData<-Base_Data[-cvIndex[[i]],]
  
  
  #*******Model data treatment: Balancing the unbalanced data (only balancing train and not test)
  #SMOTE on train data: Removing variables which are doubtful or not important business wie or statitistically note logical
  set.seed(1)
  trainData<- SMOTE(Target ~ .,
                    data=subset(trainData,select=c(-ID,-name,-club,-eur_value,-eur_wage,-overall,-potential,-release_clause_eur,-international_reputation,-body_type,-league,-position,-gk)), 
                    perc.over = 2000,perc.under=100, k=5)
  
  #Ensuring variables of testData has same format as trainData (might have changed after SMOTE)
  logic_cols <- sapply(testData, is.logical)
  testData[,logic_cols] <- lapply(testData[,logic_cols], as.numeric)
  
  
  #*********Model building (3 different models - Trees, Random Forest, Adaboost)
  
  #------------------------------------------------------------
  #*****1. RPART*******
  #Note: Using built-in CV for repart to get optimal hyperparameter value (cp here)
  #------------------------------------------------------------
  library(rpart)
  
  #---Making decision tree model (fully grown on training data)
  DT_rpart <- rpart(Target~., 
                    trainData,                   
                    method="class", 
                    parms=list(split="gini"),
                    control=rpart.control(minbucket=1,cp=-1))
  
  
  #---Optimizing through Pruning using optimal CP (checking cptable for minimum "xerror" based on internal CV)
  cp_optimal<-DT_rpart$cptable[DT_rpart$cptable[,"xerror"]==min(DT_rpart$cptable[,"xerror"]),"CP"]
  DT_rpart <- prune(DT_rpart,cp=cp_optimal[1])
  
  #---Predicting test based on trained model tree (pruned)
  DT_rpart.pred <- predict(DT_rpart, newdata = testData, type = "class")
  DT_rpart.pred.P <- predict(DT_rpart, newdata = testData, type = "prob")
  
  #---Outputs (tree plot, confusion matrix, ROC)
  DT_rpart.plot.def<-rpart.plot(DT_rpart)
  
  DT_rpart.cf.def<-confusionMatrix(DT_rpart.pred,testData$Target,positive="1")
  
  myROC<-function(prob_plus_only,test_labels, my_color){
    pred=prediction(prob_plus_only,test_labels)
    perf=performance(pred,"tpr","fpr")
    plot(perf,col=my_color)
    abline(a= 0, b=1)
  }
  DT_rpart.ROC.def<-myROC(DT_rpart.pred.P[,2],testData$Target,"red")
  
  #---Evaluation of tree using trained model on test
  models.err.def[i,"rpart"] <- mean(testData$Target != DT_rpart.pred)
  
  
  #-------------------------------------------------------------------------------------
  #*******2. RANDOM FOREST*********
  #Note: Using thumb rule of mtry=sqrt(no. of predictors) for classfication as optimal hyperparameter value, whereas ntree stabilibises beyind 100
  #-------------------------------------------------------------------------------------
  library(randomForest)
  
  #---Making optimal rf (using thumb rule pertaining mtry for classification problem)
  rf <- randomForest(Target~., 
                     trainData,
                     ntree = 500, mtry = sqrt(ncol(trainData)-1))
  
  #---Predicting test based on trained optimal rf
  rf.pred <- predict(rf, newdata = testData, type = "class")
  rf.pred.P <- predict(rf, newdata = testData, type = "prob")
  
  #---Outputs (error plot, varImp, confusion matrix, ROC)
  rf.plot.def<-plot(rf)
  
  rf.varImp.def<-varImpPlot(rf)
  
  rf.cf.def<-confusionMatrix(rf.pred,testData$Target,positive="1")
  
  myROC<-function(prob_plus_only,test_labels, my_color){
    pred=prediction(prob_plus_only,test_labels)
    perf=performance(pred,"tpr","fpr")
    plot(perf,col=my_color)
    abline(a= 0, b=1)
  }
  rf.ROC.def<-myROC(rf.pred.P[,2],testData$Target,"orange")
  
  #---Evaluation of rf using trained model on test
  models.err.def[i,"rf"] <- mean(testData$Target != rf.pred)
  
  
  #-------------------------------------------------------------------------------------
  #*******3. ADABOOST*********
  #Note: Using weak learners enseamble, hence using maxdepth=1 germane to weak trees 
  #-------------------------------------------------------------------------------------
  library(adabag)
  
  #---Making adaboost model (using weak learner such that maxdepth=1)
  adaboost <- boosting(Target~., 
                       trainData,
                       coeflearn = "Breiman", mfinal = 100,
                       control=rpart.control(maxdepth = 1))
  
  #---Predicting test based on trained optimal rf
  adaboost.pred <- predict.boosting(adaboost, newdata = testData)$class
  adaboost.pred.P <- predict.boosting(adaboost, newdata = testData)$prob
  
  #---Outputs (first weak learner, varImp, confusion matrix, ROC)
  adaboost.plot.def<-rpart.plot(adaboost$trees[[1]])
  
  adaboost.varImp.def<-as.data.frame(adaboost$importance)                  
  
  adaboost.cf.def<-confusionMatrix(adaboost.pred,testData$Target,positive="1")
  
  myROC<-function(prob_plus_only,test_labels, my_color){
    pred=prediction(prob_plus_only,test_labels)
    perf=performance(pred,"tpr","fpr")
    plot(perf,col=my_color)
    abline(a= 0, b=1)
  }
  adaboost.ROC.def<-myROC(adaboost.pred.P[,2],testData$Target,"purple")
  
  #---Evaluation of adaboost using trained model on test
  models.err.def[i,"adaboost"] <- mean(testData$Target != adaboost.pred)
  
}

models.err.def
colMeans(models.err.def)

#################################################################
##Modeling for MIDFIELDERS: Modeling of youth data (FIFA-2007) for experienced players of FIFA 2018
#################################################################

#******************************************
#CALIBRATING R & IMPORTING PACKAGES
#******************************************
#*******Getting and loading relevant packages*******
library(DMwR)         #for SMOTE - converting unbalanced to balanced data
library(caret)        #for evaluation of classification metrics here
library(ROCR)         #for ROC plots
library(rpart)        #for decision tree models
library(rpart.plot)   #for decsion tree model plots
library(randomForest) #for random forest models
library(adabag)       #for boosting models

#*******************************
#Modeling BUILDING - for MID
#*******************************


#*******Calibration for my general code: 
#Renaming our original training data set to "Data" for convenience
Base_Data<-fifa_07_ADS_yng_18_exp_mid
Base_Data$Target<-as.factor(Base_Data$good_flag)
Base_Data$good_flag<-NULL

#Distribution of good vs avg players
#Note: It's a highly unbalanced data (so, will be using SMOTE on train data after partition, and not in test data)
good_avg_dist<-table(Base_Data$Target)
good_avg_pct_dist<-prop.table(table(Base_Data$Target))
good_avg_dist


#**********Using Hold-out validation method for model building and comparison (naive right now but will use nested CV during final report)
#Stratified sampling into 50% training data and 50% test data
set.seed(1)
k <- 5
nmethod<-3
cvIndex <- createFolds(factor(Base_Data$Target), k, returnTrain = T)
models.err.mid <- matrix(-1,k,nmethod, dimnames=list(paste0("Fold", 1:k), c("rpart","rf","adaboost")))

for(i in 1:k){ 
  
  trainData<-Base_Data[cvIndex[[i]],]
  testData<-Base_Data[-cvIndex[[i]],]
  
  
  #*******Model data treatment: Balancing the unbalanced data (only balancing train and not test)
  #SMOTE on train data: Removing variables which are doubtful or not important business wie or statitistically note logical
  set.seed(1)
  trainData<- SMOTE(Target ~ .,
                    data=subset(trainData,select=c(-ID,-name,-club,-eur_value,-eur_wage,-overall,-potential,-release_clause_eur,-international_reputation,-body_type,-league,-position,-gk)), 
                    perc.over = 2000,perc.under=100, k=5)
  
  #Ensuring variables of testData has same format as trainData (might have changed after SMOTE)
  logic_cols <- sapply(testData, is.logical)
  testData[,logic_cols] <- lapply(testData[,logic_cols], as.numeric)
  
  
  #*********Model building (3 different models - Trees, Random Forest, Adaboost)
  
  #------------------------------------------------------------
  #*****1. RPART*******
  #Note: Using built-in CV for repart to get optimal hyperparameter value (cp here)
  #------------------------------------------------------------
  library(rpart)
  
  #---Making decision tree model (fully grown on training data)
  DT_rpart <- rpart(Target~., 
                    trainData,                   
                    method="class", 
                    parms=list(split="gini"),
                    control=rpart.control(minbucket=1,cp=-1))
  
  
  #---Optimizing through Pruning using optimal CP (checking cptable for minimum "xerror" based on internal CV)
  cp_optimal<-DT_rpart$cptable[DT_rpart$cptable[,"xerror"]==min(DT_rpart$cptable[,"xerror"]),"CP"]
  DT_rpart <- prune(DT_rpart,cp=cp_optimal[1])
  
  #---Predicting test based on trained model tree (pruned)
  DT_rpart.pred <- predict(DT_rpart, newdata = testData, type = "class")
  DT_rpart.pred.P <- predict(DT_rpart, newdata = testData, type = "prob")
  
  #---Outputs (tree plot, confusion matrix, ROC)
  DT_rpart.plot.mid<-rpart.plot(DT_rpart)
  
  DT_rpart.cf.mid<-confusionMatrix(DT_rpart.pred,testData$Target,positive="1")
  
  myROC<-function(prob_plus_only,test_labels, my_color){
    pred=prediction(prob_plus_only,test_labels)
    perf=performance(pred,"tpr","fpr")
    plot(perf,col=my_color)
    abline(a= 0, b=1)
  }
  DT_rpart.ROC.mid<-myROC(DT_rpart.pred.P[,2],testData$Target,"red")
  
  #---Evaluation of tree using trained model on test
  models.err.mid[i,"rpart"] <- mean(testData$Target != DT_rpart.pred)
  
  
  #-------------------------------------------------------------------------------------
  #*******2. RANDOM FOREST*********
  #Note: Using thumb rule of mtry=sqrt(no. of predictors) for classfication as optimal hyperparameter value, whereas ntree stabilibises beyind 100
  #-------------------------------------------------------------------------------------
  library(randomForest)
  
  #---Making optimal rf (using thumb rule pertaining mtry for classification problem)
  rf <- randomForest(Target~., 
                     trainData,
                     ntree = 500, mtry = sqrt(ncol(trainData)-1))
  
  #---Predicting test based on trained optimal rf
  rf.pred <- predict(rf, newdata = testData, type = "class")
  rf.pred.P <- predict(rf, newdata = testData, type = "prob")
  
  #---Outputs (error plot, varImp, confusion matrix, ROC)
  rf.plot.mid<-plot(rf)
  
  rf.varImp.mid<-varImpPlot(rf)
  
  rf.cf.mid<-confusionMatrix(rf.pred,testData$Target,positive="1")
  
  myROC<-function(prob_plus_only,test_labels, my_color){
    pred=prediction(prob_plus_only,test_labels)
    perf=performance(pred,"tpr","fpr")
    plot(perf,col=my_color)
    abline(a= 0, b=1)
  }
  rf.ROC.mid<-myROC(rf.pred.P[,2],testData$Target,"orange")
  
  #---Evaluation of rf using trained model on test
  models.err.mid[i,"rf"] <- mean(testData$Target != rf.pred)
  
  
  #-------------------------------------------------------------------------------------
  #*******3. ADABOOST*********
  #Note: Using weak learners enseamble, hence using maxdepth=1 germane to weak trees 
  #-------------------------------------------------------------------------------------
  library(adabag)
  
  #---Making adaboost model (using weak learner such that maxdepth=1)
  adaboost <- boosting(Target~., 
                       trainData,
                       coeflearn = "Breiman", mfinal = 100,
                       control=rpart.control(maxdepth = 1))
  
  #---Predicting test based on trained optimal rf
  adaboost.pred <- predict.boosting(adaboost, newdata = testData)$class
  adaboost.pred.P <- predict.boosting(adaboost, newdata = testData)$prob
  
  #---Outputs (first weak learner, varImp, confusion matrix, ROC)
  adaboost.plot.mid<-rpart.plot(adaboost$trees[[1]])
  
  adaboost.varImp.mid<-as.data.frame(adaboost$importance)                  
  
  adaboost.cf.mid<-confusionMatrix(adaboost.pred,testData$Target,positive="1")
  
  myROC<-function(prob_plus_only,test_labels, my_color){
    pred=prediction(prob_plus_only,test_labels)
    perf=performance(pred,"tpr","fpr")
    plot(perf,col=my_color)
    abline(a= 0, b=1)
  }
  adaboost.ROC.mid<-myROC(adaboost.pred.P[,2],testData$Target,"purple")
  
  #---Evaluation of adaboost using trained model on test
  models.err.mid[i,"adaboost"] <- mean(testData$Target != adaboost.pred)
  
}

models.err.mid
colMeans(models.err.mid)

#################################################################
##Modeling for ATTACKERS: Modeling of youth data (FIFA-2007) for experienced players of FIFA 2018
#################################################################

#******************************************
#CALIBRATING R & IMPORTING PACKAGES
#******************************************
#*******Getting and loading relevant packages*******
library(DMwR)         #for SMOTE - converting unbalanced to balanced data
library(caret)        #for evaluation of classification metrics here
library(ROCR)         #for ROC plots
library(rpart)        #for decision tree models
library(rpart.plot)   #for decsion tree model plots
library(randomForest) #for random forest models
library(adabag)       #for boosting models

#*******************************
#Modeling BUILDING - for ATK
#*******************************


#*******Calibration for my general code: 
#Renaming our original training data set to "Data" for convenience
Base_Data<-fifa_07_ADS_yng_18_exp_atk
Base_Data$Target<-as.factor(Base_Data$good_flag)
Base_Data$good_flag<-NULL

#Distribution of good vs avg players
#Note: It's a highly unbalanced data (so, will be using SMOTE on train data after partition, and not in test data)
good_avg_dist<-table(Base_Data$Target)
good_avg_pct_dist<-prop.table(table(Base_Data$Target))
good_avg_dist


#**********Using Hold-out validation method for model building and comparison (naive right now but will use nested CV during final report)
#Stratified sampling into 50% training data and 50% test data
set.seed(1)
k <- 5
nmethod<-3
cvIndex <- createFolds(factor(Base_Data$Target), k, returnTrain = T)
models.err.atk <- matrix(-1,k,nmethod, dimnames=list(paste0("Fold", 1:k), c("rpart","rf","adaboost")))

for(i in 1:k){ 
  
  trainData<-Base_Data[cvIndex[[i]],]
  testData<-Base_Data[-cvIndex[[i]],]
  
  
  #*******Model data treatment: Balancing the unbalanced data (only balancing train and not test)
  #SMOTE on train data: Removing variables which are doubtful or not important business wie or statitistically note logical
  set.seed(1)
  trainData<- SMOTE(Target ~ .,
                    data=subset(trainData,select=c(-ID,-name,-club,-eur_value,-eur_wage,-overall,-potential,-release_clause_eur,-international_reputation,-body_type,-league,-position,-gk)), 
                    perc.over = 2000,perc.under=100, k=5)
  
  #Ensuring variables of testData has same format as trainData (might have changed after SMOTE)
  logic_cols <- sapply(testData, is.logical)
  testData[,logic_cols] <- lapply(testData[,logic_cols], as.numeric)
  
  
  #*********Model building (3 different models - Trees, Random Forest, Adaboost)
  
  #------------------------------------------------------------
  #*****1. RPART*******
  #Note: Using built-in CV for repart to get optimal hyperparameter value (cp here)
  #------------------------------------------------------------
  library(rpart)
  
  #---Making decision tree model (fully grown on training data)
  DT_rpart <- rpart(Target~., 
                    trainData,                   
                    method="class", 
                    parms=list(split="gini"),
                    control=rpart.control(minbucket=1,cp=-1))
  
  
  #---Optimizing through Pruning using optimal CP (checking cptable for minimum "xerror" based on internal CV)
  cp_optimal<-DT_rpart$cptable[DT_rpart$cptable[,"xerror"]==min(DT_rpart$cptable[,"xerror"]),"CP"]
  DT_rpart <- prune(DT_rpart,cp=cp_optimal[1])
  
  #---Predicting test based on trained model tree (pruned)
  DT_rpart.pred <- predict(DT_rpart, newdata = testData, type = "class")
  DT_rpart.pred.P <- predict(DT_rpart, newdata = testData, type = "prob")
  
  #---Outputs (tree plot, confusion matrix, ROC)
  DT_rpart.plot.atk<-rpart.plot(DT_rpart)
  
  DT_rpart.cf.atk<-confusionMatrix(DT_rpart.pred,testData$Target,positive="1")
  
  myROC<-function(prob_plus_only,test_labels, my_color){
    pred=prediction(prob_plus_only,test_labels)
    perf=performance(pred,"tpr","fpr")
    plot(perf,col=my_color)
    abline(a= 0, b=1)
  }
  DT_rpart.ROC.atk<-myROC(DT_rpart.pred.P[,2],testData$Target,"red")
  
  #---Evaluation of tree using trained model on test
  models.err.atk[i,"rpart"] <- mean(testData$Target != DT_rpart.pred)
  
  
  #-------------------------------------------------------------------------------------
  #*******2. RANDOM FOREST*********
  #Note: Using thumb rule of mtry=sqrt(no. of predictors) for classfication as optimal hyperparameter value, whereas ntree stabilibises beyind 100
  #-------------------------------------------------------------------------------------
  library(randomForest)
  
  #---Making optimal rf (using thumb rule pertaining mtry for classification problem)
  rf <- randomForest(Target~., 
                     trainData,
                     ntree = 500, mtry = sqrt(ncol(trainData)-1))
  
  #---Predicting test based on trained optimal rf
  rf.pred <- predict(rf, newdata = testData, type = "class")
  rf.pred.P <- predict(rf, newdata = testData, type = "prob")
  
  #---Outputs (error plot, varImp, confusion matrix, ROC)
  rf.plot.atk<-plot(rf)
  
  rf.varImp.atk<-varImpPlot(rf)
  
  rf.cf.atk<-confusionMatrix(rf.pred,testData$Target,positive="1")
  
  myROC<-function(prob_plus_only,test_labels, my_color){
    pred=prediction(prob_plus_only,test_labels)
    perf=performance(pred,"tpr","fpr")
    plot(perf,col=my_color)
    abline(a= 0, b=1)
  }
  rf.ROC.atk<-myROC(rf.pred.P[,2],testData$Target,"orange")
  
  #---Evaluation of rf using trained model on test
  models.err.atk[i,"rf"] <- mean(testData$Target != rf.pred)
  
  
  #-------------------------------------------------------------------------------------
  #*******3. ADABOOST*********
  #Note: Using weak learners enseamble, hence using maxdepth=1 germane to weak trees 
  #-------------------------------------------------------------------------------------
  library(adabag)
  
  #---Making adaboost model (using weak learner such that maxdepth=1)
  adaboost <- boosting(Target~., 
                       trainData,
                       coeflearn = "Breiman", mfinal = 100,
                       control=rpart.control(maxdepth = 1))
  
  #---Predicting test based on trained optimal rf
  adaboost.pred <- predict.boosting(adaboost, newdata = testData)$class
  adaboost.pred.P <- predict.boosting(adaboost, newdata = testData)$prob
  
  #---Outputs (first weak learner, varImp, confusion matrix, ROC)
  adaboost.plot.atk<-rpart.plot(adaboost$trees[[1]])
  
  adaboost.varImp.atk<-as.data.frame(adaboost$importance)                  
  
  adaboost.cf.atk<-confusionMatrix(adaboost.pred,testData$Target,positive="1")
  
  myROC<-function(prob_plus_only,test_labels, my_color){
    pred=prediction(prob_plus_only,test_labels)
    perf=performance(pred,"tpr","fpr")
    plot(perf,col=my_color)
    abline(a= 0, b=1)
  }
  adaboost.ROC.atk<-myROC(adaboost.pred.P[,2],testData$Target,"purple")
  
  #---Evaluation of adaboost using trained model on test
  models.err.atk[i,"adaboost"] <- mean(testData$Target != adaboost.pred)
  
}

models.err.atk
colMeans(models.err.atk)


