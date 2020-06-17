rm(list = objects())
library(tidyverse)
library(caret)
library(caretEnsemble)
library(mlbench)
library(DMwR)
library(klaR)
library(magrittr)
library(RSNNS)
library(randomForest)
library(xgboost)
library(elasticnet)
library(quantregForest)
library(qrnn)

# Read data in 

ED <- read.csv('Data/ED_TTBS_Data.csv') %>% 
  drop_na() 

ED %>%  
  glimpse()

# Set a training control scheme as per classification example
reg_ctrl <- caret::trainControl(method = "cv", number=10)
seed <- 123
metric <- "RMSE"


# Run separate models on dataset - linear and non linear

# Linear regression
set.seed(seed)
lm_mod <- caret::train(
  TTBS_mins ~ ., 
  data = ED, 
  method = "lm", metric = metric, preProc=c("center", "scale"), 
  trControl=reg_ctrl
)
# Elasticnet 
set.seed(seed)
elasticnet_mod <- caret::train(
  TTBS_mins ~ ., 
  data = ED, 
  method = "enet", metric = metric, preProc=c("center", "scale"), 
  trControl=reg_ctrl
)

#Least angle regression
set.seed(seed)
lars_mod <- caret::train(
  TTBS_mins ~ ., 
  data = ED, 
  method = "lars", metric = metric, preProc=c("center", "scale"), 
  trControl=reg_ctrl
)

# Try some non-linear examples

# Elasticnet 
set.seed(seed)
ann_mod <- caret::train(
  TTBS_mins ~ ., 
  data = ED, 
  method = "nnet", metric = metric, preProc=c("center", "scale"), 
  trControl=reg_ctrl
)

# SVM Radial Basis Kernels
set.seed(seed)
svm_mod <- caret::train(
  TTBS_mins ~ ., 
  data = ED, 
  method = "svmRadial", metric = metric, preProc=c("center", "scale"), 
  trControl=reg_ctrl
)

#Regression Tree
rpart_mod <- caret::train(
  TTBS_mins ~ ., 
  data = ED, 
  method = "rpart", metric = metric, preProc=c("center", "scale"), 
  trControl=reg_ctrl
)
#Random regression forest
rf_mod <- caret::train(
  TTBS_mins ~ ., 
  data = ED, 
  method = "rf", metric = metric, preProc=c("center", "scale"), 
  trControl=reg_ctrl
)


results <- resamples(
  list(Linear = lm_mod,
       ElasticNet = elasticnet_mod,
       Least_Angle_Reg = lars_mod,
       Neural_Net = ann_mod,
       SVMRadial = svm_mod,
       RPART = rpart_mod,
       RandomForest = rf_mod)
)


scales <- list(x=list(relation="free"), y=list(relation="free"))
dotplot(results)
bwplot(results, scales=scales)

# Create an ensemble from the regression models


ensemble_function <- function(Y.label, df, k_folds, meta_model_name){
  
  ensemble_control <- caret::trainControl(method="cv", number = k_folds, 
                                   savePredictions = 'final', classProbs = TRUE)
  
  ensemble_alg_list <- c("rf", "qrf")
  ensemble_models <- caretEnsemble::caretList(
    as.formula(paste(Y.label, "~ .")), 
    data=df, trControl=ensemble_control, methodList=ensemble_alg_list
  )
 
  meta_ensemble <- caretEnsemble::caretStack(ensemble_models, method = as.character(meta_model_name))
}

system.time(
  suppressWarnings(meta_ensemble <- ensemble_function("TTBS_mins", ED, k_folds = 10, 
                                   meta_model_name = "svmRadial"))
)


plot(meta_ensemble$ens_model)
meta_ensemble$ens_model$results


# Save meta_ensemble for live production

save(ED, meta_ensemble, file = "Models/Regression_Prod_Model.rda")






