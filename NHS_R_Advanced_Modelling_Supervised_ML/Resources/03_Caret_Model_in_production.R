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

# Load original model to be used in estimating new / unseen production values

load("Models/ML_Production_Model.rda")

# Here we will treat the original data as new data coming in to be classified and a probability to be predicted
# to that class
set.seed(123)
prod_data <- dplyr::sample_n(dataset, size = 40) %>% 
  dplyr::select(everything(), -Stranded.label)

# These 40 new patients will be used as our unseen cases to make predictions on - you won't know there
# labels (Long.Waiter or Not.Long.Waiter) as this will be live data flowing through the system

class_predictions <- predict(ensemble, newdata = prod_data, type = "raw")
prob_predictions <- predict(ensemble, newdata = prod_data, type = "prob")

# Bind prediction on to original data to use in decision support tools

prod_data %<>% 
  cbind(class_predictions) 


