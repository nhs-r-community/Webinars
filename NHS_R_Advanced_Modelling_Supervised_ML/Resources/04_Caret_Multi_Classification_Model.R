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

df <- read.csv("Data/Multiclass_data.csv", header = TRUE)
head(df, 10)
table(df$Outcome)
df$Outcome <- make.names(df$Outcome)
df$Outcome <- as.factor(df$Outcome)
levels(df$Outcome)

# Hold out split on data 

split_idx <- caret::createDataPartition(df$Outcome, 
                                        p = 0.8,
                                        list = FALSE)

validation <- df[-split_idx,]
train <- df
# Summarise dataset

dim(df)
sapply(df, class)


# Summarise class distribution
class_distribution <- function(field){
  per_dist <- prop.table(table(field)) * 100
  cbind(Class_Frequency=table(field), 
        Class_percentage = per_dist) %>% 
    as.data.frame()
}

class_distribution(df$Outcome)

# Run algorithms on multiple classification problem
metric <- "ROC"
train_ctrl <- caret::trainControl(method = "cv", number = 10)


# Two algorithms to compare - use more in theory - 2 selected to optimise run time

set.seed(123)
rf_mod <- caret::train(Outcome ~ ., 
                       data = df, 
                       method = "rf", 
                       metric = metric, 
                       trainControl=train_ctrl)

# Run on K-Nearest neighbours and naive bayes
set.seed(123)
lda_mod <- caret::train(Outcome ~ ., 
                       data = df, 
                       method = "lda", metric = metric, trainControl=train_ctrl)
set.seed(123)
svm_rad_mod <- caret::train(Outcome ~ ., 
                        data = df, 
                        method = "svmRadial", metric = metric, trainControl=train_ctrl)


# Compare the three algorithms using the resamples function

mc_results <- caret::resamples(
  list(Random.Forest = rf_mod,
       Linear.Discriminant.Analysis = lda_mod,
       Support.Vector.Machine.Radial.Basis.Kernel = svm_rad_mod)
)

summary(mc_results)
dotplot(mc_results)
print(rf_mod) # Best fittinssg model - would need improvement - DNN would be best for this

# Esimate the model skill on validation dataset

set.seed(123)
mc_pred <- predict(rf_mod, newdata = validation)
# Create confusion matrix to view this
cm <- caret::confusionMatrix(mc_pred, validation$Outcome)
print(cm)


