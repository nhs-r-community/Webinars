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

df <- read.csv("Data/Stranded_Data.csv", header = TRUE)
head(df, 10)
table(df$Stranded.label)


balanced_stranded <- SMOTE(Stranded.label~., df, perc.over=200, perc.under=120) %>% 
  as.data.frame() %>% 
  drop_na()
table(balanced_stranded$Stranded.label)
any(is.na(balanced_stranded))


balanced_stranded %<>%  drop_na()
scaled <- preProcess(balanced_stranded[,2:ncol(balanced_stranded)], method=c("scale"))
transformed <- predict(scaled, balanced_stranded[,2:ncol(balanced_stranded)])
summary(transformed)



# Bind transformed back to df_min_correlaton

dataset <- cbind(Stranded.label = balanced_stranded$Stranded.label, transformed)
levels(dataset$Stranded.label) <- c("Long.Waiter", "Not.Long.Waiter")
summary(dataset)
train_split_idx <- caret::createDataPartition(dataset$Stranded.label, p = 0.75, list = FALSE)
data_TRAIN <- dataset[train_split_idx, ]
data_TEST <- dataset[-train_split_idx, ]
dim(data_TRAIN)
dim(data_TEST)
eval_metric <- "Accuracy"
set.seed(123) # Random seed to make the results reproducible


nb_mod <- caret::train(Stranded.label ~ .,
                       data = data_TRAIN,
                       method = "nb", 
                       metric = eval_metric)

summary(nb_mod)
print(nb_mod)


# Make prediction on the dataset

nb_predict <- predict(nb_mod, newdata = data_TEST, type = "prob")
nb_class <- predict(nb_mod, newdata = data_TEST, type = "raw")
nb_class
predictions <- cbind(nb_predict, nb_class) 


cm <- caret::confusionMatrix(nb_class, 
                      data_TEST[,names(data_TEST) %in% c("Stranded.label")],
                      positive = "Long.Waiter")
cm

# Visualise confusion matrix

source('Functions/confusion_matrix_plot_function.R')
conf_matrix_plot(
  cm_input = cm,
  class_label1 = "Not Long Waiter",
  class_label2 = "Long Waiter",
)

# There are a number of ways this algorithm could be made more accurate:
#- Increasing sample size 
#- Use a different ML algorithm 
#- Change the resampling method
#- Feature scale differently
#- Bring in more features that are more predictive

# Variable importance

var_imp_plot_GH <- function(model, field) {
  .field <- rlang::enquo(field)
  .field_name <- rlang::quo_name(.field)
  
  imp <-caret::varImp(model)$importance %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column(var="Metric") %>% 
    dplyr::select(Metric, !!.field) %>% 
    dplyr::mutate(Metric = as.factor(Metric))
  
  imp <- imp %>% 
    dplyr::mutate(Reodered= forcats::fct_reorder(Metric, !!.field))
  
  ggplot(data = imp,
         aes(x=Reodered,
             y=!!.field,
             label = !!.field)) +
    geom_point(stat="identity", aes(colour = factor(Reodered)), size = 6) +
    coord_flip() +
    ggthemes::theme_fivethirtyeight() + 
    theme(legend.position = "none") + 
    labs(title="Global variable importance")
}

var_imp_plot_GH(nb_mod, Long.Waiter) 



# Variable importance plots show that BMI and if they are in a care home are 
# significant factors

# Model Selection and different resampling method

#Run a repeated cross validation method to divide partitions in the full dataset selecting
#different training sets at each pass

# Map levels 

seed_val <- 123
set.seed(seed_val)
ctrl <- trainControl(method="repeatedcv", number=10, repeats=3) 
# Create the training control resampling method
set.seed(seed_val)
nb_mod <- caret::train(Stranded.label ~ ., data = dataset, method = "nb", trControl=ctrl)
#RPART
set.seed(seed_val)
rpart_mod <- caret::train(Stranded.label ~ . ,data = dataset, method = "rpart", trControl=ctrl)
#Logistic regression
set.seed(seed_val)
log_mod <- caret::train(Stranded.label ~ .,data = dataset, method = "glm", trControl=ctrl)
  
# Random forest 
set.seed(seed_val)
rf_mod <- caret::train(Stranded.label ~ ., data = dataset, method = "rf", trControl=ctrl)
#Collect the resamples
resample_results <- resamples(list(NB_Fit = nb_mod,
                                     Rpart_Fit = rpart_mod,
                                     Logistic_Fit = log_mod,
                                     Random_Forest = rf_mod))
  
summary(resample_results)
dotplot(resample_results)


# As the random forest model is the highest performer in terms of accuracy we will
# perform different search parameters to try and optimise further

# Random search 
seed_val <- 123
set.seed(seed_val)
ctrl <- trainControl(method="repeatedcv", number=10, repeats=3, search = "random")
#The tuneLength parameter tells the algorithm to try different default values for the main parameter
system.time(
  rf_mod <- caret::train(Stranded.label ~ ., data = dataset, method = "rf", 
                       trControl=ctrl)
  )
print(rf_mod)
rf_results <- as.data.frame(rf_mod$results)
plot(rf_mod)
# Automatic grid search 
seed_val <- 123
set.seed(seed_val)
ctrl <- trainControl(method="repeatedcv", number=10, repeats=3, search = "grid")
system.time(rf_mod <- caret::train(Stranded.label ~ ., data = dataset, method = "rf", 
                                   trControl=ctrl, tunelength = 2))
print(rf_mod)
rf_results <- rbind(rf_results, as.data.frame(rf_mod$results))
plot(rf_mod)
# Other approaches would be custom search algorithms and undertaking manual grid searches. However, the automatic 
# searching in caret is excellent at doing the hyperparameter tuning for you

# Ensembling - Stacking models

# Set factor levels

tr_ctrl <- trainControl(method="cv", number =10, savePredictions = "final", classProbs = TRUE)
algo_list <- c("nb", 'lda', 'knn', 'glm', 'rf', 'rpart')
system.time(
  stacked_models <- caretList(
  factor(Stranded.label) ~ ., data = dataset,
  trControl = tr_ctrl, methodList = algo_list)
  )
system.time(
  ensemble <- caretStack(stacked_models, 
                       method = "rf")
)

ensemble
ensemble$ens_model$results
ensemble$ens_model$bestTune

# Using the model - making predictions based on ensemble model

# Create test dataset

production_data <- dataset[sample(dataset$Stranded.label, 60),2:ncol(dataset)]
# Creates a sample of 60 records from the original dataset to be used and drops the label column, 
# as the aim is to estimate the label - class that patient belongs to
prob_pred <- predict(ensemble, production_data, type = "prob")
class_pred <- predict(ensemble, production_data, type = "raw")
predictions <- cbind(prob_pred, class_pred)
# Bind the predictions to the production data
production_data %<>% 
  cbind(predictions)

# Save the ensemble model to make predictions of in the future
save(ensemble, dataset, df, file = "Models/ML_Production_Model.rda")





