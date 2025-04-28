## Marketing Modeling ####

# STEP 1 - Librerie ####
library(tidyverse)
# library(unbalanced) non funziona su questa versione di R
library(ROSE) # per sostituire unbalanced
library(caret)
library(C50)

# STEP 2 - Working directory ####
getwd() # Controllare in quale directory ci si trova prima di accedere qualsiasi file
setwd("/Users/carlovilloresi/Documents/Coding/R/Projects/1. Customer Churn Classification ")

# STEP 3 - Caricare il dataset pulito e pronto per la fase di Modeling ####
customer_tbl <- readRDS(df, file = "customer_tbl.RData")

# STEP 4 - Partizione del dataset con tecnica K-Fold Cross Validation ####
set.seed(2017)

intrain <- createDataPartition(customer_tbl$var_target, p = 0.7, list = FALSE)

training <- customer_tbl[intrain,]
testing <- customer_tbl[-intrain,]

nrow(customer_tbl)
nrow(training)
nrow(testing)

# STEP 5 - Distribuione della variabile target ####
training %>% count(var_target) %>% mutate(prop = round(n / sum(n)*100,1))
testing %>% count(var_target) %>% mutate(prop = round(n / sum(n)*100,1))

# STEP 6 - Bilanciamento del dataset di training ####
neg_perc <- 0.8

positive <- training %>% filter(var_target == 1)
negative <- training %>% filter(var_target == 0)

pos <- nrow(positive)
neg <- as.integer((pos * neg_perc)/(1 - neg_perc))

training_balance <- negative %>% 
    slice_sample(n = neg) %>% 
    bind_rows(positive)

training_balance %>% 
    count(var_target) %>% 
    mutate(prop = round(n / sum(n)*100,1))

# STEP 7 - Addestramento del modello con dataset non bilanciato ####
tr_x <- training %>% select(-Id_Cliente, -var_target)

model_unb <- C5.0(x = tr_x, y = as.factor(training$var_target))

names(model_unb)
summary(model_unb)

# STEP 8 - Valutazione del modello non bilanciato ####
prob_unb_predict <- predict(object = model_unb, 
                            newdata = testing %>% select(-Id_Cliente, -var_target), 
                            type = "prob")

prob_unb_predict <- data.frame(prob_unb_predict)

head(prob_unb_predict)

target <- testing$var_target

soglia <- 0.5

result_unb <- bind_cols(prob_unb_predict$X1, target) %>% 
    rename(
        score = ...1,
        target = ...2
    ) %>% 
    mutate(prediction = ifelse(score >= soglia,1,0))

result_unb

    # Matrice di confusione
    true_positive_unb <- sum(ifelse(result_unb$target == 1 & result_unb$prediction == 1,1,0))
    
    positive_unb <- sum(result_unb$target)
    
    false_positive_unb <- sum(ifelse(result_unb$target == 0 & result_unb$prediction == 1,1,0))
    
    precision_unb <- (true_positive_unb/(true_positive_unb+false_positive_unb))*100
    
    recall_unb <- (true_positive_unb/positive_unb)*100
    
    f_unb <- 2*((precision_unb*recall_unb)/(precision_unb+recall_unb))
    
    data.frame(true_positive_unb, positive_unb, false_positive_unb, precision_unb, recall_unb, f_unb)

# STEP 9 - Addestramento del modello con dataset bilanciato ####
tr_x_bal <- training_balance %>% select(-Id_Cliente, -var_target)
    
model_b <- C5.0(x = tr_x_bal, y = as.factor(training_balance$var_target))
    
names(model_b)
    
summary(model_b)

# STEP 10 - Valutazione del modello bilanciato ####
prob_b_predict <- predict(object = model_b, 
                          newdata = testing %>% select(-Id_Cliente, -var_target), 
                          type = "prob")

prob_b_predict <- data.frame(prob_b_predict)

target <- testing$var_target
soglia <- 0.5

result_b <- bind_cols(target, prob_b_predict$X1) %>% 
    rename(
        target = ...1,
        score = ...2
    )

view(result_b)

result_b <- result_b %>% 
    mutate(prediction = ifelse(score >= soglia,1,0))

view(result_b)

    # Matrice di confusione
    true_positive_b <- sum(ifelse(result_b$target == 1 & result_b$prediction == 1,1,0))
    
    positive_b <- sum(result_b$target)
    
    false_positive_b <- sum(ifelse(result_b$target == 0 & result_b$prediction == 1,1,0))
    
    precision_b <- (true_positive_b/(true_positive_b+false_positive_b))*100
    
    recall_b <- (true_positive_b/positive_b)*100
    
    f_b <- 2*((precision_b*recall_b)/(precision_b+recall_b))
    
    data.frame(true_positive_b, positive_b, false_positive_b, precision_b, recall_b, f_b)

    table(result_b$prediction,result_b$target)

# STEP 11 - BOOSTING ####
model_boost <- C5.0(x = tr_x_bal, y = as.factor(training_balance$var_target), trials = 10)
    
summary(model_boost)

# STEP 12 - Valutazione modello Boosting ####
p_boost_predict <- predict(object = model_boost, 
                           newdata = testing %>% select(-Id_Cliente, -var_target), 
                           type = "prob")

p_boost_predict <- data.frame(p_boost_predict)

result_boost <- bind_cols(target, p_boost_predict$X1) %>% 
    rename(
        target = ...1,
        score = ...2
    ) %>%
    mutate(prediction = ifelse(score >= soglia,1,0))

view(result_boost)

    # Matrice di confusione
    true_positive_boost <- sum(ifelse(result_boost$target == 1 & result_boost$prediction == 1,1,0))
    
    positive_boost <- sum(result_boost$target)
    
    false_positive_boost <- sum(ifelse(result_boost$target == 0 & result_boost$prediction == 1,1,0))
    
    precision_boost <- (true_positive_boost/(true_positive_boost+false_positive_boost))*100
    
    recall_boost <- (true_positive_boost/positive_boost)*100
    
    f_boost <- 2*((precision_boost*recall_boost)/(precision_boost+recall_boost))
    
    data.frame(true_positive_boost, positive_boost, false_positive_boost, precision_boost, recall_boost, f_boost)

    table(result_boost$prediction,result_boost$target)    

# CONCLUSIONI ####
# si procede con la fase di evaluation
    
    
    
    
