# import dei pacchetti 
library(tidyverse)
library(fairness)
# carico il dataset 
data('compas')
# ispezione
dim(compas)
str(compas)

# ADDESTRIAMO DUE MODELLI DI CLASSIFICAZIONE
# Modello che utilizza tutte le features in input
# Modello che utilizza tutte le features tranne 'ethnicity' in input

# tolgo due variabili che non servono per quest'applicazione
df <- compas %>% select(-probability, -predicted)

# genero campioni di training e test
set.seed(77)
in_train <- caret::createDataPartition(df$Two_yr_Recidivism, p=0.7, list = F)
df_train <- df[in_train, ]
df_test <- df[-in_train, ]

# dimensioni dei due dataset 
print(nrow(df_train))
print(nrow(df_test))

# addestro i due modelli 
# 1- con tutti i predittori 
model1 <- glm(Two_yr_Recidivism ~ .,
              data = df_train,
              family = binomial(link = 'logit'))
# 2 - con tutti i predittori tranne la variabili ethnicity
model2 <- glm(Two_yr_Recidivism ~ . -ethnicity,
              data = df_train,
              family = binomial(link = 'logit'))

# Previsioni con i due modelli generati 
df_test$prob_1 <- predict(model1, df_test, type = 'response')
df_test$prob_2 <- predict(model2, df_test, type = 'response')
# ispezione dei risultati 
head(df_test)

# variabile target - trasformo in 0/1
df_test$Two_yr_Recidivism_01 <- ifelse(df_test$Two_yr_Recidivism == 'yes', 1, 0)
# ispezione 
table(df_test$Two_yr_Recidivism_01)

############################## METRICHE DI FAIRNESS ##################
# PREDICTIVE RATE PARITY
# TP / (TP + FP)
res1 <- pred_rate_parity(data = df_test,
                         outcome = 'Two_yr_Recidivism_01',
                         outcome_base = '0',
                         group = 'ethnicity',
                         probs = 'prob_1',
                         cutoff = 0.5,
                         base = 'Caucasian')
res1$Metric

# cambio modalità di riferimento 
res1h <- pred_rate_parity(data = df_test,
                          outcome = 'Two_yr_Recidivism_01',
                          outcome_base = '0',
                          group = 'ethnicity',
                          probs = 'prob_1',
                          cutoff = 0.5,
                          base = 'Hispanic')
res1h$Metric
# visualizzo 
res1h$Metric_plot
res1h$Probability_plot


# calcolo per il modello 2
res2 <- pred_rate_parity(data = df_test,
                         outcome = 'Two_yr_Recidivism_01',
                         outcome_base = '0',
                         group = 'ethnicity',
                         probs = 'prob_2',
                         cutoff = 0.5,
                         base = 'Caucasian')
# confronto 
res2$Metric
res1$Metric

# DEMOGRAPHIC PARITY
# (TP + FP)
res_dem <- dem_parity(data         = df_test, 
                      outcome      = 'Two_yr_Recidivism_01', 
                      outcome_base = '0', 
                      group        = 'ethnicity',
                      probs        = 'prob_1', 
                      cutoff       = 0.5, 
                      base         = 'Caucasian')
# visualizzo 
res_dem$Metric
res_dem$Metric_plot
table(df_test$ethnicity)

# PROPORTIONAL PARITY
# (TP + FP) / (TP + FP + TN + FN)
res_prop <- prop_parity(data         = df_test, 
                        outcome      = 'Two_yr_Recidivism_01', 
                        outcome_base = '0', 
                        group        = 'ethnicity',
                        probs        = 'prob_1', 
                        cutoff       = 0.5, 
                        base         = 'Caucasian')
# visualizzo 
res_prop$Metric
res_prop$Metric_plot

# EQUALIZED ODD
# TP / (TP + FN)
res_eq <- equal_odds(data         = df_test, 
                     outcome      = 'Two_yr_Recidivism_01', 
                     outcome_base = '0', 
                     group        = 'ethnicity',
                     probs        = 'prob_1', 
                     cutoff       = 0.5, 
                     base         = 'African_American')  
# visualizzo 
res_eq$Metric

# ACCURACY PARITY
# (TP + TN) / (TP + FP + TN + FN)
res_acc <- acc_parity(data         = df_test, 
                      outcome      = 'Two_yr_Recidivism_01', 
                      outcome_base = '0', 
                      group        = 'ethnicity',
                      probs        = 'prob_1', 
                      cutoff       = 0.5, 
                      base         = 'African_American')
# visualizzo 
res_acc$Metric

# FALSE NEGATIVE RATE PARITY
# FN / (TP + FN)
res_fnr <- fnr_parity(data         = df_test, 
                      outcome      = 'Two_yr_Recidivism_01', 
                      outcome_base = '0', 
                      group        = 'ethnicity',
                      probs        = 'prob_1', 
                      cutoff       = 0.5, 
                      base         = 'African_American')
# visualizzo 
res_fnr$Metric

# FALSE POSITIVE RATE PARITY
# FP / (TN + FP)
res_fpr <- fpr_parity(data         = df_test, 
                      outcome      = 'Two_yr_Recidivism_01', 
                      outcome_base = '0', 
                      group        = 'ethnicity',
                      probs        = 'prob_1', 
                      cutoff       = 0.5, 
                      base         = 'African_American')
# visualizzo 
res_fpr$Metric

# NEGATIVE PREDICTIVE VALUE PARITY
# TN / (TN + FN)
res_npv <- npv_parity(data         = df_test, 
                      outcome      = 'Two_yr_Recidivism_01', 
                      outcome_base = '0', 
                      group        = 'ethnicity',
                      probs        = 'prob_1', 
                      cutoff       = 0.5, 
                      base         = 'African_American')
# visualizzo 
res_npv$Metric

# SPECIFICITY PARITY
# TN / (TN + FP)
res_sp <- spec_parity(data         = df_test, 
                      outcome      = 'Two_yr_Recidivism_01', 
                      outcome_base = '0', 
                      group        = 'ethnicity',
                      probs        = 'prob_1', 
                      cutoff       = 0.5, 
                      base         = 'African_American')
# visualizzo 
res_sp$Metric

# ROC AUC COMPARISON
res_auc <- roc_parity(data         = df_test, 
                      outcome      = 'Two_yr_Recidivism_01', 
                      group        = 'Female',
                      probs        = 'prob_1', 
                      base         = 'Male')
# visualizzo 
res_auc$Metric
res_auc$ROCAUC_plot

# MATTHEWS CORRELATION
# (TP×TN-FP×FN)/√((TP+FP)×(TP+FN)×(TN+FP)×(TN+FN))
res_mcc <- mcc_parity(data         = df_test, 
                      outcome      = 'Two_yr_Recidivism_01', 
                      outcome_base = '0', 
                      group        = 'Female',
                      probs        = 'prob_1', 
                      cutoff       = 0.5, 
                      base         = 'Male')
# visualizzo 
res_mcc$Metric

