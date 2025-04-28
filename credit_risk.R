# Rischio di Credito ####

# STEP 1 - Librerie ####
library(tidyverse)
library(caret)
library(glmnet)
library(verification) # Funzioni roc.plot e roc.area

# STEP 2 - Working directory ####
getwd() # Controllare in quale directory ci si trova prima di accedere qualsiasi file
setwd("/Users/carlovilloresi/Documents/Coding/R/Projects/3. Logistic Regression Credit Risk")

# STEP 3 - Caricare il dataset. Il dataset e' contenuto nel pacchetto 'fairness' ####
dataset <- fairness::germancredit

# Organizziamo le variabili e trasformiamo la variabile target in 0 - 1
dataset <- dataset %>% 
    # Specificare dplyr perche altrimenti va in conflitto con MASS
    dplyr::select(-predicted, -probability) %>% 
    rename(Gender = Female) %>% 
    mutate(BAD = ifelse(BAD == "BAD",1,0)) %>% 
    relocate(BAD) # Sposta la variabile nella prima colonna

# STEP 4 - Train / Test Split ####
set.seed(12420246) # Per garantire riproducibilita' del codice

in_train <- createDataPartition(dataset$BAD, p=0.5, list = F)

dataset_train <- dataset[in_train,]
dataset_test <- dataset[-in_train,]

# STEP 5 - Analisi della relazione tra le variabili continue e la target ####
an_dur <- dataset_train %>% 
    dplyr::select(Duration, BAD) %>% 
    mutate(bin = ntile(Duration, 10))

ggplot(data = an_dur) + 
    geom_bar(aes(x = bin, fill = as.factor(BAD)))


an_amt <- dataset_train %>% 
    dplyr::select(Amount, BAD) %>% 
    mutate(bin = ntile(Amount, 10))

ggplot(data = an_amt) + 
    geom_bar(aes(x = bin, fill = as.factor(BAD)))


an_age <- dataset_train %>% 
    dplyr::select(Age, BAD) %>% 
    mutate(bin = ntile(Age, 10))

ggplot(data = an_age) + 
    geom_bar(aes(x = bin, fill = as.factor(BAD)))

# STEP 6 - Codifica delle variabili con WoE binning ####
library(devtools)

# codifica le tre variabili con woe binning
devtools::install_github("ayhandis/creditR")
library(creditR)

# Impostazione delle regole con WoE binning
woerules <- woe.binning(df = dataset_train,target.var = "BAD", 
        pred.var = c("Amount","Duration", "Age"), event.class = 1)

# Applicazionde del binning
train_woe <- woe.binning.deploy(df=dataset_train, woerules, add.woe.or.dum.var='woe')

# Estrazione delle variabili codificate per valutazione risultati
train_woe <- woe.get.clear.data(train_woe,default_flag = "BAD",prefix = "woe")

# Esame dell'effetto della trasformazione
t1 <- table(train_woe$BAD, train_woe$woe.Duration.binned)
proportions(t1, margin=2)
barplot(proportions(t1, margin=2))

t2 <- table(train_woe$BAD, train_woe$woe.Amount.binned)
proportions(t2, margin=2)
barplot(proportions(t2, margin=2))

t3 <- table(train_woe$BAD, train_woe$woe.Age.binned)
proportions(t3, margin=2)
barplot(proportions(t3, margin=2))

# STEP 7 - Modeling ####

# Preparazione del dataset per il modeling 
altri_pred_train <- dataset_train %>% dplyr::select(-BAD, -Duration, -Age, -Amount)
dataset_train_enc <- train_woe %>% bind_cols(altri_pred_train) %>% relocate(BAD)

# Modello 1: Modello addestrato con tutti i predittori
mod_all <- glm(BAD ~ ., data = dataset_train,  family = binomial)

# Modello 1: Verifica della stabilita' dei coefficienti. Ci si aspetta un'alta variabilita' 
options(scipen=100)
set.seed(NULL) # Rimuovo seed per verificare instabilità coefficienti
num_it = 30
output_all <- NULL
for (i in 1:num_it) {
    set.seed(NULL)
    in_train <- createDataPartition(dataset$BAD, p=0.8, list = F)
    dataset_train <- dataset[in_train,]
    dataset_test <- dataset[-in_train,]
    glm_uci <- glm(BAD ~ ., 
                   family = binomial, 
                   dataset_train)
    # se voglio interpretare i coefficienti 
    cf <- exp(coef(glm_uci))
    if(is.null(output_all)) {
        output_all <- data.frame(cf = cf)
    }
    else {
        output_all <- cbind(output_all, cf)
    }
    
}
camp_var <- apply(output_all, 1, function(x) max(x) - min(x))
output_all$camp_var <- camp_var
output_all <- output_all %>% relocate (camp_var)


# Modello 2: Modello addestrato con selezione manuale dei predittori
form_man_2 <- as.formula("BAD ~ Account_status + Duration + Credit_history +  
    Amount + Savings + Employment + Installment_rate + Guarantors + 
    Age + Other_plans + Housing")

# Modello 2: Verifica della stabilita' dei coefficienti. Ci si aspetta una minore variabilita' 
options(scipen=100)
set.seed(NULL) # Rimuovo seed per verificare instabilità coefficienti
num_it = 30
output_man_2 <- NULL
for (i in 1:num_it) {
    set.seed(NULL)
    in_train <- createDataPartition(dataset$BAD, p=0.8, list = F)
    dataset_train <- dataset[in_train,]
    dataset_test <- dataset[-in_train,]
    glm_uci <- glm(formula = form_man_2, 
                   family = binomial, 
                   dataset_train)
    # se voglio interpretare i coefficienti 
    cf <- exp(coef(glm_uci))
    if(is.null(output_man_2)) {
        output_man_2 <- data.frame(cf = cf)
    }
    else {
        output_man_2 <- cbind(output_man_2, cf)
    }
    
}
camp_var <- apply(output_man_2, 1, function(x) max(x) - min(x))
output_man_2$camp_var <- camp_var
output_man_2 <- output_man_2 %>% relocate (camp_var)


# Modello 3: Modello addestrato con selezione manuale dei predittori - 3 variabili
form_manual <- as.formula("BAD ~ Duration + Amount + Age")

# Modello 3: Verifica della stabilita' dei coefficienti.  
options(scipen=100)
set.seed(NULL) # Rimuovo seed per verificare instabilità coefficienti
num_it = 30
output_manual <- NULL
for (i in 1:num_it) {
    set.seed(NULL)
    in_train <- createDataPartition(dataset$BAD, p=0.8, list = F)
    dataset_train <- dataset[in_train,]
    dataset_test <- dataset[-in_train,]
    glm_uci <- glm(formula = form_manual, 
                   family = binomial, 
                   dataset_train)
    # se voglio interpretare i coefficienti 
    cf <- exp(coef(glm_uci))
    if(is.null(output_manual)) {
        output_manual <- data.frame(cf = cf)
    }
    else {
        output_manual <- cbind(output_manual, cf)
    }
    
}
camp_var <- apply(output_manual, 1, function(x) max(x) - min(x) )
output_manual$camp_var <- camp_var
output_manual <- output_manual %>% relocate (camp_var)

# STEP 8 - Valutazione delle performances ####

# Dopo aver analizzato la variabilita' dei coefficienti nei modelli addestrati con variabili correlate
# Decidiamo di addestrare un modello finale che presenteremo al business

form_man_2 <- as.formula("BAD ~ Account_status  + Credit_history +  
    Savings + Employment + Installment_rate + Guarantors + 
    + Other_plans + Housing +  woe.Duration.binned + woe.Amount.binned  + woe.Age.binned")

mod_f2 <- glm(formula = form_man_2, data = dataset_train_enc,  family = binomial)

# Per poter valutare il modello, devo applicare WoE anche al dataset di test
# Applicazione delle codifiche WoE anche sul campione di test
test_woe <- woe.binning.deploy(dataset_test, woerules, add.woe.or.dum.var = 'woe')
test_woe <- woe.get.clear.data(test_woe, default_flag = "BAD", prefix = "woe")

altri_pred_test  <- dataset_test %>% dplyr::select(-BAD, -Duration, -Age, -Amount)
dataset_test_enc <- test_woe %>% bind_cols(altri_pred_test) %>% relocate(BAD)

# IN-SAMPLE MISCLASSIFICATION - Performance sul dataset di Training
mod_in_esame <- mod_f2
prob_insample <- predict(mod_in_esame, type = "response")

predicted_insample <- prob_insample > 0.167
predicted_insample <- as.numeric(predicted_insample)

mean(ifelse(dataset_train_enc$BAD != predicted_insample, 1, 0))

# CONFUSION MATRIX

table(dataset_train_enc$BAD, predicted_insample, dnn = c("Truth", "Predicted"))

# CURVE ROC

roc.plot(dataset_train_enc$BAD == 1, prob_insample)
roc.area(dataset_train_enc$BAD == '1', prob_insample)$A

# OUT OF SAMPLE MISCALSSIFICATION - Performance sul dataset di Test
prob_outsample <- predict(mod_in_esame, dataset_test_enc, type = "response")

predicted_outsample <- prob_outsample > 0.1667 
predicted_outsample <- as.numeric(predicted_outsample)

mean(ifelse(dataset_test_enc$BAD != predicted_outsample, 1, 0))

# CONFUSION MATRIX

table(dataset_test_enc$BAD, predicted_outsample, dnn = c("Truth", "Predicted"))

# CURVE ROC

roc.plot(dataset_test_enc$BAD == 1, prob_outsample)
roc.area(dataset_test_enc$BAD == 1, prob_outsample)$A

# STEP 9 - Calibrazione
cal <- regression.calibration(mod_f2, dataset_test_enc, "BAD")
cal <- cal$calibration_data %>% dplyr::select(BAD, calibrated_pd)
cal$bin <- cut(cal$calibrated_pd, breaks = seq(0,1.01,by=0.1))

cal_group <- cal %>% group_by(bin) %>% 
    summarise(num = n(), hit= sum(BAD), pc = hit/num)

cal_group <- cal_group %>% mutate (cum_num = cumsum(num), 
        cum_hit = cumsum(hit), cum_pc = cum_hit / cum_num) 


