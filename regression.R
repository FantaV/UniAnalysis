### Regressione - Demand Forecasting ####

# STEP 1 - Librerie ####
library(tidyverse)
library(data.table)
library(stringr)
#install.packages("skimr")
library(skimr) # per comando "skim"
library(broom) # per comando "glance"

# STEP 2 - Working directory ####
getwd() # Controllare in quale directory ci si trova prima di accedere qualsiasi file
setwd("/Users/carlovilloresi/Documents/Coding/R/Projects/2. Supply Chain Regression")

# STEP 3 - Data preparation ####
path <- getwd()
nome_file_out_vendite <- paste0(path,"/", "reg_dati_vendite.txt")
nome_file_out_anag <- paste0(path,"/", "reg_dati_anag_prodotti.txt")

dati_vendite <- read.table(nome_file_out_vendite, sep=";", header=T)
dati_prodotti <- read.table(nome_file_out_anag, sep=";", header=T)

# Join dei due dataset 
dataset <- dati_vendite %>% 
    left_join(dati_prodotti, by = "PROD_ID") %>% 
    relocate(PROD_ID, MEINS)

dataset$MEINS <- as.factor(dataset$MEINS)


lista_prodotti <- dataset %>% 
    group_by (PROD_ID) %>% 
    summarise(n_righe = n())

skim(dataset)
hist(dataset$QTA_ORD)

# Evidenze da inviare al Business
exp_qta_neg <- dataset %>% filter(QTA_ORD < 0)

# Con il business concordiamo che si possono imputare a zero
# Queste evidenze rappresentano i mesi in cui si sono effettuati solo dei resi

# Imputazione vendite negative
dataset <- dataset %>% 
    mutate(QTA_ORD = if_else(QTA_ORD < 0, 0, QTA_ORD))

# STEP 4 - Modeling ####

# Scartiamo tutti i prodotti con sconto costante
lista_prod_reg <- dataset %>% 
    group_by(PROD_ID) %>% 
    summarise(n = n(), n_sc = n_distinct(SCONTO)) %>% 
    filter(n_sc > 1)

dataset_regr <- dataset %>% 
    filter(PROD_ID %in% lista_prod_reg$PROD_ID)

# Dividiamo i dataset per le regressini 
lista_dataset <- dataset_regr %>% group_split(PROD_ID)


# Addestriamo una regressione per ogni prodotto
output <- NULL

for (i  in 1:length(lista_dataset)) {
    ds <- lista_dataset[[i]]
    print(i)
    mod <- lm(QTA_ORD ~ SCONTO, data = ds)
    riga <- cbind(prod_id = ds$PROD_ID[1], glance(mod))
    output <- rbind(output, riga)
}

# Estrazione delle regressioni "interessanti"
sig <- output %>% filter(p.value <= 0.05, nobs > 10)

# STEP 5 - Evaluation ####
# Ispezione evidenze dalla tabella dei risultati significativi

pr <- 238
evidenza <- dataset_regr %>% filter(PROD_ID == pr)
md_test <- lm(evidenza$QTA_ORD ~ evidenza$SCONTO)

summary(md_test)
plot(evidenza$SCONTO, evidenza$QTA_ORD)
abline(md_test)









