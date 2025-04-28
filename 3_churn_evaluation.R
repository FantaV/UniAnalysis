## Marketing Evaluation ####

# STEP 1 - Librerie ####
library(tidyverse)

# library(unbalanced)
library(ROSE) # per sostituire unbalanced
library(caret)
library(C50)
library(gains)
library(gridExtra)

# STEP 2 - Working directory ####
getwd() # Controllare in quale directory ci si trova prima di accedere qualsiasi file
setwd("/Users/carlovilloresi/Documents/Coding/R/Projects/1. Customer Churn Classification ")

# STEP 3 - Caricare il dataset pulito e pronto per la fase di Modeling ####
load("Modeling.RData")

# STEP 4 - Creazione Gain Curve ####
gain_curve <- gains(target, result_b$score, optimal = TRUE, percents = TRUE, groups = 50)
gain_curve_boost <- gains(target, result_boost$score, optimal = TRUE, percents = TRUE, groups = 100)

    # Cumulative Gain Chart

    # GGPLOT2
    # ggplot2 accetta solo df, quindi devo estrarre le variabili da gain_curve e creare un df
    gain_curve_boost_tbl <- bind_cols(c(0,gain_curve_boost$depth), c(0,gain_curve_boost$cume.pct.of.total*100)) %>% 
        rename(boost_depth = ...1,
               boost_cume.pct.of.total = ...2)
    
    gain_curve_tbl <- bind_cols(c(0,gain_curve$depth), c(0,gain_curve$cume.pct.of.total*100)) %>% 
        rename(depth = ...1,
               cume.pct.of.total = ...2)
    
    ggplot(data = gain_curve_boost_tbl) +
        geom_line(aes(x = boost_depth, y = boost_cume.pct.of.total), colour = "darkgreen", show.legend = TRUE) +
        geom_line(data = gain_curve_tbl, aes(x = depth, y = cume.pct.of.total), colour = "darkblue", show.legend = TRUE) +
        geom_line(data = data.frame(x = c(0,100), y = c(0,100)), aes(x = x, y = y), colour = 'red', show.legend = TRUE) +
        geom_line(data = data.frame(x = c(0, nrow(positive)/nrow(testing), 100), y = c(0,100,100)), aes(x = x, y = y), colour = 'orange', show.legend = TRUE) +
        theme_minimal() +
        labs(title = "Cumulative Gains Chart") +
        xlab("% Customers Contacted") +
        ylab("% Positive Responses")
    
    # ALBERO SEMPLICE
    
    ggplot(data = gain_curve_tbl) +
        geom_line(aes(x = depth, y = cume.pct.of.total), colour = "darkblue", show.legend = TRUE) +
        geom_line(data = data.frame(x = c(0,100), y = c(0,100)), aes(x = x, y = y), colour = 'red', show.legend = TRUE) +
        geom_line(data = data.frame(x = c(0, nrow(positive)/nrow(testing), 100), y = c(0,100,100)), aes(x = x, y = y), colour = 'orange', show.legend = TRUE) +
        theme_minimal() +
        labs(title = "Cumulative Gains Chart") +
        xlab("% Customers Contacted") +
        ylab("% Positive Responses")

# STEP 5 - Scelta della soglia di segnalazione ####
C5_boost_cum <- result_boost %>% 
        
    # binning della variabile score
    mutate(score_bin = case_when(
        score < 0.1 ~ "0.0-0.1",
        score < 0.2 ~ "0.1-0.2",
        score < 0.3 ~ "0.2-0.3",
        score < 0.4 ~ "0.3-0.4",
        score < 0.5 ~ "0.4-0.5",
        score < 0.6 ~ "0.5-0.6",
        score < 0.7 ~ "0.6-0.7",
        score < 0.8 ~ "0.7-0.8",
        score < 0.85 ~ "0.8-0.85",
        score < 0.9 ~ "0.85-0.9",
        score < 0.95 ~ "0.9-0.95",
        score <= 1 ~ "0.95-1")
    ) %>% 
        
    # raggruppamento
    group_by(score_bin) %>% 
    summarize(
        nr_totale = n(),
        nr_target_1 = sum(target)
    ) %>% 
    ungroup() %>% 
        
    arrange(desc(score_bin)) %>% 
        
    # creazione cumulative
    mutate(
        nr_contact_cum = cumsum(nr_totale),
        nr_TP_cum = cumsum(nr_target_1),
        precision = round(nr_TP_cum / nr_contact_cum *100,2),
        recall = round(nr_TP_cum / sum(nr_target_1) *100,2)
    )
    
C5_boost_cum

# STEP 6 - Scenario What-If per la scelta della soglia di cut-off
contact_cost <- 5
revenue_profit <- 100

cut_off <- C5_boost_cum %>% 
    
    select(score_bin, nr_contact_cum, nr_TP_cum) %>% 
    
    mutate(
        total_contact_cost = nr_contact_cum * contact_cost,
        total_revenue_profit = nr_TP_cum * revenue_profit,
        ROI = total_revenue_profit - total_contact_cost
    )

cut_off

# CONCLUSIONI ####
# Possiamo procedere con il deployment











