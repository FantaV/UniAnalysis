# Load necessary libraries
library(tidyverse)
library(data.table)
library(cluster) # For clustering evaluation
library(corrplot) # For correlation plot

# Set the file path
path <- "~/Documents/Coding/R/Projects/9. Marketing Clustering/"
file_name_cluster <- paste0(path, "dataset_cluster.txt")
CT_cluster <- read.table(file_name_cluster, sep = "\t", header = TRUE, fileEncoding = "latin1")

# Initial exploration of the dataset
glimpse(CT_cluster)

# Select variables suggested by the client
CT_cluster_var <- CT_cluster %>% 
    select(
        Id_Cliente,
        Amm_Bonifici_Competitors,
        Amm_Spesa_Carta_Credito,
        Cont_Accessi_Sito,
        Cont_attività_trading,
        Amm_Liquidita_Attuale,
        Amm_Amministrato_Attuale,
        Amm_Canone_cc,
        Amm_Bonifici_No_Competitors,
        Amm_Spesa_Bancomat,
        Cont_Movimenti_Dispositivi,
        Amm_Gestito_Attuale
    )

# Exclude the customer ID for clustering
dataset_cluster <- CT_cluster_var %>% select(-Id_Cliente)

# Correlation analysis
corr_matrix <- cor(dataset_cluster)
col2 <- colorRampPalette(c('red', 'white', 'blue'))
corrplot(corr_matrix, type = "upper", tl.col = 'black', cl.ratio = 0.1, col = col2(10))

# K-means clustering without standardization
kmeans_no_sd <- function(data, k) {
    kmeans(data, centers = k, nstart = 25)
}

plot_cluster_size <- function(cluster_data) {
    ggplot(cluster_data, aes(x = cl)) +
        geom_bar(aes(y = 100 * (..count..) / sum(..count..)), width = 0.5) +
        ggtitle("Cluster size") +
        xlab("Cluster id") +
        ylab("Percentage") +
        coord_flip() +
        theme_minimal()
}

# K-means without standardization (k=4)
k4_no_sd <- kmeans_no_sd(dataset_cluster, 4)
df_cl <- data.frame(cl = k4_no_sd$cluster)
plot_cluster_size(df_cl)

# K-means with standardization
dataset_cluster_std <- dataset_cluster %>% mutate_all(scale)

# K-means with standardization (k=4)
k4_sd <- kmeans_no_sd(dataset_cluster_std, 4)
df_cl <- data.frame(cl = k4_sd$cluster)
plot_cluster_size(df_cl)

# Standardization for subsequent clustering
dataset_cluster <- dataset_cluster %>% mutate_all(scale)

# Apply k-means for k = 2 to k = 10 and evaluate using Silhouette and WSS
evaluate_k <- function(data, k_range) {
    sil_output <- data.frame(k = k_range, sil = NA)
    wss_output <- data.frame(k = k_range, totwss = NA)
    
    for (k in k_range) {
        km <- kmeans(data, centers = k, nstart = 25, iter.max = 30)
        
        # Silhouette evaluation
        sil_test <- silhouette(km$cluster, dist(data))
        sil_output$sil[k - 1] <- mean(sil_test[, 3])
        
        # WSS evaluation
        wss_output$totwss[k - 1] <- km$tot.withinss
    }
    
    return(list(silhouette = sil_output, wss = wss_output))
}

# Evaluate for k from 2 to 10
k_range <- 2:10
eval_results <- evaluate_k(dataset_cluster, k_range)

# Plot Silhouette and WSS results
plot(eval_results$silhouette$k, eval_results$silhouette$sil, type = "b", pch = 19, main = "Average Silhouette vs K")
plot(eval_results$wss$k, eval_results$wss$totwss, type = "b", pch = 19, main = "Total WSS vs K")

# Based on evaluation, choose K = 3
k3 <- kmeans_no_sd(dataset_cluster, 3)
df_cl <- data.frame(cl = k3$cluster)
plot_cluster_size(df_cl)

# Profile the clusters
CT_profile <- CT_cluster %>% 
    select(
        Eta,
        Cont_Accessi_Sito,
        Cont_attività_trading,
        Amm_Liquidita_Attuale,
        Amm_Amministrato_Attuale,
        Ind_Prestito,
        Amm_Gestito_Attuale
    ) %>%
    mutate(Patrimonio = Amm_Liquidita_Attuale + Amm_Gestito_Attuale + Amm_Amministrato_Attuale)

# Add the cluster assignment
CT_profile$cluster <- k3$cluster

# Summarize the cluster profiles
profile_k3 <- CT_profile %>%
    group_by(cluster) %>%
    summarise(
        Cont_Accessi_Sito_mean = mean(Cont_Accessi_Sito),
        Patrimonio_mean = mean(Patrimonio),
        Eta_mean = mean(Eta),
        Trading_mean = mean(Cont_attività_trading),
        Ind_Prestito_Sum = sum(Ind_Prestito)
    )

print(profile_k3)
