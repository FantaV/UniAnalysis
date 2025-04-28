library(tidyverse)
library(data.table)

# percorso 
path <- "~/Documents/Coding/R/Projects/9. Marketing Clustering/"
file_name_cluster = paste0(path, "dataset_cluster.txt")
CT_cluster <- read.table(file_name_cluster, sep="\t", header=T, fileEncoding="latin1")


# primo esame 
glimpse(CT_cluster)
# selezione delle variabili suggerite dal cliente
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
        Amm_Gestito_Attuale)

# escludo il cliente
dataset_cluster <- CT_cluster_var %>% select(-Id_Cliente)

# analisi delle correlazioni 
corr_matrix <- cor(dataset_cluster)
library(corrplot)
col2 = colorRampPalette(c('red', 'white', 'blue')) 
corrplot(corr_matrix, type = "upper", 
         tl.col = 'black', cl.ratio = 0.1, col = col2(10))




################# cluster senza standardizzare ##############
k4_no_sd <-dataset_cluster %>%
    kmeans(nstart = 25,centers = 4)

df_cl <- data.frame(cl = k4_no_sd$cluster)


ggplot(df_cl, aes(x=cl))+
    geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5)+
    ggtitle("Cluster size")+
    xlab("Cluster id")+
    ylab("Percentage")+
    coord_flip() +
    theme_minimal()

################# fine cluster senza standardizzare ##############


################# cluster con standardizzazione  ##############
k4_sd <-dataset_cluster %>%
    mutate_all(scale) %>% 
    kmeans(nstart = 25,centers = 4, iter.max = 30)

df_cl <- data.frame(cl = k4_sd$cluster)


ggplot(df_cl, aes(x=cl))+
    geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5)+
    ggtitle("Cluster size")+
    xlab("Cluster id")+
    ylab("Percentage")+
    coord_flip() +
    theme_minimal()

################# fine cluster con std ##############

# standardizzazione 
dataset_cluster <- dataset_cluster %>% mutate_all(scale)

k2 <- kmeans(dataset_cluster, nstart=25, centers = 2   , iter.max = 30)
k3 <- kmeans(dataset_cluster, nstart=25, centers = 3   , iter.max = 30)
k4 <- kmeans(dataset_cluster, nstart=25, centers = 4   , iter.max = 30)
k5 <- kmeans(dataset_cluster, nstart=25, centers = 5   , iter.max = 30)
k6 <- kmeans(dataset_cluster, nstart=25, centers = 6   , iter.max = 30)
k7 <- kmeans(dataset_cluster, nstart=25, centers = 7   , iter.max = 30)
k8 <- kmeans(dataset_cluster, nstart=25, centers = 8   , iter.max = 30)
k9 <- kmeans(dataset_cluster, nstart=25, centers = 9   , iter.max = 30)
k10 <-kmeans(dataset_cluster, nstart=25, centers = 10  , iter.max = 30)
 
# carico la libreria che ha le funzioni per la valutazione 
library(cluster)

# scelta K ottimale con avg silhouette
output <- data.frame(k = 2:10, sil = NA)
for (k in 2:10) {
    km <- kmeans(dataset_cluster, nstart=25, centers = k , iter.max = 30)
    test <- silhouette(km$cluster, dist(dataset_cluster))
    avg_sil <- mean(test[,3])
    print(paste("K = ", k))
    print(avg_sil)
    output$sil[k-1] <- avg_sil
}
plot(output$k, output$sil, type="b", pch=19)

# scelta K ottimale con WSS
output <- data.frame(k = 2:10, totwss = NA)
for (k in 2:10) {
    km <- kmeans(dataset_cluster, nstart=25, centers = k , iter.max = 30)
    totwss <- km$tot.withinss
    print(paste("K = ", k))
    print(totwss)
    output$totwss[k-1] <- totwss
}
plot(output$k, output$totwss, type="b", pch=19)

# vince k = 3

################# cluster con standardizzazione  ##############
k3 <- kmeans(dataset_cluster, nstart = 25,centers = 3, iter.max = 30)

df_cl <- data.frame(cl = k3$cluster)


ggplot(df_cl, aes(x=cl))+
    geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5)+
    ggtitle("Cluster size")+
    xlab("Cluster id")+
    ylab("Percentage")+
    coord_flip() +
    theme_minimal()

################# fine cluster con std ##############





CT_profile <- CT_cluster %>% 
    select(Eta,
           Cont_Accessi_Sito,
           Cont_attività_trading,
           Amm_Liquidita_Attuale,
           Amm_Amministrato_Attuale,
           Ind_Prestito,
           Amm_Gestito_Attuale
    ) %>%
    mutate(Patrimonio = Amm_Liquidita_Attuale +  Amm_Gestito_Attuale + 
               Amm_Amministrato_Attuale
    )
# aggiungo cluster di appartenenza
CT_profile$cluster <- k3$cluster


profile_k3 <- CT_profile %>% 
    group_by(cluster) %>% 
    summarise(Cont_Accessi_Sito_mean = mean(Cont_Accessi_Sito),
              Patrimonio_mean = mean(Patrimonio),
              Eta_mean = mean(Eta),
              Trading_mean = mean(Cont_attività_trading),
              Ind_Prestito_Sum = sum(Ind_Prestito)
    )

profile_k3


