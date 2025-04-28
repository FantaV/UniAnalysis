library(tidyverse)
library(data.table)

# ATTENZIONE: modificare il path
path = "./Lezione 42 - Esercitazione Anomaly/"
nome_file_input = paste0(path, "Export_Netflow.csv")
dataset <- fread(nome_file_input, sep=",", header=T, data.table = F)
dim(dataset)

# scartiamo delle variabili inutili (per l'esercitazione)
nomi_scartare = c("V1","Index_table", "score", "scoreBin")
dataset <- dataset %>% select(!all_of(nomi_scartare))

length(unique(dataset$SrcAddr)) # 909
length(unique(dataset$Sport)) # 50915
length(unique(dataset$DstAddr)) # 1113
length(unique(dataset$Dport)) # 5337
summary(dataset$MinTimestamp) # circa 3 giorni di dati 
#summary(dataset$Nr_link_sessione) # asimmetrica 
#summary(dataset$Indicatore_Media_SrcDestPort) # questa è un po' simmetrica 
#hist(dataset$ind_port) # anche questa è simmetrica

var_training <- c("Nr_link_sessione",
                  "N_link_Sum",
                  "N_link_Dist",
                  "Link_Dist",
                  "Indicatore_mean_sec",
                  "Indicatore_porta_bytes",
                  "Indicatore_porta_pkts",
                  "Indicatore_Media_SrcDestPort",
                  "ind_port",
                  "Nr_distPort",
                  "Dur",
                  "TotPkts",
                  "TotBytes")

var_no_training <-  c("SrcAddr", "Sport", "DstAddr", "Dport", "MinTimestamp")

dataset_training <- dataset %>% select(!all_of(var_no_training))

var_no_training_2 <- c("Dur", "TotPkts", "TotBytes")

############## isoltaion forest 
# install.packages("isotree")
library(isotree)

# Addestramento. Gli iperparametri scelti genereranno 
# un modello semplice. Motivo: ridurre i tempi di training bassi e 
# l'occupazione di spazio in memoria. 
# Nel caso si volessero generare modelli più complessi, occhio 
# all'occupazione di RAM e ai tempi di training
m1 <- isolation.forest(dataset_training, ntrees=100, nthreads = 4, 
                       sample_size = 10000, ndim=1) 

# genero la colonna degli score di anomalia
# quest'operazione può prendere del tempo 
pred_if <- predict(m1, dataset_training)
# aggiungo la colonna degli score al dataset originario
risultati_business <- dataset %>% mutate(if_score = pred_if)

# esamino la distribuzione degli score 

hist(risultati_business$if_score)


summary(risultati_business$if_score)
# nelle classi
hist(risultati_business$if_score, breaks = seq(0.1,1,0.1))


# prendo le anomalie con lo score più alto 
# per adesso criterio poco scientifico 

risultati_business %>% filter(if_score >= 0.85) %>% count()
anomalie_si <- risultati_business %>% filter(if_score >= 0.85)
anomalie_no <- risultati_business %>% filter(if_score <= 0.4)

# visualizzazione ad impatto 
# calcolo la colonna dei minimi e massimi del radar
# con i percentili
options(scipen = 100)
limiti_radar <- apply(dataset_training, 2, function(x) c(max = max(x),min = min(x)))
limiti_radar <- as.data.frame(limiti_radar)


# analisi delle sessioni
riga = 1
# scegliere qui la riga da commentare per analizzare anomalie o 
# casi non anomali 
dati_evidenza <- anomalie_si[riga,] 
# dati_evidenza <- anomalie_no[riga,] 
evidenza <- dati_evidenza %>% select(all_of(var_training))
dataset_radar <- rbind(limiti_radar, evidenza)
titolo = paste0("Time = " , dati_evidenza$MinTimestamp, " -- " ,
                "Anomaly Score = " , dati_evidenza$if_score)
if(dati_evidenza$if_score >= 0.8) {
    colore = 2
} else {
    colore = 1
}
library(fmsb)
radarchart(dataset_radar, title= titolo, pcol= colore)

# Visualizzazione a Grafo 
# estraggo il vicinato delle sessioni anomale 
# lo visualizzo 
# parto da dati_evidenza
time_min = dati_evidenza$MinTimestamp - 300
time_max = dati_evidenza$MinTimestamp + 300
# importo la libreria per la manipolazione dei grafi 
library(igraph)
edgelist <- dataset %>% filter(MinTimestamp >= time_min 
                               & MinTimestamp <= time_max) %>% 
    group_by(SrcAddr, DstAddr) %>%
     summarize(weight = n())
grafo <- graph_from_data_frame(edgelist)
# estraggo i nodi che hanno generato l'anomalia
nodo_anom_1 = dati_evidenza$SrcAddr
nodo_anom_2 = dati_evidenza$DstAddr
nodi_evidenza <- V(grafo)[V(grafo)$name == nodo_anom_1 | V(grafo)$name == nodo_anom_2]
# costruisco il vicinato di quei nodi 
t1 <- make_ego_graph(grafo, nodes = nodi_evidenza)
g_evidenza <- union(t1[[1]], t1[[2]])
plot(g_evidenza)


library(visNetwork)
visIgraph(g_evidenza)



