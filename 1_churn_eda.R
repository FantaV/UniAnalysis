## Marketing Churn ####

# STEP 1 - Librerie ####
library(tidyverse)
library(skimr)
library(corrplot) # Matrice di correlazione
library(data.table)

# STEP 2 - Working directory ####
getwd() # Controllare in quale directory ci si trova prima di accedere qualsiasi file
setwd("/Users/carlovilloresi/Documents/Coding/R/Projects/1. Customer Churn Classification ")

# STEP 3 - Caricamento Files ####
customer_tbl <- fread("./Churn_Banking_Modeling_Anonym.csv", 
                      sep = ',',        # Specifica che il separatore è una virgola
                      header = T,       # Indica che la prima riga contiene i nomi delle colonne
                      data.table = F,   # Converte l'output in un data.frame anziché in un data.table
                      dec = '.',        # Definisce il punto come separatore decimale
                      na.string = "",    # Tratta le stringhe vuote come valori NA
                      encoding = "Latin-1"
                      )

    # Visualizziamo una parte della tabella clienti
    head(customer_tbl) # per visualizzare in console. Default prime 6 righe
    head(customer_tbl) %>% View() # visualizzazione navigabile a 6 righe
    customer_tbl %>% View() # intero dataset

# STEP 4 - Visualizzazione dei dati e pulizia ####

    # Utilizzando il pacchetto 'skimr' possiamo generare un report con lo stato generale dei dati
    # Il risultato verra' presentato in console
    skim(customer_tbl)

    ## Pulizia variabile Eta ##
    skim(customer_tbl$Eta)

    # Controlliamo le eta' dei clienti
    # ATTNE! Non crea oggetti, ma stampa solo in console
    customer_tbl %>% 
        count(Eta) %>% 
        filter(Eta > 90) %>% 
        arrange(desc(Eta))
    
    # Rimuoviamo clienti con eta' > 85 anni e modifichiamo il dataframe
    customer_tbl <- customer_tbl %>% 
        filter(Eta <= 85)
    
    # Imputiamo eta' media ai missing data
    customer_tbl <- customer_tbl %>% 
        mutate(Eta = case_when(
            is.na(Eta) == TRUE ~ round(mean(Eta, na.rm = TRUE), digits = 0),
            TRUE ~ Eta)
        )
    
    # Controllare risultato finale
    skim(customer_tbl$Eta)

    ## Pulizia variabile 'Amm_Reddito' ##
    unique(customer_tbl$Amm_Reddito)
    
    # Risolviamo il problema dei 219893 missing data
    customer_tbl %>% 
        filter(is.na(Amm_Reddito)) %>% 
        select(Id_Cliente, Professione, Amm_Reddito)

    # Assegnare nd a tutti gli utenti di cui non si conosce la Professione
    customer_tbl <- customer_tbl %>% 
        mutate(Amm_Reddito = case_when(
            is.na(Professione) == TRUE & is.na(Amm_Reddito) == TRUE ~ 'nd',
            TRUE ~ Amm_Reddito
            )
        )
    
    customer_tbl %>% 
        filter(is.na(Professione)) %>% 
        select(Id_Cliente, Professione, Amm_Reddito)
    
    # Nei clienti con Professione nota, assegnamo la classe modale di 'Amm_Reddito'
    # Frequenza di 'Amm_Reddito' per ogni professione 
    freq <- table(customer_tbl$Professione, customer_tbl$Amm_Reddito) %>% 
        as_tibble(.name_repair = "unique") %>% 
        rename(
            Professione = ...1,
            Amm_Reddito = ...2,
            Frequenza   = n
        ) %>% 
        arrange(Professione)
    
    freq
    
    # Classe modale
    modal_class <- freq %>% 
        filter(Amm_Reddito != 'nd') %>% 
        group_by(Professione) %>% 
        summarise(Frequenza = max(Frequenza)) %>% 
        left_join(freq, by = c("Professione", "Frequenza")) %>% 
        select(Professione, Amm_Reddito)
    
    modal_class

    # Classe modale nel dataset principale
    customer_tbl <- customer_tbl %>% 
        left_join(modal_class, by = 'Professione') %>% 
        mutate(Amm_Reddito.x = case_when(
            is.na(Amm_Reddito.x) == TRUE ~ Amm_Reddito.y,
            TRUE ~ Amm_Reddito.x)
        ) %>% 
        select(-Amm_Reddito.y) %>% 
        rename(Amm_Reddito = Amm_Reddito.x)
    
    customer_tbl
    
    ## Gestione Missing Values per variabili 'Seeso', 'Profilo', 'Professione' ##
    customer_tbl <- customer_tbl %>% 
        mutate(across(.cols = c('Sesso','Profilo_MIFID','Professione'), ~ replace_na(data = .x, replace = 'nd')))
    
    ## Gestione Missing Values per variabili numeriche ##
    customer_tbl <- customer_tbl %>% 
        mutate(across(where(is.numeric) & contains(match = c("Amm_", "Cont_")), ~ replace_na(data = .x, replace = 0))) 
    
    customer_tbl <- customer_tbl %>% 
        mutate(across(.cols = c('Ind_Variazione_Accredito_Stipendio'), ~ replace_na(data = .x, replace = 0)))
    
    ## Trasformazione variabili nominali in factor ##
    nom_vars <- c("Professione", 
                  "Amm_Reddito", 
                  "Categoria_Cliente", 
                  "Sesso", 
                  "Profilo_MIFID")
    
    customer_tbl <- customer_tbl %>% mutate(across(.cols = all_of(nom_vars), ~ as_factor(.x)))
    
    customer_tbl <- customer_tbl %>% mutate(var_target = case_when(
        var_target == 'si' ~ 1,
        TRUE ~ 0)
    )

    ## Trasformazione variabili flag in factor ##
    customer_tbl <- customer_tbl %>% 
        mutate(across(contains(match = "Ind_"), ~ as_factor(.x))) 

# STEP 5 - Analisi relazioni tra variabili ####
    
    num_vars <- customer_tbl %>% select(where(is.numeric))
    
    corr_matrix <- cor(num_vars)
    
    col2 = colorRampPalette(c('red', 'white', 'blue')) 
    corrplot(corr_matrix, type = "upper", tl.col = 'black', cl.ratio = 0.1, col = col2(10))
    
    ## Analisi relazione tra variabili flag ##
    t1 <- table(customer_tbl$var_target, customer_tbl$Ind_Mutuo)
    colnames(t1) <- c("Ind_Mutuo = 0", "Ind_Mutuo = 1")
    rownames(t1) <- c("var_target = 0", "var_target = 1")
    
    t1

    ## Analisi relazione tra variabili flag e variabile continua ##
    Bon <- customer_tbl %>% 
        group_by(var_target) %>% 
        summarise(Amm_Bonifici_Competitors_mean = mean(Amm_Bonifici_Competitors, na.rm = TRUE))
    
    Bon

    ## Analisi relazione tra variabili flag e variabile nominale ##  
    t2 <- round(prop.table(table(customer_tbl$var_target, customer_tbl$Categoria_Cliente),margin = 2)*100,2)
    
    rownames(t2) <- c("var_target = 0", "var_target = 1")
    
    t2

# Si procedere ad addestrare un modello

    
    
    
    
    
    
    
    
    
    
    
    
    
