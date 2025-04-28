library(arules)
library(arulesViz)
library(tidyverse)
library(plyr)
library(readxl)

# caricamento del dataset 
# ATTENZIONE: modificare il path
dataset <- read_excel(path = paste0(getwd(),'/Lezione 44 - Regole associative/Online Retail.xlsx'))

# rimozione missing data
dataset <- dataset %>% 
    filter(complete.cases(.) == TRUE)

# costruzione itemset
# selezione variabili
transaction_df <- dataset %>% 
    select(InvoiceNo, Description)

itemList <- ddply(transaction_df, c("InvoiceNo"),
                  function(transaction_df)paste(transaction_df$Description, collapse = ","))

# data preparation 
tr <- itemList %>% select(Item = V1)

# scarico su file  
# ATTENZIONE: modificare il path
write.csv(tr, "./Lezione 44 - Regole associative/market_basket_tr.csv", row.names = F)
# ricarico il dataset 
transaction <- read.transactions('./Lezione 44 - Regole associative/market_basket_tr.csv', 
                                 format = 'basket', 
                                 quote = "", 
                                 cols = NULL, 
                                 sep=',', 
                                 skip = 1, 
                                 rm.duplicates = T)
# ispezione
summary(transaction)
# generazione regole associative
rules <- apriori(transaction, parameter = list(supp=0.001, conf=0.8, maxlen=10))
# ispezione risultati
summary(rules)
inspect(rules[1:10])

# By confidence
rules_by_confidence <- sort(rules, by ='confidence', decreasing = TRUE)
toprules_by_confidence <- rules_by_confidence[1:10]
options(digits=2)
inspect(toprules_by_confidence)
# visualizzazione
plot(toprules_by_confidence, method="graph",engine = 'interactive')

# By Lift
rules_by_lift <- sort(rules, by='lift', decreasing = TRUE)
toprules_by_lift <- rules_by_lift[1:10]
options(digits=2)
inspect(toprules_by_lift)
# visualizzazione
plot(toprules_by_lift, method="graph",engine = 'interactive')


# visualizzazione interattiva (100 regole con lift più alto)
p <- inspectDT(rules_by_lift[1:100])
htmlwidgets::saveWidget(p, 'arules.html', selfcontained = FALSE)
browseURL('arules.html')

