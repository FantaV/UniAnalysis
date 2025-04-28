/* 1. SINTASSI SQL

Aprire un commento si può utilizzare il doppio trattino oppure SLASH + ASTERISCO per aprire e chiudere
e scrivere su più righe.
*/

-- Query per recupero dipendenti per stipendi oltre i 1800€

SELECT nome, data_assunzione, stipendio 
FROM dipendenti 
WHERE stipendio > 1800;  

/* 
Quella che abbiamo in alto è un SQL STATEMENT: un'istruzione in SQL

SELECT, FROM e WHERE sono le parole chiave

da SELECT: "NOME, DATA ASSUNZIONE E STIPENDIO" sono le nostre liste di campi che non sono altro che LE COLONNE DI UNA TABELLA 
da FROM: "DIPENDENTI" rappresenta la nostra TABELLA
da WHERE: "STIPENDIO" rappresenta la nostra condizione

Con il PUNTO E VIRGOLA si chiude uno statement.

SQL NON È KEY SENSITIVE, ma per best practice si tende a scrivere in maiuscolo le parole chiave
*/








