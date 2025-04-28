/* FUNZIONI DI AGGREGAZIONE

hanno la funzione di eseguire calcoli su insiemi di dati ed ottenere quindi dei conteggi
- COUNT: permette di conteggiare il numero di righe di un insieme di dati e può essere utilizzato su singola colonna oppure più colonne
- SUM
- AVG: media aritmetica 
- MIN
- MAX 
*/

-- COUNT:
SELECT COUNT(*) AS conteggio FROM dipendenti;		-- al posto dell'asterisco possiamo inserire il nome colonne
													-- possiamo inserire l'alias per dare l'intestazione al conteggio effettuato 
                                                    
-- DISTINCT: possiamo utilizzarlo per evitare i doppioni		 POSSIAMO ANCHE UTILIZZARLO SENZA COUNT PER VEDERE TUTTI I DATI SENZA FARE  IL COUNT
SELECT COUNT(DISTINCT id_ufficio) FROM dipendenti;

-- SUM:
SELECT SUM(id_dipendente) FROM dipendenti; 
-- Qualora avessimo un magazzino con ad esempio due prodotti disponibili al prezzo di 10 euro l'uno, il solo sum ci restituirebbe la somma dei valori scelti ma se:
SELECT SUM(quantità*prezzo) FROM magazzino;		-- ci verrebbe mostrata la quantità per prezzo

-- MIN e MAX; 
SELECT MIN(prezzo) FROM magazzino;
SELECT MAX(prezzo) FROM magazzino;

-- AVG: 
SELECT AVG(voto) AS media FROM esame WHERE matricola=1000;

