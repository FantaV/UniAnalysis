/* .17 JOIN

Join (unire) va ad unire più tabelle in base ad una condizione

- inner join: intersezione dei valori comuni
- left join:  prende tutta la tabella di sinistra, e della parte destra soloquella che combacia con la tabella 2
- right join 
- full join (non disponibili in my sql --> union):  permette id unire completamente due tabelle 
- cross join (non presente in questo file)

PER CREARE UNA TABELLA JOINATA CI BASTA INSERIRE CREATE TABLE nome_tabella in alto
*/

/* sintassi 
SELECT campi (sleezioniamo i campi che ci interessano sia della tabella 1 che della tabella 2) ma from tabella 1
FROM tabella1 
INNER JOIN tabella2 ON condizione;
*/

SELECT dipendenti.id_dipendente, dipendenti.nome, ufficio.nome_ufficio
FROM dipendenti 
INNER JOIN ufficio ON dipendenti.id_ufficio = ufficio.id_ufficio;

SELECT dipendenti.id_dipendente, dipendenti.nome, ufficio.nome_ufficio
FROM dipendenti 
LEFT JOIN ufficio ON dipendenti.id_ufficio = ufficio.id_ufficio;

SELECT dipendenti.id_dipendente, dipendenti.nome, ufficio.nome_ufficio
FROM dipendenti 
RIGHT JOIN ufficio ON dipendenti.id_ufficio = ufficio.id_ufficio;