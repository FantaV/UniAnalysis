/* 21. GROUP BY e HAVING

Group by serve per aggregare RIGHE che hanno uno stesso valore (che andiamo a selezionare con group by) e SE E' DOPPIONE allora aggrrego le righe;

- sintassi:
SELECT colonna, count(elemento da contare) AS alias
FROM tabella AS x
LEFT JOIN tabella2 as y
ON tabella1.colonna = tabella2.colonna
GROUP BY tabella1.colonna;
*/

SELECT x.nome_ufficio
FROM uffici AS x
LEFT JOIN dipendenti AS y 
ON x.id_ufficio = y.id_ufficio
GROUP BY x.nome_ufficio;