/* 22. VIEW

View ci permette di creare delle query e di salvarle senza eseguirle

*/

/* CREATE VIEW prova AS 
SELECT x.id_dipendente, x.nome, y.id_ufficio 
FROM dipendenti AS x
LEFT JOIN ufficio AS y 
ON x.id_ufficio = y.id_ufficio;
*/

SELECT * FROM prova			-- si usa select per richiamare la view