CREATE TABLE prestiti (
    id_cliente INT,
    importo_prestito DECIMAL(10, 2),
    tipo_prestito VARCHAR(50),
    status_default INT
);

INSERT INTO prestiti (id_cliente, importo_prestito, tipo_prestito, status_default)
VALUES
(1, 10000, 'personale', 0),
(2, 5000, 'aziendale', 1),
(3, 15000, 'ipotecario', 1),
(4, 7000, 'personale', 0),
(5, 2000, 'auto', 1);


-- JOIN DELLE DUE TABELLE
SELECT 
    prestiti.id_cliente,
    clienti.denominazione,
    prestiti.importo_prestito,
    prestiti.tipo_prestito,
    prestiti.status_default
FROM prestiti
INNER JOIN clienti
ON prestiti.id_cliente = clienti.id_cliente;
 
-- CREAZIONE TABELLA JOINATA
CREATE TABLE prestiti_combinati AS			-- per creare una tabella con il join tra due tabelle inserisco create table e copio il join
SELECT 
    prestiti.id_cliente,
    clienti.denominazione,
    prestiti.importo_prestito,
    prestiti.tipo_prestito,
    prestiti.status_default
FROM prestiti
INNER JOIN clienti
ON prestiti.id_cliente = clienti.id_cliente;

/* CALCOLO DEL PD
sintassi CASE:
CASE
    WHEN condizione1 THEN risultato1
    WHEN condizione2 THEN risultato2
    ...
    ELSE risultato_default
END
*/

SELECT 
    id_cliente,
    tipo_prestito,
    denominazione,
    status_default,
    CASE 
        WHEN status_default = 1 THEN 1.0  -- Probabilità di default 100% se in default
        WHEN status_default = 0 THEN 0.0  -- Probabilità di default 0% se non in default
        ELSE NULL  -- In caso di valori NULL
    END AS PD
FROM prestiti_combinati;


SELECT 
    tipo_prestito,
    COUNT(CASE WHEN status_default = 1 THEN 1 END) / COUNT(*) AS PD
FROM 
    prestiti_combinati
GROUP BY 
    tipo_prestito;


