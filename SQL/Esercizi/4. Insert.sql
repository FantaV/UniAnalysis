/* 06. INSERT

- sintassi 
. inserire la prima riga 
- senza campi
. auto increment
- inserimenti multipli
*/

INSERT INTO dipedenti ( id_dipendente, nome,cognome,data_assunzione, stipendio, telefono, mansione)
VALUES
-- (1, "Gianmarco", "Lucetti", "2000-12-26", 1900.00,"1234567891", "stagista"),
(2,"Pollino", "Cameroti", "2000-07-19", 1950.00,"1234567892", "risk manager"),
(3, "Giacomo", "chilo", "2000-01-31", 3400.00,"1234567893", "compliance"),
(4, "alberto", "cestino", "2000-07-11", 2500.00,"1234567894", "mafioso");
