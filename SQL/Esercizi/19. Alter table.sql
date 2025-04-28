/* ALTER TABLE 

Alter si utilizza per MODIFICARE LE TABELLE 

- aggiungere nuova colonna 
ADD column_name data_type constraints;

- cambiare posizione della colonna,
ALTER TABLE table_name 
MODIFY column_name column_definition ADD UNIQUE (column_name,...;

-aggiungere constraints
ALTER TABLE tabLe_name ADD UNIQUE (column_name,...);

- rimuovere una colonna
ALTER TABLE table_name DROP COLUMN column_name;

- cambiare tipo di dato in una colonna
ALTER TABLE table_name MODIFY column_name new_data_type;

- rinominare tabella
ALTER TABLE current_table_name RENAME new_table_name;

*/

ALTER TABLE ufficio ADD seiforte varchar(100);

ALTER TABLE dipedenti MODIFY mansione varchar(255) not null AFTER id_ufficio;

ALTER TABLE ufficio DROP COLUMN camorra;

ALTER TABLE ufficio MODIFY seiforte int;

ALTER TABLE dipedenti RENAME dipendenti;



