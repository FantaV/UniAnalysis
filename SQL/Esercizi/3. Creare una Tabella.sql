-- 3. CREARE UNA TABELLA

-- Per ELIMINARE tabella: DROP TABLE + "nome tabella". Se ne avessimo di più dovranno essere separata con dalla virgola

CREATE TABLE if not exists Rapporto_clienti (
    ID_rapporto int not null AUTO_INCREMENT,
    ID_cliente int not null,
    ID_dipendente int not null,
    PRIMARY KEY(ID_rapporto)
    -- FOREIGN KEY(ID_cliente) REFERENCES Clienti(ID_cliente),   -- Chiave esterna che fa riferimento alla tabella Clienti
    -- FOREIGN KEY(ID_dipendente) REFERENCES Dipedenti(ID_dipendente)  -- Chiave esterna che fa riferimento alla tabella Dipedenti
);

CREATE TABLE if not exists Clienti (
    ID_cliente int UNSIGNED not null AUTO_INCREMENT REFERENCES rapporto_clienti(ID_cliente),		-- con AUTO_INCREMENT è OBBLIGATORIO INSERIRE UNA CHIAVE PRIMARIA
    Denominazione varchar(255) not null,
    P_IVA varchar(16) not null unique,
    Indirizzo varchar(255) not null,
    Telefono varchar(10) not null unique,
    PRIMARY KEY(ID_cliente)
);
    
CREATE TABLE if not exists Dipedenti(
ID_dipendente int UNSIGNED not null AUTO_INCREMENT REFERENCES rapporto_clienti(id_cliente),   												 -- NOT NULL: bisogna per forza assegnare un valore 
Nome varchar(255),
Cognome varchar(255),
Data_assunzione date,
Stipendio decimal not null check (stipendio >=1200 AND stipendio <=5000),	 -- CHECK: lo stipendio può essere compreso solo tra questi valori 
Telefono varchar(10) not null unique,  										 -- UNIQUE: valore univoco che può cambiare nel tempo ma deve corrispondere ad un valore univoco 
Mansione varchar(255) not null default 'impiegato', 						 -- DEFAULT: dico che il valore default qualora non fosse inserito nulla è 'Impiegato'  
PRIMARY KEY(ID_dipendente)													 -- PRIMARY KEY: ogni record della tabella deve essere distintivo, non possono esserci due righe con lo stesso valore, NON può contenere valori nulli 
-- FOREIGN KEY(ID_dipendente) REFERENCES Rapporto_clienti(ID_dipendente)		 -- FOREIGN KEY: collega due tabelle, cioè la riga in una tabella con la riga corrispondente in un'altra tabella, assicurando il match tra le due e che i valori esistano anche nella tabella di riferimento 
);																		     -- REFERENCES: È il comando che definisce una foreign key, specificando il campo a cui fare riferimento in un'altra tabella 

CREATE TABLE if not exists ufficio (
    id_ufficio int not null AUTO_INCREMENT, 
    nome_ufficio varchar(255),
    PRIMARY KEY (id_ufficio)  -- Aggiungi la chiave primaria				
);

-- PER EVITARE DI CREARE UN DOPPIONE USIAMO: CREATE TABLE IF NOT EXIST + "nome tabella"

/* i campi sono le COLONNE (come se fossero le lettere in alto di excel). Ad es:

CREATE TABLE nome_tabella(
nome_campo, tipo_di_dato, constraint  
);
*/

/* TIPI DI DATI PER TABELLE:

INT				Stores numeric values in the range of -2147483648 to 2147483647
NOT NULL 		Non può essere lasciato vuoto
DECIMAL		Stores decimal values with exact precision.
CHAR			Stores fixed-length strings with a maximum size of 255 characters.
VARCHAR		Stores variable-length strings with a maximum size of 65,535 characters.
TEXT			Stores strings with a maximum size of 65,535 characters.
DATE			Stores date values in the YYYY-MM-DD format.
DATETIME	Stores combined date/time values in the YYYY-MM-DD HH:MM:SS format.
TIMESTAMP	Stores timestamp values. TIMESTAMP values are stored as the number of seconds since the Unix epoch ('1970-01-01 00:00:01' UTC).
*/