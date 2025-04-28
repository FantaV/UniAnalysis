/* UPDATE

serve per aggiornare o inserire altri valori in una colonna

- sintasi:
UPDATE nome_tabella
SET nome_colonna = valore
WHERE condizione;

- modificare una colonna
- modificare più colonne
- modificare più righe insieme
- ATTENZIONE A NON OMETTERE WHERE!!!!!!!!!!!!!!

*/
-- una colonna 
UPDATE dipendenti
SET id_ufficio = 4
WHERE id_dipendente = 2;

-- un valore 
UPDATE dipendenti
SET nome = UPPER(nome)
WHERE nome = 'alberto';

-- più righe 
UPDATE dipendenti
SET id_ufficio = 2
WHERE id_dipendente IN (3,4);   	-- IN per inserire più valori