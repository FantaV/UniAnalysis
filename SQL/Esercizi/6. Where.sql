/* 06. WHERE

a differenza di SELECT che restituisce tutte le righe delle colonne selezionate, con WHERE possiamo scegliere anche le condizioni delle righe da scegliere, ad esempio visionare solo le persone che vivono a Roma 
*/

SELECT * FROM actor WHERE actor_id = 9;		-- condizione

/* Operatori logici di Where:
=  uguale a
!=  diverso da 
< minore di 
> maggiore di 
<=  minore uguale 
>= maggiore uguale
LIKE dove alcuni valori matchano con la nostra esigenza: ad esempio per il nome David, inseriamo Like 'Dav'
IN 
BETWEEN
*/