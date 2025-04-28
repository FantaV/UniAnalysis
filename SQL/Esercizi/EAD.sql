SELECT id_cliente, saldo_residuo +(ccf * importo_utilizzabile) as AED
FROM clienti_credito;

SELECT MAX(AED) 
FROM (
    SELECT id_cliente, saldo_residuo + (ccf * importo_utilizzabile) AS AED
    FROM clienti_credito
) AS subquery;		-- se si toglie AS da errore perche per una subquery bisogna sempre avere un alias




