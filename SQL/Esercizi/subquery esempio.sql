SELECT nome
FROM dipendenti d
WHERE stipendio > (SELECT AVG(stipendio) FROM dipendenti WHERE id_ufficio = d.id_ufficio);

WITH StipendiMedi AS (
    SELECT dipartimento_id, AVG(stipendio) AS stipendio_medio
    FROM dipendenti
    GROUP BY dipartimento_id
)
SELECT d.nome, s.stipendio_medio
FROM dipendenti d
JOIN StipendiMedi s ON d.dipartimento_id = s.dipartimento_id
WHERE d.stipendio > s.stipendio_medio;
