SELECT cliente,esposizione AS EAD FROM clienti_rischio;
SELECT cliente,LGD FROM clienti_rischio;

-- EL
SELECT cliente,
	PD_originale * esposizione * LGD AS EL 
FROM clienti_rischio;

-- UL
SELECT cliente,
	SQRT(EAD*(1-LGD)) *2,33 AS UL
from clienti_rischio;

-- VaR(99%)
SELECT cliente,
	esposizione*LGD*2.33 AS VaR
FROM clienti_rischio;

-- STRESS TEST
SELECT cliente,
PD_originale * 1.20 AS PD_stressata,
esposizione * (PD_originale * 1.20) * LGD AS Stress_Test
FROM clienti_rischio;

WITH CTE AS ( SELECT cliente,
esposizione AS EAD,
LGD,
PD_originale,
PD_originale * 1.20 AS PD_stressata
FROM clienti_rischio
)

SELECT 
cliente, 
EAD,
LGD,
ROUND(PD_originale * LGD * EAD, 3)AS EL,
ROUND(SQRT(EAD*(1-LGD))*2.33, 3) AS UL,
ROUND(EAD * 2.33 * LGD, 3) AS VaR,
ROUND(EAD * PD_stressata * LGD, 3) AS Stress_Test
FROM CTE;

