SELECT 
loan_id,
borrower_id,
EAD,
PD,
LGD,
CAST((EAD*PD*LGD) AS decimal (6,3)) AS EL,
CAST((EAD*(PD*(1-PD))*LGD) AS decimal (6,3)) AS UL
FROM 
loan_portfolio
GROUP BY 
loan_id, borrower_id, EAD, PD, LGD 
LIMIT 0,1000;

SELECT 
CAST(SUM(EAD*PD*LGD) AS decimal (9,3)) AS total_EL
FROM 
loan_portfolio;

-- simulazione stress PD +20%, LGD +10%
SELECT 
loan_id,
borrower_id,
EAD,
PD * (1 + 0.20) AS PD_stressato,
LGD * (1 + 0.10) AS LGD_stressato,
EAD*(PD*(1 + 0.20))*(LGD*(1 + 0.10)) AS EL_stressata
FROM 
loan_portfolio;

SELECT
CAST(SUM(EAD*(PD*(1 + 0.20))*(LGD*(1 + 0.10))) AS decimal (8,3)) AS total_EL_stressata
FROM 
loan_portfolio;

SELECT 
loan_type,
CAST(SUM(EAD*PD*LGD) AS decimal (7,3)) AS Tot_EL
FROM loan_portfolio
GROUP BY loan_type;

SELECT 
CASE
WHEN PD < 0.02 THEN 'basso_rischio'
WHEN PD BETWEEN 0.02 AND 0.01 THEN 'medio_rischio' 
ELSE 'alto_rischio'
END AS categoria_di_rischio,
COUNT(*) AS numeri_di_prestiti
FROM loan_portfolio
GROUP BY 
categoria_di_rischio;

SELECT 
    COUNT(*) AS High_Risk_Loans,
    SUM(EAD * PD * LGD) AS Total_EL_High_Risk
FROM 
    (
        SELECT 
            Loan_ID, 
            EAD, 
            PD, 
            LGD
        FROM 
            Loan_Portfolio
        WHERE 
            PD > 0.10  -- Condizione per alto rischio
    ) AS High_Risk_Subquery;