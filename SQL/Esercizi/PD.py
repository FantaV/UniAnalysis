import pandas as pd
import numpy as np

# 1. Simula i dati o caricali da un file
data = {
    "Loan_ID": range(1, 101),
    "EAD": np.random.uniform(1000, 1000000, 100),  # Esposizione al Default
    "PD": np.random.uniform(0.001, 0.15, 100),     # Probabilità di Default
    "LGD": np.random.uniform(0.2, 0.8, 100),       # Loss Given Default
}

# Crea il DataFrame
df = pd.DataFrame(data)

# 2. Crea la colonna Risk_Category
conditions = [
    (df["PD"] < 0.02),
    (df["PD"] >= 0.02) and (df["PD"] <= 0.10),
    (df["PD"] > 0.10),
]
choices = ["Low Risk", "Medium Risk", "High Risk"]
df["Risk_Category"] = np.select(conditions, choices, default="Unknown")

# 3. Calcola il numero di prestiti e la perdita attesa totale
df["Expected_Loss"] = df["EAD"] * df["PD"] * df["LGD"]  # Calcola la perdita attesa

# Raggruppa per categoria di rischio
result = df.groupby("Risk_Category").agg(
    Number_of_Loans=("Loan_ID", "count"),
    Total_Expected_Loss=("Expected_Loss", "sum")
).reset_index()

# 4. Visualizza i risultati
print(result)