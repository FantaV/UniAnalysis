# 1. INTRO 

# Numpy è una libreria che introduce il sistema ARRAY  ed è utilizzato per la Data Science, Data Analyst, Machine Learning Statistica ecc.. ed è la base per Pandas, Matplotlib ecc..

import numpy as np  # questo va lasciato altrimenti non riconosce ne la libreria ne l'alias

arr = np.array([1,2,3,4,5])     # Questa è una lista con metodo array che ci darò indietro un array
list = [1,2,3,4,5]

print(arr)
print(list)

# DIFFERENZA A SCHERMO CON LISTA 

arr = np.array([1,2,3,4,5])     
list = [1,2,3,4,5]

print(arr*5)
print(list*5)

# La differenza tra una normale lista e numpy è che quest'ultimo ragiona in termini numerici. Infatti basta guardare il print dell'ultimo codice