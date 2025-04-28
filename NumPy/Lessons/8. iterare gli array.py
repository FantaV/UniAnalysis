# 8. ITERARE GLI ARRAY

# Iterare gli array è per intenderci lo stesso che per il CICLO FOR. Passare quindi in rassegna ogni elemento per mandare a schermo gli elementi 

import numpy as np

arr1D = np.array([1,2,3,4,5,6])

for x in arr1D:
    print(x)


# Le grane cominiciano quando abbiamo più dimensioni

arr2D = np.array([[1,2,3],[4,5,6]])
#                   x       x
for x in arr2D:          # ATTENZIONE, ora la x non è più elemento contenuto ma i livelli della dimensione
    for y in x:
        print(y)         # Quindi per iterare più dimensioni dobbiamo inserire un FOR dentro un altro for 

    
arr3D = np.array([[[1,2,3],[4,5,6],[7,8,9],[10,11,12]]])

for x in arr3D:
    for y in x:
        for z in y:
            print(z)


# NDITER: se vogliamo fare tutto in volta sola

# NDITE: sta per N = numero 
#                D = dimensione
#                ITE = iterazione

arrNDITER = np.array([[[1,2,3],[4,5,6],[7,8,9],[10,11,12]]])

for x in np.nditer(arrNDITER):
    print(x)


# NDITER ci da la possibilità di fare molte cose. Tra queste abbiamo:

# 1. CAMBIARE TIPO DI DATO
arrFLAG = np.array([[[1,2,3],[4,5,6],[7,8,9],[10,11,12]]])

for x in np.nditer(arrFLAG, flags=['buffered'], op_dtypes=['S']):
    print(x)

# il FLAGS BUFFERED è obbligatorio per attivare l'OPTYPES
# ODTYPES: ci permette di dire che il tipo dell'output (op) non è più un intero ma è una STRINGA 


# 2. ITERARE A STEP 

arrSTEP = np.array([[1,2,3,4],[5,6,7,8]])

for x in np.nditer(arrSTEP[:,::2]):
    print(x)

# i PRIMI DUE PUNTI si riferiscono agli array della prima dimensione in cui sono contenuti i 2D che sono stati presi tutti (: = prendi tutto)
# i SECONDI due punti si riferiscono all'interno della seconda dimensione che sono stati presi tutti: da 1 a 8
# i TERZI due punti seguiti dal 2 dicono: parti dall'inizio arriva fino alla fine saltando di 2 


# PRENDERE INDICE NDENUMERATE

arrindex = np.array([[1,2,3,4],[5,6,7,8]])

for idx,x in np.ndenumerate(arrindex):
    print(idx)                              # verrà mandato a schermo livello array ed indice elemento: (0,0) che si riferisce ad 1, (0,1) a 2, (0,2) a 3 ecc...


arrIndexRif = np.array([[1,2,3,4],[5,6,7,8]])

for idx,x in np.ndenumerate(arrIndexRif):
    print(idx, x)                           # così verrà mandato anche l'elemento corrispendente 


