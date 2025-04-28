# 4. SLICING DEGLI ARRAY

# Lo slicing lo si utilizza per prendere valori definiti che possono andare da x a y, da x fino alla fine, o dall'inizio fino ad x


import numpy as np

# DA X  a Y
arr = np.array([1,2,3,4,5])

(print(arr[0:2]))       # indice iniziale INCLUSO, indice finale ESCLUSO


# DA X FINO ALLA FINE
arr2 = np.array([1,2,3,4,5,6,7])

print(arr2[4:])


# DALL'INIZIO FINO AD X
arr3 = np.array([1,2,3,4,5,6,7])

print(arr3[:5])


# PRENDERLI TUTTI: due punti
arr4 = np.array([1,2,3,4,5,6,7])

print(arr4[:])


# SLICE NEGATIVO: funziona come l'indicizzazione
arr5 = np.array([1,2,3,4,5])

print(arr5[-4:-1])          # IMPORTANTE: se invertissimo - 4 e -1 ci darebbe una lista vuota come risultato. 
                            # Questo perché il numero di sinistra deve stare sempre a sinistra mentre il numero di destra deve stare sempre a destra
                            # Quindi il sinistro deve essere PIÙ GRANDE(più piccolo) del destro


# STEP: possiamo prendere degli elementi in un range con la possibilità di SALTARE degli elementi

# Nello step dobbiamo definire 2 parametri: IL RANGE e LO STEP 

arrStep = np.array([1,2,3,4,5,6,7,8,9,10])

print(arrStep[0:-1:2])      # lo 0 indica il punto di partenza, il -1 il punto di fine, il 2 è lo step ed indica la size dell'incremento tra una cifra ed un altra



# SLICING ARRAY 2D e 3D

# 2D
arr2D = np.array([[1,2,3,4,5],
                  [6,7,8,9,10]])

print(arr2D[1, 1:])


# 3D
arr3D = np.array([[[1,2,3],[4,5,6]],[[7,8,9],[10,11,12]]])

print(arr3D[1, 0, :])


# AGGIUNGERE STEP ALLE DIMENSIONI 
arr3DSTEP = np.array([ [ [1,2,3,4,5,6,7,8,9,10],[1,2,3,4,5,6,7,8,9,10] ],[ [1,2,3,4,5,6,7,8,9,10],[1,2,3,4,5,6,7,8,9,10] ] ])

print(arr3DSTEP[0,0,::2])       # I primi : indicano di dover prendere tutti gli elementi dell'array della dimensione indicata. I secondi : fungono da separatore per la sintassi step. Il 2 indica il salto

