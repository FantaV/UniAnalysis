# 3. INDICIZZAZIONE DEGLI ARRAY

# L'indicizzazione degli array serve per prendere degli elementi contenuti negli array

# NON APPLICABILE PER ARRAY 0D

import numpy as np

# Gli array di numpy funzionano allo stesso modo che per le liste


# ARR1D
arr1D = np.array([1,2,3,4,5])

print(arr1D[4])

# ARR2D
arr2D = np.array([[1,2,3,4,5],
                  [6,7,8,9,10]])

print(arr2D[0,2])   # 0 sta per il livello, 2 sta per l'indice


# ARR3D
arr3D = np.array([[[1,2,3],[4,5,6]],[[7,8,9],[10,11,12]]])
# indice array:           0                 1
# livello array       0       1         0         1
# indice valore:                               0  1  2

print(arr3D[1,1,2])     # il primo 1 sta per l'indice dell'array, il secondo 1 sta per il numero dell'array, il 2 sta per l'indice del contenuto



# INDICIZZAZIONE NEGATIVA: ovvero la possibilità di prendere gli elementi all'indietro (-1, -2, -3...)

# ARR1D
arr1D = np.array([1,2,3,4,5])

print(arr1D[-1])


# ARR2D 
arr2D = np.array([[1,2,3,4,5],
                  [6,7,8,9,10]])

print(arr2D[-1,0])      # -1: livello array; 0: indice valore 


# ARR3D
arr3D = np.array([[[1,2,3],[4,5,6]],[[7,8,9],[10,11,12]]])

print(arr3D[-1,-1,-1])