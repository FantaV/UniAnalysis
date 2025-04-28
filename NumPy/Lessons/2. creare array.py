# 2. CREARE ARRAY 

import numpy as np

# DIMENSIONI DEGLI ARRAY: OD, 1D, 2D, 3D

# 0D: si tratta semplicemente di un array con dentro UN SOLO NUMERO

arr = np.array(42)      # SENZA parentesi quadre


# 1D
arr1D = np.array([1,2,3,4,5])


# 2D: è un array che contiene un array
arr2D = np.array([ [1,2,3,4,5], [6,7,8,9,10] ])     # parentesi viole = primo array; parentesi celesti: altri array contenuti. 

arr2D = np.array([[1,2,3,4,5], 
                  [6,7,8,9,10]])       # altro tipo di formattazione più intuitivo sembrando più una MATRICE


# 3D

arr3d = np.array([ [ [1,2,3,], [4,5,6] ],[ [7,8,9], [10,11,12] ] ]) 

# per essere più chiari: grande array e livelli inferiori
arr3D = np.array([
    [[1,2,3,], [4,5,6]],
    [[7,8,9], [10,11,12]]
    ]) 


# NDIM: lo si utilizza per mostrare le varie dimensioni (N: number DIM: dimension)

print(arr.ndim)
print(arr1D.ndim)
print(arr2D.ndim)
print(arr3D.ndim)


# COME CREARE VELOCEMENTE UN ARRAY CON PIÙ DIMENSIONI

arr5D = np.array([1,2,3], ndmin = 5)  # ndMIN per il numero di dimensioni. ATTENZIONE ALLA SINTASSI 

print(arr5D.ndim)
print(arr5D) 


# ARANGE, ZEROS e ONES: pre creare velocemnte degli array in base al range che vogliamo piuttosto che solo 0 o solo 1. Può succedere che abbiamo bisogno di creare degli array e solo successuvamente di modificarli


# ARRANGE: serve per creare un range di dati da inserire all'interno di un array 

arrArange = np.arange(5,50,5)

print(arrArange)            # Cosa abbiamo detto al programma: mi fai un RANGE array, che parte da 5, arriva a 50 ESCLUSO, incrementando di ogni 5


# ZEROS

arrZeros = np.zeros(3) # numero di zeri che vogliamo

print(arrZeros)

# ZEROS CON PIÙ ARRAY 

arrZeros2 = np.zeros((3,2)) # 3 array da due elementi 

print(arrZeros2)

# ZEROS CON PIÙ ARRAY E PIÙ LIVELLI:

arrZeros2 = np.zeros((2,2,1))  # qui abbiamo 3 dimensioni (perché ci sono 3 numeri), con 2 array ognuna, ed 1 elemento all'interno di ogni array
print(arrZeros2)


# ONES: idem che per zeros, al posto degli 0 ci saranno gli 1 

arrOnes = np.ones((2,2,1))  

print(arrOnes)