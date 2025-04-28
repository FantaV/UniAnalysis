# 10. DIVIDERE GLI ARRAY

import numpy as np 

# ARRAY_SPLIT 1D
arr = np.array([1,2,3,4,5,6])

arrAS = np.array_split(arr, 2) 
arrAS = np.array_split(arr, 3) 

print(arrAS)            # chiediamo di splittare l'array indicato in 2 
print(arrAS)           # qui chiediamo per 3 


# SPLIT 1D (utilizzato senza array davanti)
arr2 = np.array([1,2,3,4,5,6])

arrSplit = np.split(arr2, 2) 
arrSplit2 = np.split(arr2, 3) 
# arrSplit4 = np.split(arr2, 4)     Inserito come commento perché darebbe errore in quanto non abbiamo un array divisibile per il numero indicato (abbiamo 6 elementi)

print(arrSplit)            # chiediamo di splittare l'array indicato in 2 
print(arrSplit2)           # qui chiediamo per 3 


# ACCEDERE AGLI ELEMENTI DEGLI ARRAY INTERNI 
arr3 = np.array([1,2,3,4,5,6])

arrAS2 = np.array_split(arr3, 4)

print(arrAS2[3])     # si indica l'indice. IMPORTANTE: il numero di indice non può essere superiore al numero di split richiesto altrimenti da errore di OUT OF RANGE 


# ARRAY_SPLIT 2D
arr4 = np.array([[1,2],[3,4],[5,6],[7,8],[9,10],[11,12]])

arrAS2D = np.array_split(arr4, 3)

print(arrAS2D)


# SPLIT SUGLI ASSI 
arr5 = np.array([[1,2],[3,4],[5,6],[7,8],[9,10],[11,12]])

arrAX = np.array_split(arr5, 3, axis=1)

print(arrAX)


# VSPLIT
arr6 = np.array([[1,2,3],[4,5,6],[7,8,9],[10,11,12],[13,14,15],[16,17,18]])

arrVS = np.vsplit(arr6, 3)

print(arrVS)


# HSPLIT
arr7 = np.array([[1,2,3],[4,5,6],[7,8,9],[10,11,12],[13,14,15],[16,17,18]])

arrHS = np.hsplit(arr7, 3)

print(arrHS)


# DSPLIT
arr8 = np.array([[[1,2,3],[4,5,6]],[[7,8,9],[10,11,12]],[[13,14,15],[16,17,18]]])

arrDS = np.dsplit(arr8, 3)

print(arrDS)