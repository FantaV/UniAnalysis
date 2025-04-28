# 11. SEARCH, SORT, FILTER

import numpy as np


# SEARCH: per cercare utilizziamo WHERE nella sintassi
arr = np.array([1,2,3,4,5,4,4])

arrIndici = np.where(arr == 4)      # e volessimo cercare ik 4 contenuto nel nostro array 

print(arrIndici)


arr2 = np.array([1,2,3,4,5,6,7,8])

arrPari = np.where(arr2%2 == 0)     # e volessimo cercare i numeri pari utilizziamo il MODULO che uguale 0 restituisce numeri pari altirmenti con 1 i dispari 

print(arrPari)


# SORT: per ordinare i nostri elementi 
arr3 = np.array([6,7,3,5,9,1,5,0,2])
arr4 = np.array(['marco', 'luca','edoardo','anna'])

arrOrdinato1 = np.sort(arr3)
arrOrdinato2 = np.sort(arr4)

print(arrOrdinato1)
print(arrOrdinato2)


# SORT array 2D
arr5 = np.array([[5,9,2],[9,1,4]])

arr2DOrdinato = np.sort(arr5)

print(arr2DOrdinato)


# FILTER con esempio STATICO
arr6 = np.array([1,2,3,4])

filtroPari = [False, True, False, True]                   # andiamo a filtrare manualmente con un altro array facendo finta di volere i numeri pari

arrayFiltrato = arr6[filtroPari]

print(arrayFiltrato)


# FILTER con esempio DINAMICO
arr7 = np.array([1,2,3,4])

filtroPari2 = [] 

for element in arr7:
    if element%2 == 0:
        filtroPari2.append(True)
    else:
        filtroPari2.append(False)

arrayFiltrato2 = arr7[filtroPari2]

print(arrayFiltrato2)


# FILTER con SCORCIATOIA 
arr8 = np.array([1,2,3,4])

filtroPari3 = element%2 == 0

arrayFiltrato3 = arr8[filtroPari3]

print(arrayFiltrato3)
