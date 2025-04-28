# 5. TIPI DI DATI

import numpy as np

# TIPI DI DATI NUMPY

# i  -   integer
# b  -   boolean
# u  -   unsigned integer
# f  -   float
# c  -   complex float
# m  -   timedelta
# M  -   datetime
# O  -   object
# S  -   string
# U  -   unicode string
# V  -   fixed chunk of memory for other type ( void )


# COME ASSEGNARE UN TIPO DI DATO, VERIFICARE IL TIPO DI DATO, CONVETIRE IL TIPO DI DATI DA ARRAY A NUMPY


# VERIFICARE IL TIPO DI DATO
arr = np.array([1,2,3,4,5])
print(arr.dtype)


arr2 = np.array(['a','b','c'])
print(arr2.dtype)



# CREARE UN ARRAY DI UN DETERMINATO TIPO: 
arr3 = np.array([1,2,3,4,5], dtype='S')

print(arr3)     # i valori mandati a schermo non saranno più dei numeri ma delle stringhe. Se si andrà quindi ad effettuare un operazione questa darà errore.


arr4 = np.array(['1','2','3'], dtype='i')

print(arr4)



# CONVERTIRE UN ARRAY ESISTENTE: astype
arr5 = np.array(['1','2','3'])
arrN = arr4.astype('i')

print(arrN)