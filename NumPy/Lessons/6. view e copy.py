# 6. VIEW e COPY

# View e Copy servono per copiare il contenuto di un array all'interno di un altro array

# COPY: fa una copia effettiva dei dati, creando quindi una copia che può essere scarabocchiata senza che l'altra venga modificata. HO LA MIA PRORPRIETÀ DEL FOGLIO 

# VIEW: è una copia "fittizia" in quanto della copia che andremo a fare né risulterà più una condivisione. 
# Ciò vuol dire che se mi venisse passato un documento copiato con view e l'altro utente ha scarobocchiato, io vedrei lo scarabocchio. HO LA VISTA SUL FOGLIO MA NON LA PROPRIETÀ

import numpy as np 


# COPY 

arr = np.array([1,2,3])
arrCopy = arr.copy()

print(arr)
print(arrCopy)      # I valori mandati a schermo sono gli stessi 


# Modifica dell'indice 0

arr = np.array([1,2,3])
arrCopy = arr.copy()

arr[0] = 10

print(arr)
print(arrCopy)      # Mandando a schermo soltanto "arr" risulterà con l'indice 0 modificato mentre "arrCopy" resterà uguale a prima. Questo perché la copia e l'originale sono 2 entità distinte



# VIEW

arr1 = np.array([1,2,3])
arrView = arr.view()

arr1[0] = 10

print(arr1)
print(arrView)      # Entrambi i fogli risulteranno modificati 


# La logica di VIEW è la seguente: ARRAY è il proprietario dei dati, arrView invece è solo una vista di "arr1". Quindi se la base di Arr1 cambia anche arrView cambierà


# CHECK: ha la funziona di farci vedere se qualcuno è proprietario dei dati oppure no 

# Con COPY
arr = np.array([1,2,3])
arrCopy = arr.copy()

print(arr.base)
print(arrCopy.base)             # i valori mandati a schermo sono NONE per entrambi i formati. Questo ci indica che arr e arrCopy hanno due basi distinte

# Con VIEW
arr1 = np.array([1,2,3])
arrView = arr.view()

print(arr1.base)
print(arrView.base)             # i valori mandati a schermo sono NONE e 1,2,3. Questo ci indica che arrView ha come base arr1