# 13. UFUNC: Universal Function

# sono delle funzioni che agiscono sull'oggetto ND.ARRAY ovvero l'oggetto utilizzato per creare gli array in numpy 

import numpy as np 


# FUNC
arr = np.array([0, 5, 10, 15, 20])

def addCinque(x):       # la 'x' indica l'array
    return x + 5       

# registrare la funzione: FONDAMENTALE affinché venga riconosciuto l'utilizzo di numpy e i valori non vengano interpretati come list ma possono essere vettorializzati
addCinque = np.frompyfunc(addCinque, 1, 1)  # il primo 1 indica l'elemento in INPUT in questo caso solo uno perché abbiamo solo la x
                                            # il secondo 1 indica quanti elementi abbiamo in OUTPUT, in questo caso solo l'array + 5

print(addCinque(arr))


# ADD: andrà ad effettuare la somma 
arr1 = np.array([0, 5, 10, 15, 20])
arr2    = np.array([25, 30, 35, 40, 45])

arrAdd = np.add(arr1, arr2)

print(arrAdd)


# SUBTRACT: sottrazione 
arr3 = np.array([0, 5, 10, 15, 20])
arr4    = np.array([25, 30, 35, 40, 45])

arrSub = np.subtract(arr3, arr4)

print(arrSub)


# MULTIPLY, DIVIDE, POWER 
arr5 = np.array([0, 5, 10, 15, 20])
arr6    = np.array([25, 30, 35, 40, 45])

arrMul = np.multiply(arr5, arr6)
arrDiv = np.divide(arr5, arr6)
arrPow = np.power(arr5, arr6)

print(arrMul)
print(arrDiv)
print(arrPow)


# MOD/REMAINDER: stesso nome per dire che vogliamo il resto 


# TRUNC e FIX:  vanno a troncare e ci restiuisce il numero intero senza la virgola, quindi non manda a schermo il float
arr7 = np.trunc([4.64665, 2.4111])

print(arr7)


# AROUND, CEIL e FLOOR: arrotondano per eccesso
arr8 = np.around([4.64665, 2.4111])  # arrotonda per eccesso

print(arr8)