# 12. CLASSE RANDOM

import numpy as np
from numpy import random

x = random.randint(100)      # vogliamo un numero random compreso rtra 0 e 100 
x1 = random.rand()           # vogliamo un numero random tra 0 e 1 

print(x)
print(x1)

# CREARE UN ARRAY CON SIZE
x3 = random.randint(100, size=5)    # chiediamo un array di 5 elementi random compresi tra 0 e 100 

print(x3)

# ARRAY 2D 
x4 = random.randint(100, size=(3, 5)) 

print(x4)


# PIÙ ELEMENTI CON .RAND 
x5 = random.rand(5)             # la differenza di sintassi è che mentre nell'intero dobbiamo specificare il limite superiore (100) in rand è automatico l'output tra 0 e 1, specificando così solo la size 

print(x5)


# CHOICE: utilizzato per estrarre valore e generare array
x6 = np.array([1,2,3,4,5,6])

print(random.choice(x6))        


# Choice è molto funzionale se combinato con size per creare array basati solo gli elementi che ci servono
x7 = np.array([10, 20, 30, 40, 50])
arrChoice = random.choice(x7, size=(5, 2))

print(arrChoice)


# DATA DISTRIBUTION CON PROPABILITÀ
values = np.array([10, 20, 30, 40, 50])
probability = np.array([0.1, 0.2, 0.2, 0.3, 0.2])   # questi elementi identificano il peso percentuale che avrà ogni elemento inserito nell'array. 0.3 corrisponde al 30% ed è legato a 40 ecc...
                                                    # la distribuzione di probabilità deve risultare sempre 1, cioè 100 e gli elementi devono essere di pari numero degli elementi degli array
arrProb = random.choice(values, p=probability, size=(20))   # su 20 elementi con il peso stabilito verranno mandati a schermo i valori indicati 

print(arrProb)


# SHUFFLE: mischia gli elementi dell'array quindi AGISCE SULL'ARRAY 
arr = np.array([1,2,3,4,5,6])

random.shuffle(arr)

print(arr)


# PERMUTATION: stessa cosa di shuffle ma con la differenza che RITORNA NUOVO ARRAY
arr2 = np.array([1,2,3,4,5,6])

arrPerm = random.permutation(arr2)      # volendo possiamo anche non riassegnarlo dandogli lo stesso nome dell'array principale, quindi arr = random.perm ecc...

print(arrPerm)
