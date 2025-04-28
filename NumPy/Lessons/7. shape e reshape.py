# 7. SHAPE e RESHAPE

import numpy as np

# La shape è la FORMA del nostro array 
# Il reshape è una VIEW 

arrtab = np.array([4, 'Giulio', 22])
print(arrtab.shape)

arrtab2D = np.array([['canzoncine', 55, 'mafioso'],[33,90,21]])
print(arrtab2D.shape)


# una volta introdotto il concetto di forma con shape, POSSIAMO CAMBIARLA ATTRAVERSO RESHAPE
arrtab2DRESH = np.array([['canzoncine', 55, 'mafioso'],[33,90,21]])

print(arrtab2DRESH.reshape(-1))


# Come dimostrare che è una shape? Con una VIEW. Utilizzare quindi BASE come spiegato nella lezione 6
arrtab2DRESH = np.array([['canzoncine', 55, 'mafioso'],[33,90,21]])

print(arrtab2DRESH.reshape(-1).base)        # Mandando a schermo ci restitutirà valori e dimensioni di un altro array. Se uscisse NONE, vuol dire non ci sono collegamenti


# COMA PASSARE DA ARRAY DI 1D AD ARRAY 2D, E DA ARRAY 1D AD ARRAY 3D 

arr1D2D = np.array([1,2,3,4,5,6,7,8,9,10,11,12])

print(arr1D2D.reshape(4,3))     # 2 dimensioni = 2 numeri
                                # prima dimensione 4 numeri 
                                # il numero degli elementi contenuti negli array delle varie dimensioni è 3


arr1D3D = np.array([1,2,3,4,5,6,7,8,9,10,11,12])

print(arr1D3D.reshape(4,3,1))

# IMPORTANTE: POSSIAMO SEMPRE FARE RESHAPE? NO! PERCHÉ DOBBIAMO UTILIZZARE DELLE COMBINAZIONI CHE CI DANNO COME RISULTATO LA LUNGHEZZA DELL'ARRAY PRINCIPALE.
# Un modo intelligente per non impazzire nella conta è effettuare la moltiplicazione. Ad es: in arr1D2D il reshape è 4,3 che moltiplicato ci da 12. Se facessimo quindi 6,2 verrebbe mandato a schermo.


# DIMENSIONI SCONOSCIUTE: inserendo il -1 (MENO 1) da qualche parte in fase di print, stiamo dicendo: non so bene in queste dimensioni quante ce ne vanno, fallo tu!
# Numoy vede che vogliamo un ambiente di 2 o 3 dimensioni, a seconda di ciò che inseriamo. Sa che vogliamo un certo numero di elementi dentro ogni array, e lui fara il calcolo con esclusione;

# IMPORTANTE: NON POSSIAMO AVERE DUE -1!

arrSCON = np.array([1,2,3,4,5,6,7,8,9,10,11,12])

print(arrSCON.reshape(2,2,-1))


# FLATTENING: SPIANARE L'ARRAY: vuol dire passare da un array di due o più dimensioni ad un array ad 1 dimensione

arr2DxFLAT = np.array([['canzoncine', 55, 'mafioso'],[33,90,21], ['catorcio', 45, 'cartoccio']])

print(arr2DxFLAT.shape)  


# spianato:

arr1DFLAT = np.array([['canzoncine', 55, 'mafioso'],[33,90,21], ['catorcio', 45, 'cartoccio']])

print(arr1DFLAT.flatten())  

# oppure con reshape -1
print(arr1DFLAT.reshape(-1))  