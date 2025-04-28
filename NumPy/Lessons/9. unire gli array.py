# 9. UNIRE GLI ARRAY 

import numpy as np

# CONCATENATE 1D

arr1 = np.array([1,2,3])
arr2 = np.array([4,5,6])

arrConc = np.concatenate((arr1, arr2))      # doppie parentesi!

print(arrConc)


# CONCATENATE 2D

arr2D1 = np.array([['giumangi', 'python',3],[99, 'pollini', 45]])
arr2D2 = np.array([['quadrato', 'garden', 22],['minestrone', 80, 21]])

arrDconc =  np.concatenate((arr2D1, arr2D2))

print(arrDconc)     # in questo primo metodo viene mandato a schermo come sopra: AXIS = 0 (se non si scrive niente allora axis = 0)


# Se si vuole mandare a schermo in altro modo andiamo a specificare l'axis (asse)

arr2D1 = np.array([['giumangi', 'python',3],[99, 'pollini', 45]])
arr2D2 = np.array([['quadrato', 'garden', 22],['minestrone', 80, 21]])

arrDaxis =  np.concatenate((arr2D1, arr2D2), axis= 1)       # con axis = 1 andremo a dire che gli INTERNI DEGLI ARRAY ANDRANNO CONCATENATI SEGUENDO IL LORO ORDINE.
                                                            # perciò avremo livello 1 del primo arr1D1 con il primo livello di arr2D2
print(arrDaxis)                                             # Tuttavia può risultare CONFUSO quindi bisogna ANTICIPARE IL RISULTATO OTTENUTO 


# NOTA SU AXIS: se abbiamo un array con una dimensione, se inserissimo AXIS = 1 dopo CONCATENATE, darebbe ERRORE, perché non ci sarebbe un array con indice 1 che corrispende alla seconda dimensione 


# FUNZIONE STACK (stack, hstack, vstack, dstack). 
# Gli ultimi 3 ci permettono di fare le stesse cose di stack ma in modo individale, preciso ed isolato rispetto a STACK

# STACK
arr1S = np.array([1,2,3])
arr2S = np.array([4,5,6])

arrConc2 = np.concatenate((arr1S, arr2S))   # lo inserisco per evidenziare le differenze nella stampa 
arrStack = np.stack((arr1S, arr2S))

print(arrConc2)
print(arrStack)             # DIFFERENZA: mentre concatenate manda schermo unendo gli array, STACK CREERÀ UN UNICO ARRAY MA SU DUE DIMENSIONI 


arr1Sax = np.array([1,2,3])
arr2Sax = np.array([4,5,6])

arrStackAx = np.stack((arr1S, arr2S), axis=1)     # nonostante abbiamo 1 dimensione, con stack possiamo anche specificare l'axis senza mandare in errore

print(arrStackAx) 


# HSTACK: horizontal stack. Andrà a fare la stessa cosa di concatenate
arr1HS = np.array([1,2,3])
arr2HS = np.array([4,5,6])

arrHS = np.hstack((arr1HS, arr2HS))

print(arrHS)


# VSTACK: vertical stack. Andrà a fare la stessa cosa di Stack su axis = 0 (quindi quello base)
arr1VS = np.array([1,2,3])
arr2VS = np.array([4,5,6])

arrVS = np.vstack((arr1VS, arr2VS))

print(arrVS)


# DSTACK: death stack. Andrà a fare la stessa cosa di stack ma nella SECONDA dimensione quindi axis = 1
arr1DS = np.array([1,2,3])
arr2DS = np.array([4,5,6])

arrDS = np.dstack((arr1DS, arr2DS))

print(arrDS)
