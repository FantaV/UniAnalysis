#include <stdio.h>
#define SIZE 10
#include "ricBinaria.h"

/*
int ricercaBinaria (const int vettore[], int chiave) {
  int posMin = 0, posMax = SIZE - 1, posMedia;
  while ( posMin <= posMax ) {    // ad ogni giro l'array è ridotto
    posMedia = (posMin + posMax) / 2;
    if ( vettore[ posMedia ] == chiave )
      return posMedia;    // Trovato!
    else if (vettore[posMedia] > chiave )
      posMax = posMedia - 1;  // Cerca nella metà inferiore
    else
      posMin = posMedia + 1;  // Cerca nella metà superiore
  }
  return -1;    // valore chiave non trovato
} */

int main () {
  int vettore[10] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
  int chiave = 4;
  int risultato;

  // Chiamata della funzione ricercaBinaria
  risultato = ricBinaria(vettore, chiave);

  // Stampa il risultato
  if (risultato != -1) {
    printf("La chiave %d è stata trovata all'indice %d.\n", chiave, risultato);
  } else {
    printf("La chiave %d non è stata trovata nel vettore.\n", chiave);
  }
  return 0;
}
