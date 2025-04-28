// ARRAY: collezione fissa di dati
// NON si possono eliminare o aggiungere elementi

#include<stdio.h>

int main() {
  int votiStoria[10] = {1,2,3,4,5,6,7,8,9};  // specificando il numero di elementi
  int votiGeografia[] = {1,2,3};            // senza specificazione

  printf("%d\n", votiStoria[2]);

  votiGeografia[0] = 9;       // cambio del valore indicando l'indice

  printf("Voto modificato %d", votiGeografia[0]);

  printf("\n%zu", sizeof(votiStoria));    // mi resitutisce la grandezza dell'array in BYTE, ogni INTERO vale 4 byte

  printf("\n%zu\n", sizeof(votiStoria) / sizeof(votiStoria[0]));
  // mi dice quanti elementi ci sono nell'array: size dell'array diviso il primo elemento che vale 4 byte

  //******* UTILIZZO DEL CICLO FOR per mandare a schermo gli elementi contenuti dell'array**********
  for (int i = 0; i < sizeof(votiStoria) / sizeof(votiStoria[0]); i++) {
    printf("%d\n", votiStoria[i]);
  }
  return 0;
}