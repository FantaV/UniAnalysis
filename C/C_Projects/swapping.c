#include<stdio.h>
#include<stdlib.h>

int main() {
  //dichiarazione variabili
  int flag, m, app, n = 10, i; // v[10];

  /* fase di input
  for (i = 0; i < n; i++) {
    printf("Inserisci vettore[%d]\n", i);
    scanf("%d", &v[i]);

  } */
  int v[10] = { 4,6,0,1,5,4,2,1,8,9};
  // ordinamento
  m = n;                // m tiene traccia della dimensione originale dell'array
  do {
    flag = 0;           // sentinella: mi dice se l'array è ordinato oppure no
    for ( i = 0; i < n - 1; i++ ) {
      if ( v[i] > v[ i + 1 ] ) {

        // SWAPPING: ad ogni passata si confrontano le coppie successive di elementi e se non sono in ordine vengono scambiate
        app = v[i];
        v[i] = v[i + 1];
        v[i + 1] = app;
        flag = 1;
      }
    }
    n--;                // riduciamo la dimensione effettiva per ottimizzare il ciclo
  }
  while ( flag == 1 ); // finché ci sono scambi

  // output
  for ( i = 0; i < m; i++)
    printf("v[%d] = %d\n", i, v[i] );
  return 0;
}