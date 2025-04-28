#include <stdio.h>

int main() {
  float min;
  float max;
  float numero;
  int n;
  int i;

  printf( "Quanti numeri sono da valutare?\n" );
  scanf( "%d", &n );
  for ( i = 0; i < n; i++ ) {

    printf( "Inserisci numero: [%d]\n", i );
    scanf( "%f", &numero );
      if ( i == 0 ) {
        min = numero;
        max = numero;
        continue;
      }

      if ( numero > max )   // la variabile max si aggiorna ad ogni ciclo, quindi quando finisce prende il valore numero più alto come max
        max = numero;
      else if ( numero < min )
        min = numero;
  }

  printf ( "max = %.2f, min = %.2f", max, min);
  return 0;
}