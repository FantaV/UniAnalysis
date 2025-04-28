// DO

# include <stdio.h>

int main() {
  int counter = 1;

  do {                            // la condizione di iterazione viene controllata dopo l'esecuzione delle istruzioni del ciclo
    printf( "%d\n", counter );
  } while ( ++counter <= 10 );    // l'istruzione incremente prima il contatore e POI fa il confronto con il valore 10

  int x;

  for ( x = 1; x <= 10; x++ ) {
    if ( x == 5 )
      continue;                   // salta l'iterazione indicata e procede con quella successiva

    printf( "%d ", x );
  }
  printf ( "\nSi usa continue per saltare la stampa del 5\n" );
  return 0;
}