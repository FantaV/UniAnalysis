#include <stdio.h>

int main() {
  int n;
  long int fattoriale;

  fattoriale = 1;

  printf( "Inserisci il numero n:\n" );
  scanf( "%d", &n );

  for ( int i = 1; i < n+1; i++ ) {
    fattoriale = fattoriale * i;
  }
  printf( "Fattoriale di %d = %ld\n", n, fattoriale );
}