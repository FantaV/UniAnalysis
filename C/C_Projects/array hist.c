#include <stdio.h>
#define SIZE 7

int main() {
  int n [ SIZE ] = { 19, 3, 15, 7, 11, 9, 13};
  int i, j;

  printf( "%s%13s%16s\n", "Elemento", "Valore", "Istogramma" );
  for ( i = 0; i < SIZE; i++ ) {
    printf( "%8d%13d      ", i, n[ i ] );
    for ( j = 1; j <= n[ i ]; j++ )
      printf( "%c", '*' );
    printf( "\n" );
  }
  return 0;
}