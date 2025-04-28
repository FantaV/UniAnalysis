#include <stdio.h>
#include <string.h>

int main() {
  int *p;
  int a = 0;

  p = &a;   // & punta e prende l'indirizzo
  *p = 3;   // * punta e prende il valore

  printf( "a=%d\n", a );
  printf( "p=%p\n", p );
  printf( "*p=%d\n", *p );
  (*p)++;
  printf( "a=%d\n", a );

  return 0;
}