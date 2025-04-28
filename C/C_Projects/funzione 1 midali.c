#include <stdio.h>
#include <stdbool.h>
#include <string.h>

// DICHIARAZIONE FUNZIONE
void saluta() {
  printf( "ciao sono Carlo!\n");        // blocco di codice in cui possiamo fare ciò che vogliamo (for, if, while ecc...)
}

int somma( int x, int y ) {            // x ed y sono dei PARAMETRI che devono essere definiti in fase di chiamata
    int risultato = x + y + 5;
    printf( "%d\n", risultato );
    return risultato;
}

int main()
{
  saluta();
  saluta();
  somma(10, 40);        // definizione ARGOMENTI
  somma(99, 102);

  int z = somma( 34, 88);
  printf( "%d\n", z );

  return 0;
}