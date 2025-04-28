// CALCOLO FATTORIALE CON PROGRAMMAZIONE MODULARE

#include <stdio.h>

long int  CalcolaFattoriale ( int n )   // int n: PARAMETRO FORMALE
    {
  long int m;

  m = 1;                             // m: ACCUMULATORE
  for ( int i = 1; i < n+1; i++ )   // i: variabile CONTATORE
    {                               // for ( inizializzazione, condizione, aggiornamento )
    m = m * i;
    }
    return m;
    }

/*
Esempio: se n = 4, il ciclo si comporta così:
    •	Iterazione 1: i = 1, m = 1 * 1 = 1
    •	Iterazione 2: i = 2, m = 1 * 2 = 2
    •	Iterazione 3: i = 3, m = 2 * 3 = 6
    •	Iterazione 4: i = 4, m = 6 * 4 = 24
 */

// ********** SECONDA PARTE **********

    int main() {

      int n;
      long int fattoriale;

      printf( "Inserisci il numero n:\n" );
      scanf( "%d", &n );

      fattoriale = CalcolaFattoriale ( n );   // calcolo del fattoriale del numero n scelto

      printf ( "Fattoriale di %d = %d\n", n, fattoriale );
    }
