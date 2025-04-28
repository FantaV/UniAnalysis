#include <stdio.h>
#include <stdbool.h>
#include <math.h>
#include <locale.h>

int varGlobal = 17;

int main() {

  int x = 10;

          if ( x == 10 )
          {
              int x = 99;
              x = 56;                   // se assumo x come nuova variabile la x int 10 cambia valore all'interno del blocco in cui è scritta
              int y = 23;
              printf( "x = %d\n", x );
              printf( "y = %d\n", y );
              printf( "varGB = %d\n", varGlobal );  // può essere chiamata ovunque, anche nell'if
          }

    printf("x = %d\n", x);          // questa x non è uguale alla x inserita nel blocco if

    return 0;
 }