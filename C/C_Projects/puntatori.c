#include <stdio.h>

int main() {
  int a;
  int *Ptr;                                             // puntatore ad un intero

  a = 7;
  Ptr = &a;                                            // assegna a aPtr l'indirizzo di a
  printf( "L'indirizzo di 'a' e: %p\n"
         "Il valore di aPtr è: %p\n", &a, Ptr );

  printf( "Il valore di 'a' è: %d\n"
          "Il valore di *Ptr è: %d", a, *Ptr );       // l'operatore * restituisce un alias della variabile puntata dall'operando.
                                                        // aPtr punta ad 'a', quindi aPtr è alias di 'a'.

  printf( "\nDimostrazione che * e & sono operatori inversi:\n"
          "&*aPtr = %p\n*&aPtr = %p\n", &*Ptr, *&Ptr );
  return 0;
}