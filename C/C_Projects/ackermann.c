#include <stdio.h>

int ackermann( int m, int n )
{
  if ( m < 0 || n < 0 ) return -1; // ack non è definita per valori interi negativi

  if ( m == 0 ) return n + 1;
  else if ( n == 0 ) return ackermann( m - 1, 1 );
  else return ackermann( m - 1, ackermann( m, n - 1 ) );
}
 int main()
    {
   int m, n;
   printf("Inserisci un intero m >= 0: ");
   scanf("%d", &m);
   printf("Inserisci un intero n >= 0: ");
   scanf("%d", &n);
   printf("Ackermann( %d, %d ) = %d!\n", m, n, ackermann( m, n ) );
   return 0;
    }