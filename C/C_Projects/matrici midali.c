#include <stdio.h>

int main() {

  // matrice 3x4: tre vettori in un unico array che contengono 4 elementi l'uno
  int matrice [3][4] = {{1, 2, 3, 4}, {5, 6, 7, 8}, {9, 10, 11, 12} };
  for ( int riga = 0; riga < 3; riga++ )
  {
    for ( int colonna = 0; colonna < 4; colonna++ )
    {
      printf( "Elemento [%d][%d] = %d\n", riga, colonna, matrice[riga][colonna] );
    }
  }
  return 0;
}