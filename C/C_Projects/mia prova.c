# include <stdio.h>

int main() {

  int payCode;
  printf("Insert a pay code: ");
  scanf("%d", &payCode);

  if ( payCode >= 4 ) {
    printf( "Hai un bonus" );
  }
  else {
    printf( "Non hai ottenuto il bonus" );
  }
  return 0;
}