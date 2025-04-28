#include <stdio.h>
#define SIZE 10

int main() {
  int vettore[ SIZE ];
  float somma;
  float media;

  // INPUT
  for(int i = 0; i < SIZE; i++) {
    printf("Inserisci la componente: [%d]\n", i);
    scanf("%d", &vettore[ i ]);
  }

  // CALCOLO MEDIA
  somma = 0;
  for(int i = 0; i < SIZE ; i++) {
    somma += vettore[ i ];
  }
  media = somma / SIZE;

  // OUTPUT
  printf("Media = %.2f\n", media);
  return 0;
}




