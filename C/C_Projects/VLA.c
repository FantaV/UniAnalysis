// VLA ( Variable-Lenght-Array )

#include <stdio.h>

int main() {
  int dimensioneArray;

  printf("Inserisci la dimensione dell'array\n");
  scanf("%d", &dimensioneArray);

  // Lo spazio di memoria veine allocato in fase di esecuzione
  int array[dimensioneArray];
  printf("La dimensione dell'array è: %d\n", sizeof(array));

  return 0;
}