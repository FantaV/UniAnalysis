#include <stdio.h>
#include <string.h>

// una stringa è un ARRAY DI CARATTERI

int main() {
  char benvenuto[] = "Ciao, benvenuto";
  char nome[] = "edoardo";  // ogni carattere è rappresentato da un vettore di caratteri

  printf("%s\n", nome);     // print stringa
  printf("%c\n", nome[2]);  // print carattere

  // modifica carattere
  nome[5] = 'w';
  printf("%s\n", nome);

  // Loop
  for (int i = 0; i < sizeof(nome)/sizeof(nome[0]); i++) {
    printf("%c\n", nome[i]);
  }

  // oppure
  for (int i = 0; i < strlen(nome); i++) {    // affinché strlen funzioni bisogni importare string.h
    printf("%c\n", nome[i]);
  }

  // CONCATENAZIONE DI STRINGHE
  printf("%s", strcat(benvenuto," ", nome));

  return 0;
}