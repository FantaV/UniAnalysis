#include <stdio.h>
#include <string.h>

// creazione stampino con tipi di dati associati alla struttura Persona
struct Persona {
  char nome[20];
  char cognome[20];
  int età;
};

// creazione funzione che manda a schermo tutti i dati di una persona
void presentati(struct Persona persona) {
  printf("Nome: %s\n", persona.nome);
  printf("Cognome: %s\n", persona.cognome);
  printf("Età persona: %d\n\n", persona.età);
}

int main() {

  // creo una variabile con un tipo di dato appartenente alla struttura persona
  struct Persona persona1;
  persona1.età = 21;    // inserisco un valore

  /* persona1.nome = "Luca";  QUESTA SINTASSI È ERRATA perchè stiamo assegnando all'array un valore non permesso */
  // dobbiamo usare una funzione per copiare l'array con stringa che contiene il nome dentro l' array definito in STRUCT PERSONA:
  strcpy(persona1.nome, "Luca");
  strcpy(persona1.cognome, "Ambrosini");

  printf("Età persona1: %d\n", persona1.età);
  printf("Nome: %s\n", persona1.nome);
  printf("Cognome: %s\n"
         "------------------\n", persona1.cognome);
  printf("Stampa funzione\n\n");

  // chiamata della funzione
  presentati(persona1);

  // stampa di una persona2
  struct Persona persona2;
  strcpy(persona2.nome, "Anna");
  strcpy(persona2.cognome, "Neri");
  persona2.età = 26;

  presentati(persona2);


  return 0;
}