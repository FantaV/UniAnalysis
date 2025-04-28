// Strutture di dati

#include <stdio.h>

// Creazione della struttura. PRIMA DEL MAIN!!!
struct studente             // contrassegno della struttura (abbreviazione dei dati tra parentesi)
    {
      char nome[50];        // membro 1 della struttura
      int id;
      char cognome[20];
      int età;
      char città[30];
    } std;                  // variabile di struttura


int main()
{
      printf("Inserisci le caratteristiche dello studente\n");
      printf("---------------------------------\n");
      printf("Inserisci il nome: ");
      scanf("%s", std.nome);    //richiama struttura e nome del membro uniti dall'op '.'

      printf("Inserisci il cognome: ");
      scanf("%s", std.cognome);

      printf("Inserisci Matricola: ");
      scanf("%d", &std.id);       // operatore & poiché trattasi di int e non char che hanno già indirizzo

      printf("Inserisci l'età: ");
      scanf("%d", &std.età);

      printf("Inserisci la città: ");
      scanf("%s", std.città);

      return 0;
}