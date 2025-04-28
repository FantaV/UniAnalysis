#include <stdio.h>

int main()
    {
  // DICHIARAZIONI VARIABILI
  int contatore, voto, totale, media;

  // INIZIALIZZAZIONE VARIBIABILI
  totale = 0;
  contatore = 1;

  // ELABORAZIONE
  while (contatore <= 10)
    {
    printf("Inserisci un voto: ");
    scanf("%d", &voto);
    totale = totale + voto;
    contatore = contatore + 1;
    }
    media = totale / 10;
    printf("Media = %d", media);
    return 0;
    }