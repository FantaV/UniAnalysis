#include <stdio.h>

int main()
    {
  // dichiarazione
  int voto, contatore, totale;
  float media;

  // inizializzazione
  totale = 0;
  contatore = 0;  // sentinella

  // elaborazione
    printf("Inserisci un voto, -1 per terminare:\n ");
    scanf("%d", &voto);
    while(voto != -1) {
      totale = totale + voto;
      contatore = contatore + 1;
      printf("Inserisci un voto, -1 per terminare:\n ");
      scanf("%d", &voto);
    }

  // fase di terminazione
    if (contatore !=0) {
    media = (float)totale/contatore;
    printf("Media = %.2f", media);
    }
    else
      printf("Non è stato inserito nessun voto");
    return 0;
    }

