// CRIVELLO DI ERASTOTENE
// Trova i numeri primi tra 1 ed n

#include <stdio.h>

int main() {
  int num, n;
  int flag;
  int cont;

  // INSERIMENTO DIMENSIONE
  printf("Inserisci un massimo:\n");
  scanf("%d", &n);

  // ALLOCAZIONE CON ARRAY VARIABILE
  int numeri[n];

  numeri[0] = 0; // 0 non è un numero primo
  numeri[1] = 0; // 1 non è un numero primo

  // INIZIALIZZAZIONE ARRAY CON TUTTI I NUMERI DA 1 A N
  for (int i = 0; i < n; i++)
    numeri[i] = 1;

  // ALGORITMO CHE RILASCIA SOLO I NUMERI PRIMI
  num = 1;

  do
  {
    num++;
    flag = 0;

    // TROVO IL PRIMO NUMERO NON CANCELLATO
    for (int i = 0; i < n; i++)
      if (numeri[i] != 0)
      {
        num = numeri[i];
        break;
      }

    printf("num = %d\n", num);
    for (int i = 0; i < n; i++)
    {
      if (numeri[i * num] != 0 )
      {
        numeri[i * num] = 0;
        flag = 1;
      }
    }

    // ESCE QUANDO NON HO PIÙ NUMERI PRIMI NELL'ARRAY
  }
  while (flag == 1);

  cont = 0;
  for (int i = 2; i < n; i++)
  {
    if (numeri[i] != 0)
    {
      cont++;
      printf("%d\n", numeri[i]);
    }
  }

  // STAMPA DEL TOTALE DEI NUMERI PRIMI
  printf("Totale dei numeri primi = %d\n", cont);

  return 0;
}