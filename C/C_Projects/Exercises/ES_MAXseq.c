/* Scrivere un programma che dato un numero N>0 di valori da inserire da tastiera, stampi a video il massimo
della sequenza inserita e la posizione in cui tale valore è stato inserito.
• Supponiamo, per semplicità, che non ci siano duplicati
• Esempio: N=5 sequenza: 3, 2, 9, 5, 1 Max=9 Pos=3 */

#include <stdio.h>

int main() {
  int N, elemento, max, posizionemax, contatore = 1;

  printf("Inserisci la qunatità di valori:\n");
  scanf("%d",&N);

  // Controllo che N sia valido
  if ( N == 0 || N < 0 )
  {
    printf("Errore! Inserire un valore valido\n");

    return 1;   // Esci con codice di errore
  }

  if (N > 0)
  {
    printf("Inserire il primo valore: ");
    scanf("%d",&elemento);

    max = elemento;              // inizializzo il max al primo valore inserito
    posizionemax = contatore;    // e dichiaro la posizione dle max al contatore uno essendo un solo numero
    contatore = 2;               // il prossimo valore sarà il secondo

    while (contatore <= N)
    {
      printf("Inserire il valore numero %d: ", contatore);
      scanf("%d", &elemento);

      // Aggiorno il massimo e la posizione
      if (elemento > max)
      {
        max = elemento;
        posizionemax = contatore;
      }

      contatore++; // Incremento il contatore
    }

    printf("Il massimo e': %d\n", max);
    printf("La posizione del massimo e': %d\n", posizionemax);
  }

  return 0;
}