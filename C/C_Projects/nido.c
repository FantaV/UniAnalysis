#include <stdio.h>

int main() {
  int promossi = 0, bocciati = 0, studente = 1, risultato;  // inizializzazione variabili

  // elaborazione risultati di 10 studenti, il contatore è dato da studente
  while ( studente <=10 ) {
    printf( "Inserisci il risultato ( 1=promosso, 2=bocciato ): " );
    scanf( "%d", &risultato );
    if ( risultato ==1 ) {
      promossi +=1;
      studente +=1;   // incrementa solo se il risultato è valido
    }
    else if (risultato == 2) {
      bocciati +=1;
      studente +=1;   // incrementa solo se il risultato è valido
    }
    else
      printf( "Valore non permesso\n" );  // NON incrementa
  }

  printf( "Promossi %d\n", promossi );
  printf( "Bocciati %d\n", bocciati );

  if (promossi >8)
    printf("Si devono aumentare le tasse");
  return 0;
}