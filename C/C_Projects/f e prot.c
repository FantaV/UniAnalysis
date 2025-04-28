// DICHIARAZIONE FUNZIONE CON L'USO DEL PROTOTIPO

// Trovare il max tra 3 interi

#include <stdio.h>

/* DICHIARAZIONE FUNZIONE: Prototipo

 - La dichiarazione della funzione è una sorta di “promessa” che dici al compilatore: “Questa funzione esiste e seguirà questo schema”.
 - È una linea di codice che indica il tipo di ritorno e i parametri della funzione, ma non contiene il corpo della funzione (cioè, non dice cosa fa la funzione).
 - Il prototipo di funzione è necessario quando vuoi dichiarare una funzione prima di usarla, soprattutto se la funzione è definita dopo il punto in cui viene chiamata nel codice.

*/

int CalcolaMax ( int, int , int );    // PROTOTIPO di funzione

int main() {
  int a, b, c;

  printf("Inserisci tre interi:\n");
  //scanf("%d%d%d", &a, &b, &c);
  // Controllo se l'input è valido
  if (scanf("%d%d%d", &a, &b, &c) != 3) {
    printf("Carattere non valido.\n");
    return 1;  // Esce con un errore
  }

  printf("Il valore Max è: %d\n", CalcolaMax (a,b,c));   // chiamata della funzione
  return 0;
}

/* IMPLEMENTAZIONE FUNZIONE CalcolaMax

  - L’implementazione della funzione è la parte in cui definisci effettivamente cosa fa la funzione, includendo il corpo e la logica di esecuzione.
  - La funzione vera e propria, che contiene il codice che verrà eseguito.
*/

int CalcolaMax ( int x, int y, int z ) {        // Il motivo per cui il programma può stampare correttamente il risultato
                                                // prima che la funzione CalcolaMax sia effettivamente definita è grazie al prototipo della funzione.
  int max = x;

  if (y > max) max = y;
  if (z > max) max = z;
  return max;
}
