// il programma deve calcolare e restituire la distribuzione di frequenza delle varie votazioni

#include <stdio.h>

int main() {
  int voto; //dichiarazione variabile
  int aCount = 0, bCount = 0, cCount = 0, dCount = 0, eCount = 0, fCount = 0, gCount = 0;   // inizializzazione

  printf( "Inserisci la lettera del voto: \n" );
  printf( "Oppure digita EOF (Ctrl D) per finire l'input\n " );  // EOF (End of File: Ctrl D su Mac e Linux, Ctrl Z su Windows

  while ( ( voto = getchar() ) != EOF ) {     // GETCHAR è la funzione per acquisire un carattere, che poi viene assegnato alla variabile voto
    switch ( voto ) {     // SWITCH è come il cameriere che guarda cosa hai scelto. Qui è nidificato nel while. Lo si utilizza quando una variabile deve essere confrontata con molteplici possibili valori
      case 'A': case 'a':   // CASE è come un menù: sia A che a piccolo vengono valutati come aCount
        ++aCount;
        break;              // BREAK consente l'uscita dal blocco
        case 'B': case 'b':
          ++bCount;
          break;
          case 'C': case 'c':
            ++cCount;
            break;
            case 'D': case 'd':
              ++dCount;
              break;
              case 'E': case 'e':
                ++eCount;
                break;
                case 'F': case 'f':
                  ++fCount;
                  break;
                  case 'G': case 'g':
                    ++gCount;
                    break;

                  case '\n': case ' ':  // ignora i caratteri vuoti
                    break;
                  default:              // DEFAULT è ciò che succede se non scegli nulla di valido: respinge tutti gli altri caratteri
                    printf( "Hai inserito un carattere non valido. Inserisci un'altra lettera.\n" );
                    break;
    }
  }

  printf( "\nI totali per ogni voto in lettera sono:\n" );
  printf( "A: %d\n" , aCount );
  printf( "B: %d\n" , bCount );
  printf( "C: %d\n" , cCount );
  printf( "D: %d\n" , dCount );
  printf( "E: %d\n" , eCount );
  printf( "F: %d\n" , fCount );
  printf( "G: %d\n" , gCount );
  return 0;
}

