#include <stdio.h>
#include <stdbool.h>
#include <math.h>
#include <locale.h>

int main() {
  setlocale( LC_ALL, "it_IT.UTF-8" );

  int giornoSettimana;

  printf( "Inserisci il numero del giorno della settimana (1-7): \n" );
  scanf( "%d", &giornoSettimana );

  switch ( giornoSettimana ) {
    case 1:
      printf( "lunedì" );
      break;
        case 2:
          printf( "martedì" );
          break;
            case 3:
              printf( "mercoledì" );
              break;
                case 4:
                  printf( "giovedì" );
                  break;
                    case 5:
                      printf( "venerdì" );
                      break;
                        case 6:
                          printf( "sabato" );
                          break;
                            case 7:
                              printf( "domenica" );
                              break;
                      default:
                        printf( "Non ci sono più di 7 giorni nella settimana" );
  }
return 0;
}
