#include <stdio.h>
#define STUDENTI 3
#define ESAMI 4

// PROTOTIPI DELLE FUZIONI
int minimo( const int [] [ ESAMI ], int, int );
int massimo( const int [] [ ESAMI ], int, int );
double media( const int [], int );
void stampaARRAY( const int [] [ ESAMI ], int, int );

int main() {
  int studente;
  const int votiStudenti[ STUDENTI ][ ESAMI ] =
    {{ 77, 68, 86, 73 },
     { 96, 87, 89, 78 },
     { 70, 90, 86, 81 }
    };

  printf( "Il vettore dei voti è:\n" );
  stampaARRAY( votiStudenti, STUDENTI, ESAMI );  // chiamata della funzione stampa
    return 0;
}

// DEFINIZIONE DELLA FUNZIONE stampaARRAY
void stampaARRAY( const int voti[] [ ESAMI ], int studenti, int esami )
    {
    // Attraversa le righe (studenti)
    for ( int studente = 0; studente < studenti; studente++ )
        {
        printf("Studente %d: ", studente + 1);

        // Attraversa le colonne (esami)
        for ( int esame = 0; esame < esami; esame++ ) {
            printf( "%d ", voti[studente][esame] );
        }
        printf("\n"); // Passa alla riga successiva
    }
}

// implementare in autonomia il resto del codice