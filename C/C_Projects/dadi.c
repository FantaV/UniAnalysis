#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int lanciaDadi( void );

int main() {
  int statoGiocatore, esitoLancio, punteggio;

  srand( time( NULL ) );
  esitoLancio = lanciaDadi();  // primo lancio dei dadi
  switch ( esitoLancio ) {
    case 7: case 11:          // vince al primo lancio
      statoGiocatore = 1;     // 1 vince, 2 perde
      break;
    case 2: case 3: case 12:  // perde al primo lancio
      statoGiocatore = 2;
      break;
    default:                 // memorizza il punteggio
      statoGiocatore = 0;
      punteggio = esitoLancio;
      printf( "Il tuo punteggio è %d\n", punteggio );
  }
  while( statoGiocatore == 0 ) {  // continua a lanciare
    esitoLancio = lanciaDadi();
    if ( esitoLancio == punteggio ) // vince se uguali
      statoGiocatore = 1;
    else if ( esitoLancio == 7 )    // perde ottenendo un 7
      statoGiocatore = 2;
  }
  if ( statoGiocatore == 1 ) printf( "Hai vinto\n" );
  else printf( "Hai perso\n" );
  return 0;
}

int lanciaDadi( void ) {
  int dado1, dado2, sommaDadi;

  dado1 = 1 + (rand() % 6 );
  dado2 = 1 + (rand() % 6 );
  sommaDadi = dado1 + dado2;
  printf( "Hai ottenuto %d + %d = %d\n", dado1, dado2, sommaDadi );
  return sommaDadi;
}

//
// Created by Carlo Villoresi on 12/01/25.
//
