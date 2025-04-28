// NON STAMPA MODA

#include <stdio.h>
#include <stdlib.h>
#define SIZE 99

void trovaMedia ( const int [] );
void trovaMediana ( int [] );
void trovaModa ( int [], const int [] );            // prototipi delle funzioni
void bubbleSort ( int [] );
void stampaVettore ( const int [] );

int main () {
  int frequenze[10] = { 0 };    // dichiarazione array di interi che può contenere 10 valori, tutti inizializzati a 0
                                // dato che non sono specificati i valori degli altri 9
  int vettoreDati[ SIZE ] =
  {6 ,7 ,7 ,7 ,6 ,6 ,8, 8, 8,7,6 ,5 ,5, 7, 7, 6, 5, 5, 7, 6, 5, 6, 6, 5, 5, 6, 7, 7, 8,7, 7, 5, 4, 2, 9, 9, 8, 7,
  4, 4, 4, 3, 3, 2, 2, 3, 4, 6, 7, 8, 9, 7, 6, 5, 5, 4, 4, 3, 3, 1, 1 ,1 ,1 ,2 ,3 , 4, 1 , 1, 9, 6, 5, 6, 7, 5, 4,3};
 // dichiarazione array vettoreDati

  trovaMedia( vettoreDati );
  trovaMediana( vettoreDati );                // chiamata delle funzioni
  trovaModa( vettoreDati, frequenze );
  return 0;
}

void trovaMedia ( const int vettore[] ) {
    int j, totale = 0;

    printf( "%s\n%s\n", "Media", "************");
    for ( j = 0; j < SIZE; j++ )      // j è usata come indice del ciclo for
        totale += vettore[ j ];          // totale è usato per accumulare la somma dei valori dell’array vettore.
    printf( "La media è uguale alla somma totale degli elementi"
        "\ndiviso per il numero di elementi ( %d ).\n"
        "Qui la media vale: %d / %d = %.4f\n\n",
        SIZE, totale, SIZE, (double) totale / SIZE ); // double permette di convertire totale che è un int, in decimale
}

void trovaMediana ( int vettore[] ) {
    printf( "\n%s\n%s\n%s", "Mediana", "************",
        "Il vettore con i dati ordinati è: ");
    stampaVettore( vettore );
    bubbleSort ( vettore );
    printf( "\n\nIl vettore ordinato è: ");
    stampaVettore( vettore );
    printf( "\n\nLa mediana è l'elemento %d del vettore ordinato."
        "\nQui la mediana vale: %d\n\n",
        SIZE / 2, vettore[ SIZE/2 ] );
}

void trovaModa ( int freq[], const int vettore[] ) {
    int dato, j, h, maxFreq = 0, moda = 0;

    printf( "%s\n%s\n", "Moda", "************");
    for ( dato = 1; dato <= 9; dato++) freq[ dato ] = 0;
    for ( j = 0; j < SIZE; j++ ) ++freq[ vettore[ j ] ];
    printf( "%s%11s%19s\n\n%54s\n%54s\n\n",
        "Elemento", "Frequenza", "Istogramma",
        "1  1   2   2", "5  0   5   0   5" );
    for ( dato = 1; dato <= 9; dato++ ) {
        printf( "%8d%11d            ", dato, freq[ dato ] );
        if ( freq[ dato ] > maxFreq ) {
            maxFreq = freq[ dato ];
            moda = dato;
        }
        for ( h = 1; h <= freq[ dato ]; h++)
            printf( "*" );
        printf( "\n" );
    }
    printf( "La moda è l'elemento più ricorrente nel vettore.\n"
        "Qui la moda è %d che nel vettore è presente ben %d volte.\n", moda, maxFreq);
}

void bubbleSort ( int vettore[] ) {
    int pass, j, temp;

    for ( pass = 1; pass <= SIZE; pass++ )
        for ( j = 0; j < SIZE; j++ )
            if ( vettore[ j ] > vettore[ j + 1 ] ) {
                temp = vettore [ j ];
                vettore [ j ] = vettore[ j + 1 ];
                vettore[ j + 1 ] = temp;
            }
}

void stampaVettore ( const int vettore[] ) {
    int j;

    for ( j = 0; j < SIZE; j++ ) {
        if ( j % 20 == 0 ) printf( "\n" );
        printf( "%2d", vettore[ j ] );
    }
}