#include <stdio.h>

#define RIGHE 3
#define COLONNE 3

int main() {
    int matrice[RIGHE][COLONNE]; // Dichiarazione dell'array bidimensionale
    int somma = 0;

    // Inserimento dei valori nella matrice
    printf("Inserisci i valori della matrice %dx%d:\n", RIGHE, COLONNE);
    for (int i = 0; i < RIGHE; i++) {
        for (int j = 0; j < COLONNE; j++) { // annidamento
            printf("Elemento [%d][%d]: ", i, j);
            scanf("%d", &matrice[i][j]); // Lettura dei valori
        }
    }

    // Stampa della matrice
    printf("\nMatrice inserita:\n");
    for (int i = 0; i < RIGHE; i++) {
        for (int j = 0; j < COLONNE; j++) {
            printf("%d\t", matrice[i][j]);
            somma += matrice[i][j]; // Calcolo della somma degli elementi
        }
        printf("\n");
    }

    // Stampa della somma degli elementi della matrice
    printf("\nSomma di tutti gli elementi della matrice: %d\n", somma);

    return 0;
}