#include <stdio.h>
#include <stdlib.h>

int main() {
    // Utilizzo di malloc: alloca un array di 5 interi
    int* ptr = (int*) malloc(5 * sizeof(int));
    if (ptr == NULL) {
        printf("Errore di allocazione con malloc!\n");
        return 1; // Uscita se malloc fallisce
    }

    // Inizializzazione dei valori dell'array
    for (int i = 0; i < 5; i++) {
        ptr[i] = i + 1;  // Assegna 1, 2, 3, 4, 5
    }

    printf("Array iniziale allocato con malloc:\n");
    for (int i = 0; i < 5; i++) {
        printf("ptr[%d] = %d\n", i, ptr[i]);
    }

    // Utilizzo di realloc: cambia la dimensione dell'array a 10 interi
    ptr = (int*) realloc(ptr, 10 * sizeof(int));
    if (ptr == NULL) {
        printf("Errore di riallocazione con realloc!\n");
        return 1; // Uscita se realloc fallisce
    }

    // Inizializzazione dei nuovi valori (da 6 a 10)
    for (int i = 5; i < 10; i++) {
        ptr[i] = i + 1;  // Assegna 6, 7, 8, 9, 10
    }

    printf("\nArray dopo realloc (dimensione 10):\n");
    for (int i = 0; i < 10; i++) {
        printf("ptr[%d] = %d\n", i, ptr[i]);
    }

    // Utilizzo di calloc: alloca memoria e inizializza con zero
    int* ptr2 = (int*) calloc(5, sizeof(int));
    if (ptr2 == NULL) {
        printf("Errore di allocazione con calloc!\n");
        free(ptr);  // Liberiamo ptr prima di uscire
        return 1; // Uscita se calloc fallisce
    }

    printf("\nArray allocato con calloc (tutti i valori sono 0):\n");
    for (int i = 0; i < 5; i++) {
        printf("ptr2[%d] = %d\n", i, ptr2[i]);
    }

    // Utilizzo di free: libera la memoria allocata
    free(ptr);   // Libera ptr dopo realloc
    free(ptr2);  // Libera ptr2 dopo calloc

    printf("\nMemoria liberata.\n");

    return 0;
}