#include <stdio.h>

// Funzione per scambiare due elementi usando i puntatori
void swap(int *a, int *b) {
    int temp = *a;
    *a = *b;
    *b = temp;
}

// Implementazione del Bubble Sort usando i puntatori
void bubbleSort(int *arr, int n) {
    int i, j;
    for (i = 0; i < n - 1; i++) {
        for (j = 0; j < n - i - 1; j++) {
            if (*(arr + j) > *(arr + j + 1)) { // Uso dei puntatori per confrontare elementi
                swap((arr + j), (arr + j + 1)); // Uso dei puntatori per scambiare elementi
            }
        }
    }
}

// Funzione per stampare un array usando i puntatori
void printArray(int *arr, int n) {
    for (int i = 0; i < n; i++) {
        printf("%d ", *(arr + i));
    }
    printf("\n");
}

int main() {
    int arr[] = {64, 34, 25, 12, 22, 11, 90};
    int n = sizeof(arr) / sizeof(arr[0]); // Calcolo della dimensione dell'array

    printf("Array originale:\n");
    printArray(arr, n);

    bubbleSort(arr, n);

    printf("Array ordinato:\n");
    printArray(arr, n);

    return 0;
}