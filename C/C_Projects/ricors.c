// La ricorsione consiste nell’esecuzione di una funzione che chiama sé stessa fino a quando una condizione base è soddisfatta.//

#include <stdio.h>

// Funzione ricorsiva per calcolare il fattoriale
int fattoriale(int n) {
    if (n == 0) { // Condizione base
        return 1;
    }
    return n * fattoriale(n - 1); // Chiamata ricorsiva
}

int main() {
    int numero = 5;
    printf("Il fattoriale di %d è %d\n", numero, fattoriale(numero));
    return 0;
}