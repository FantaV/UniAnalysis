#include <stdio.h>

int main()
{
    int a, b, c;

    printf("Inserisci il primo numero: ");
    scanf("%d", &a);
    printf("Inserisci il secondo numero: ");
    scanf("%d", &b);
    printf("Inserisci il terzo numero: ");
    scanf("%d", &c);

    // Caso in cui tutti i numeri sono uguali
    if (a == b && b == c) {
        printf("I numeri sono uguali: %d %d %d\n", a, b, c);
    }
    // Ordinamento con confronti diretti
    else {
        if (a > b) { int temp = a; a = b; b = temp; }
        if (b > c) { int temp = b; b = c; c = temp; }
        if (a > b) { int temp = a; a = b; b = temp; }

        printf("L'ordine è: %d %d %d\n", a, b, c);
    }

    return 0;
}