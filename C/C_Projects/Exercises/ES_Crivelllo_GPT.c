#include <stdio.h>

int main()
{
    int n;

    // INSERIMENTO DIMENSIONE
    printf("Inserisci un massimo: ");
    scanf("%d", &n);

    if (n < 2)
    {
        printf("Non ci sono numeri primi minori di %d.\n", n);
        return 0;
    }

    // ALLOCAZIONE DELL'ARRAY E INIZIALIZZAZIONE
    int numeri[n + 1]; // Usare un array con dimensione n+1 per includere n
    for (int i = 0; i <= n; i++)
        numeri[i] = 1; // Tutti i numeri sono inizialmente "non cancellati"

    numeri[0] = 0; // 0 non è un numero primo
    numeri[1] = 0; // 1 non è un numero primo

    // IMPLEMENTAZIONE DEL CRIVELLO DI ERASTOTENE
    for (int i = 2; i * i <= n; i++)  // Solo fino a sqrt(n)
    {
        if (numeri[i] == 1)     // Se il numero è ancora "non cancellato"
        {
            for (int j = i * i; j <= n; j += i)
            {
                numeri[j] = 0; // Cancella i multipli di i
            }
        }
    }

    // STAMPA DEI NUMERI PRIMI
    printf("Numeri primi fino a %d:\n", n);
    int cont = 0;
    for (int i = 2; i <= n; i++)
    {
        if (numeri[i] == 1)
        {
            printf("%d ", i);
            cont++;
        }
    }
    printf("\nTotale dei numeri primi = %d\n", cont);

    return 0;
}