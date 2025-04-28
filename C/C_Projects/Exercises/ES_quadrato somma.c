/* Si scriva un programma in linguaggio C che letto un numero intero positivo dallo standard input,
visualizzi a terminale il quadrato del numero stesso facendo uso soltanto di operazioni di somma.
Si osservi che il quadrato di ogni numero intero positivo N può essere costruito sommando tra loro i primi N numeri dispari.
Esempio: N = 5; N2 = 1 + 3 + 5 + 7 + 9 = 25.
*/

#include <stdio.h>

// Funzione per calcolare il quadrato di un numero usando la somma dei numeri dispari
int calcolaQuadrato( int N)
{
    int quadrato = 0;
    int dispari = 1;

    for (int i = 0; i < N; i++)
    {
        quadrato += dispari; // Aggiunge il numero dispari corrente
        dispari += 2;        // Passa al numero dispari successivo
    }

    return quadrato;
}

int main()
{
    int N;

    // Lettura di un numero intero positivo
    printf("Inserisci un numero intero positivo: ");
    scanf("%d", &N);

    // Controllo che il numero sia positivo
    if (N <= 0)
    {
        printf("Errore: il numero deve essere positivo.\n");
        return 1;
    }

    // Chiamata alla funzione per calcolare il quadrato
    int risultato = calcolaQuadrato(N);

    // Visualizzazione del risultato
    printf("Il quadrato di %d è: %d\n", N, risultato);

    return 0;
}