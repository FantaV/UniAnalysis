#include <stdio.h>
#include <ctype.h>

int main() {
    char c;
    int spazio_precedente = 0; // Flag per tenere traccia se l'ultimo carattere è stato uno spazio

    // Legge il flusso di input fino alla fine (EOF)
    while ((c = getchar()) != EOF)
    {
        if (c == ' ')
        {
            // Se l'ultimo carattere era già uno spazio, salta questo
            if (spazio_precedente == 0)
            {
                putchar(c); // Stampa uno spazio solo se non è stato stampato uno spazio prima
                spazio_precedente = 1; // Imposta il flag che l'ultimo carattere è uno spazio
            }
        }
        else
        {
            putchar(c); // Stampa il carattere non-spazio
            spazio_precedente = 0; // Resetta il flag per i caratteri non-spazio
        }
    }
    return 0;
}