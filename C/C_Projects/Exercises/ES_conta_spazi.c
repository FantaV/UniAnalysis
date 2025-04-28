#include <stdio.h>

int main() {
    char c;
    int spazi = 0, tabulazioni = 0, newlines = 0;       // inzializzazione contatori

    // Legge il flusso di input fino alla fine (EOF)
    while ((c = getchar()) != EOF)
    {
        if (c == ' ')
        {
            spazi++;         // Conta gli spazi bianchi
        }
        else if (c == '\t')
        {
            tabulazioni++;   // Conta i caratteri di tabulazione
        }
        else if (c == '\n')
        {
            newlines++;      // Conta i caratteri di newline
        }
    }

    // Stampa i risultati
    printf("Spazi bianchi: %d\n", spazi);
    printf("Caratteri di tabulazione: %d\n", tabulazioni);
    printf("Caratteri di newline: %d\n", newlines);

    return 0;
}