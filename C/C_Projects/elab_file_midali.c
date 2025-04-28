#include <stdio.h>
#include <stdlib.h>

// Definizione di una struttura per i dati binari
typedef struct {
    int id;
    char nome[20];
    float valore;
} Dato;

int main() {
    FILE *file;
    char riga[100];

    // **1. Apertura e scrittura su file di testo**
    file = fopen("testo.txt", "w");
    if (file == NULL) {
        printf("Errore nell'apertura del file di testo!\n");
        return 1;
    }
    fprintf(file, "Ciao, questo è un esempio di scrittura su file.\n");
    fclose(file);
    printf("File di testo creato e scritto con successo.\n");

    // **2. Lettura da file di testo**
    file = fopen("testo.txt", "r");
    if (file == NULL) {
        printf("Errore nell'apertura del file di testo per la lettura!\n");
        return 1;
    }
    fgets(riga, sizeof(riga), file);
    printf("Contenuto del file di testo: %s", riga);
    fclose(file);

    // **3. Aggiungere dati al file di testo**
    file = fopen("testo.txt", "a");
    if (file == NULL) {
        printf("Errore nell'apertura del file per aggiungere dati!\n");
        return 1;
    }
    fprintf(file, "Questa è una nuova riga aggiunta al file.\n");
    fclose(file);
    printf("Nuova riga aggiunta al file di testo.\n");

    // **4. Scrittura su file binario**
    file = fopen("dati.bin", "wb");
    if (file == NULL) {
        printf("Errore nell'apertura del file binario!\n");
        return 1;
    }
    Dato d = {1, "Esempio", 9.99};
    fwrite(&d, sizeof(Dato), 1, file);
    fclose(file);
    printf("Dati scritti su file binario con successo.\n");

    // **5. Lettura da file binario**
    file = fopen("dati.bin", "rb");
    if (file == NULL) {
        printf("Errore nell'apertura del file binario per la lettura!\n");
        return 1;
    }
    fread(&d, sizeof(Dato), 1, file);
    printf("Dati letti dal file binario: ID=%d, Nome=%s, Valore=%.2f\n", d.id, d.nome, d.valore);
    fclose(file);

    // **6. Spostarsi nel file (fseek e ftell)**
    file = fopen("testo.txt", "r");
    if (file == NULL) {
        printf("Errore nell'apertura del file per spostarsi con fseek!\n");
        return 1;
    }
    fseek(file, 10, SEEK_SET);
    printf("Posizione attuale nel file: %ld\n", ftell(file));
    fclose(file);

    // **7. Tornare all'inizio del file (rewind)**
    file = fopen("testo.txt", "r");
    if (file == NULL) {
        printf("Errore nell'apertura del file per utilizzare rewind!\n");
        return 1;
    }
    fseek(file, 10, SEEK_SET);
    rewind(file);
    printf("Puntatore riportato all'inizio del file.\n");
    fclose(file);

    // **8. Controllare la fine del file (feof)**
    file = fopen("testo.txt", "r");
    if (file == NULL) {
        printf("Errore nell'apertura del file per controllare EOF!\n");
        return 1;
    }
    while (!feof(file)) {
        char c = fgetc(file);
        if (c != EOF)
            putchar(c);
    }
    fclose(file);
    printf("\nLettura completa del file di testo.\n");

    // **9. Rinomina un file**
    if (rename("testo.txt", "nuovo_testo.txt") == 0)
        printf("File rinominato con successo.\n");
    else
        printf("Errore nella rinominazione del file.\n");

    // **10. Eliminare un file**
    if (remove("nuovo_testo.txt") == 0)
        printf("File eliminato con successo.\n");
    else
        printf("Errore nell'eliminazione del file.\n");

    return 0;
}