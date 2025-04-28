#include <stdio.h>

int main()
{

  FILE *fp;  // dichiarazione puntatore a file
  char ch;
  fp = fopen("file.txt", "a+");  // apertura in append, se il file non esiste lo crea

  fprintf(fp, "Hello World");
  fclose(fp);

  fopen("file.txt", "r"); // riparo il file in mod lettura

  // ciclo che legge carattere per carattere dal file aperto (puntato da fp) fino a raggiungere la fine del file.
  while ( (ch = fgetc(fp) ) != EOF)
  // Il risultato della chiamata a fgetc(fp) (cioè il carattere letto) viene assegnato alla variabile ch
  {
    printf("%c", ch);
  }

  fclose(fp);

  return 0;
}

/* Come fa il sistema a capire dove si trova l'EOF?
 * Quando un file viene aperto, il sistema operativo sa esattamente quanto è grande il file (in termini di byte)
   leggendo le informazioni memorizzate nel file system. Ogni file ha una dimensione specifica,
   e il sistema sa quando il puntatore di lettura ha raggiunto l’ultimo byte.
*/
