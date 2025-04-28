
/*
 * Nome del file: rubrica.c
 * Autore - Fabrizio Ciacchi
 * Dicembre 2003
 * Rubrica dimostrativa per Html.it
 * Il programma genere un file rubrica.txt in formato CSV, Comma 
 * Separated Value, e quindi permette ad Outlook o Mozilla di 
 * importare i contatti.
 */

// Inclusione delle librerie
#include <stdio.h>
#include <math.h>
#include <string.h>

// SISTEMA = 1 -> Sistema Linux;
// SISTEMA = 0 -> Sistema Windows;
#ifndef SISTEMA
#define SISTEMA 0
#endif

// Scelgo la funzione di sistema per pulire lo schermo
#if SISTEMA==0
  char* pulisci = "cls";
#elif SISTEMA==1
  char* pulisci = "clear";
#endif

// Creo la struttura per contenere i contatti
typedef struct
{
  char nome[50];
  char cognome[50];
  char telefono[30];
  char email[100];
  char sitoweb[200];
} t_contatto;

// Creo la struttura per creare la lista
struct elemento
{
  t_contatto inf;
  struct elemento *pun;
};

// Prototipi delle funzioni
void visualizzaContatto(struct elemento* p);
struct elemento *aggiungiContatto(struct elemento *p);
struct elemento *modificaContatto(struct elemento *p);
struct elemento *rimuoviContatto(struct elemento *p);
struct elemento *leggiDaFile(struct elemento *p);
int salvaSuFile(struct elemento *p);
void substring(char *dest, char *source, int i_base, int i_dim);
void pausa();


// Funzione principale - MAIN

int main()
{ // MAIN() - OPEN

// dichiaro la variabile scelta e la lista vuota
int scelta;
struct elemento *lista = NULL;

  // Ciclo infinito
  for ( ; ; )
  { // FOR - OPEN

    // Stampo il menu
    system(pulisci);
    printf (" #######################\n");
    printf (" # RUBRICA EASY v1.0   #\n");
    printf (" #######################\n\n");
    printf (" 1) VISUALIZZA CONTATTO\n\n");
    printf (" 2) AGGIUNGI CONTATTO\n\n");
    printf (" 3) MODIFICA CONTATTO\n\n");
    printf (" 4) RIMUOVI CONTATTO\n\n");
    printf (" 5) LEGGI CONTATTI DA FILE\n\n");
    printf (" 6) SALVA CONTATTI SU FILE\n\n");
    printf (" 0) ESCI\n\n\n\n");
    printf (" la tua scelta > ");

    // Aspetto scelta dell'utente
    scanf ("%i", &scelta);
    scelta = (int)scelta;

    if (scelta == 0) // ESCO DAL PROGRAMMA
    { // IF - OPEN

      // Stampo info di uscita
      system(pulisci); // PULISCE LO SCHERMO
      printf ("--------------------------------------\n");
      printf (" www.HTML.it - Rubrica Easy v1.0\n");
      printf (" Programma creato da Fabrizio Ciacchi\n");
      printf (" http://fabrizio.ciacchi.it\n");
      printf (" fabrizio@ciacchi.it\n");
      printf ("--------------------------------------\n\n");
      break;

    } // ELSE 
    else if (scelta == 1) // Visualizzo i contatti presenti
    { // ELSEIF 1 - OPEN

      visualizzaContatto(lista);

    } // ELSEIF 1 - CLOSE
    else if (scelta == 2) // Aggiungo un nuovo contatto
    { // ELSEIF 2 - OPEN

      lista = aggiungiContatto(lista);

    } // ELSEIF 2 - CLOSE
    else if (scelta == 3) // Modifico un contatto
    { // ELSEIF 3 - OPEN

      lista = modificaContatto(lista);

    } // ELSEIF 3 - CLOSE
    else if (scelta == 4) // Rimuovo un contatto
    { // ELSEIF 4 - OPEN

      lista = rimuoviContatto(lista);

    } // ELSEIF 4 - CLOSE
    else if (scelta == 5) // Leggo i contatti da File
    { // ELSEIF 5 - OPEN

      lista = leggiDaFile(lista);

    }  // ELSEIF 5 - CLOSE
    else if (scelta == 6) // Salvo i contatti su File
    { // ELSEIF 6 - OPEN

      salvaSuFile(lista);

    } // IF - CLOSE

  } // FOR - CLOSE

} // MAIN() - OPEN

// Fine del MAIN, adesso ci sono le funzioni che eseguono il 
// compito a loro assegnato

/*
 * Visualizza i contatti presenti
 */
void visualizzaContatto(struct elemento* p)
{ // visualizzaContatto() - OPEN

  if (p == NULL)
  { // IF - OPEN

    /*
     * Se non ci sono contatti lo stampo a video
     */
    system(pulisci); // PULISCE LO SCHERMO
    printf (" #######################\n");
    printf (" # RUBRICA EASY v1.0   #\n");
    printf (" #######################\n");
    printf (" # VISUALIZZA CONTATTO #\n");
    printf (" #######################\n\n");
    printf (" Nessun contatto presente\n");
    pausa();

  } else { // ELSE

    /*
     * Stampo il primo contatto presente
     */
    system(pulisci); // PULISCE LO SCHERMO
    printf (" #######################\n");
    printf (" # RUBRICA EASY v1.0   #\n");
    printf (" #######################\n");
    printf (" # VISUALIZZA CONTATTO #\n");
    printf (" #######################\n\n");
    printf (" NOME > %s\n", p->inf.nome);
    printf (" COGNOME > %s\n", p->inf.cognome);
    printf (" TELEFONO > %s\n", p->inf.telefono);
    printf (" EMAIL > %s\n", p->inf.email);
    printf (" SITOWEB > %s\n", p->inf.sitoweb);
    pausa();

  } // IF - CLOSE

  while (p != NULL)
  { // WHILE - OPEN

    /*
     * Stampo gli altri contatti
     */
    system(pulisci); // PULISCE LO SCHERMO
    printf (" #######################\n");
    printf (" # RUBRICA EASY v1.0   #\n");
    printf (" #######################\n");
    printf (" # VISUALIZZA CONTATTO #\n");
    printf (" #######################\n\n");
    printf (" NOME > %s\n", p->inf.nome);
    printf (" COGNOME > %s\n", p->inf.cognome);
    printf (" TELEFONO > %s\n", p->inf.telefono);
    printf (" EMAIL > %s\n", p->inf.email);
    printf (" SITOWEB > %s\n", p->inf.sitoweb);
    pausa();
    // Leggo l'elemento successivo
    p = p->pun;

  } // WHILE - CLOSE

  return;

} // visualizzaContatto() - CLOSE

/*
 * Aggiungo un nuovo contatto
 */
struct elemento *aggiungiContatto(struct elemento *p)
{ // aggiungiContatto() - OPEN

  system(pulisci); // PULISCE LO SCHERMO
  printf (" #######################\n");
  printf (" # RUBRICA EASY v1.0   #\n");
  printf (" #######################\n");
  printf (" # AGGIUNGI CONTATTO   #\n");
  printf (" #######################\n\n");

  // Dichiaro le variabili
  char nome[50];
  char cognome[50];
  char telefono[30];
  char email[100];
  char sitoweb[200];
  t_contatto daInserire;
  struct elemento *punt;

  // Popolo la variabile daInserire
  printf (" NOME > ");
  scanf ("%s", nome);
  strcpy(daInserire.nome, nome);
  printf (" COGNOME > ");
  scanf ("%s", cognome);
  strcpy(daInserire.cognome, cognome);
  printf (" TELEFONO > ");
  scanf ("%s", telefono);
  strcpy(daInserire.telefono, telefono);
  printf (" EMAIL > ");
  scanf ("%s", email);
  strcpy(daInserire.email, email);
  printf (" SITOWEB > ");
  scanf ("%s", sitoweb);
  strcpy(daInserire.sitoweb, sitoweb);

  if(p != NULL)
  { // IF - OPEN

    /* creazione elementi successivi */
    // Alloco la memoria necessaria
    punt = (struct elemento *)malloc(sizeof(struct elemento));
    // Metto daInserire nell'informazione del puntatore
    punt->inf = daInserire;
    // Metto il puntatore in testa alla lista
    punt->pun = p;

  } else { // ELSE

    /* creazione primo elemento */
    // Alloco la memoria necessaria
    p = (struct elemento *)malloc(sizeof(struct elemento));
    // Metto daInserire nell'informazione del puntatore
    p->inf = daInserire;
    // p punta a NULL, ovvero il marcatore di fine lista
    p->pun = NULL;
    // Assegno p a punt
    punt = p;

  } // IF - CLOSE

  // Esce dalla funzione e restituisce la lista
  return(punt);

} // aggiungiContatto() - CLOSE

/*
 * Modifico un contatto presente
 */
struct elemento *modificaContatto(struct elemento *p)
{ // modificaContatto() - OPEN

  // Dichiaro le variabili
  struct elemento *twin = p;
  int subscelta;
  int i=1;
  int n=1;

  // Stampo la schermata
  system(pulisci); // PULISCE LO SCHERMO
  printf (" #######################\n");
  printf (" # RUBRICA EASY v1.0   #\n");
  printf (" #######################\n");
  printf (" # MODIFICA CONTATTO   #\n");
  printf (" #######################\n\n");

  // Stampo la lista di contatti
  while (p != NULL)
  { // WHILE - OPEN

    printf ("%i) \t %s \t %s\n", i, p->inf.nome, p->inf.cognome);
    // Leggo l'elemento successivo
    p = p->pun;
    i++;

  } // WHILE - CLOSE

  // Ottengo il valore originario di p
  p = twin;

  // Scelgo l'emento da modificare
  printf("\n\n Inserisci il numero del contatto che vuoi modificare: ");
  scanf("%i", &subscelta);

  for (n=1; n<i; n++)
  { // FOR - OPEN

    /*
     * Trovato l'elemento modifico l'informazione dentro di esso
     */
    if (subscelta == n)
    { // IF - OPEN

      // Popolo la variabile daInserire
      printf (" NOME [%s] > ", p->inf.nome);
      scanf ("%s", p->inf.nome);
      printf (" COGNOME [%s] > ", p->inf.cognome);
      scanf ("%s", p->inf.cognome);
      printf (" TELEFONO [%s] > ", p->inf.telefono);
      scanf ("%s", p->inf.telefono);
      printf (" EMAIL [%s] > ", p->inf.email);
      scanf ("%s", p->inf.email);
      printf (" SITOWEB [%s] > ", p->inf.sitoweb);
      scanf ("%s", p->inf.sitoweb);

    } // IF - CLOSE

    p = p->pun;

  } // FOR - CLOSE

  // Esce dalla funzione e restituisce la lista
  return twin;

} // modificaContatto() - CLOSE

/*
 * Rimuovo un contatto
 */
struct elemento *rimuoviContatto(struct elemento *p)
{ // rimuoviContatto() - OPEN

  // Dichiaro le variabili
  struct elemento *aus;
  struct elemento *twin = p;
  int subscelta;
  int i=1;
  int n=1;

  // Stampo la schermata
  system(pulisci); // PULISCE LO SCHERMO
  printf (" #######################\n");
  printf (" # RUBRICA EASY v1.0   #\n");
  printf (" #######################\n");
  printf (" # RIMUOVI CONTATTO    #\n");
  printf (" #######################\n\n");

  // Stampo la lista di contatti
  while (p != NULL)
  { // WHILE - OPEN

    printf ("%i) \t %s \t %s\n", i, p->inf.nome, p->inf.cognome);
    p = p->pun; // scorre di un elemento
    i++;

  } // WHILE - CLOSE

  // Ottengo il valore originario di p
  p = twin;

  // Scelgo l'emento da eliminare
  printf("\n\n Inserisci il numero del contatto che vuoi rimuovere: ");
  scanf("%i", &subscelta);

  if (subscelta < i)
  { // IF - OPEN

    // Se la lista è vuota esco
    if(p == NULL)
      return;

    // Se la lista ha almeno due elmenti...
    if(p->pun != NULL)
    { // IF - OPEN

      // ... inizializzo un puntatore ausiliario ...
      aus=p;
      n++;

      // ... e faccio un ciclo per trovare l'elemento da eliminare ...
      while(n != i)
      { // WHILE - OPEN

        // Trovato l'elemento gli faccio puntare l'oggetto puntato dal suo
        // puntatore, in poche parole "salto" l'elemento da eliminare
        if(subscelta == n)
        { // IF - OPEN

          aus->pun=aus->pun->pun;

        } else { // ELSE

          // Nel caso in cui il puntatore fosse NULL, per non creare casini
          // glielo assegniamo direttamente
          aus=aus->pun;

        } // IF - CLOSE

        n++;

      } // WHILE - CLOSE

    } // IF - CLOSE

    /*
     * Se si vuole rimuovere il primo elemento, si assegna
     * a p il valore del suo oggetto puntato; questo accade
     * quando subscelta == 1
     */
    if(subscelta == 1)
    { // IF - OPEN

      p=p->pun;

    } // IF - CLOSE

  } // IF - CLOSE

  // Copia di nuovo il valore p modificato in twin
  twin = p;

  // Esce dalla funzione e restituisce la lista
  return twin;

} // rimuoviContatto() - CLOSE

/*
 * Leggo i contatti dal file rubrica.txt
 */
struct elemento *leggiDaFile(struct elemento *p)
{ // leggiDaFile - OPEN

  // Dichiaro le variabili
  t_contatto daInserire;
  t_contatto vuoto; // elemento vuoto
  struct elemento *punt = p;
  char linea[500]; // stringa per l'acquisizione
  int index=0; // inizializza l'indice

  // Apre il file
  FILE *stream = fopen("rubrica.txt","rt");

  // Salvo la lista di contatti
  while(fgets(linea, 500, stream) != NULL)
  { // WHILE - OPEN

    
    index = strcspn(linea, ",");
    strncpy(daInserire.nome, linea, index);
    substring(daInserire.nome, daInserire.nome, 0, index-1);
    substring(linea, linea, index+1, strlen(linea));
    index = strcspn(linea, ",");
    strncpy(daInserire.cognome, linea, index);
    substring(daInserire.cognome, daInserire.cognome, 0, index-1);
    substring(linea, linea, index+1, strlen(linea));
    index = strcspn(linea, ",");
    strncpy(daInserire.telefono, linea, index);
    substring(daInserire.telefono, daInserire.telefono, 0, index-1);
    substring(linea, linea, index+1, strlen(linea));
    index = strcspn(linea, ",");
    strncpy(daInserire.email, linea, index);
    substring(daInserire.email, daInserire.email, 0, index-1);
    substring(linea, linea, index+1, strlen(linea));
    strcpy(daInserire.sitoweb, linea);
    substring(daInserire.sitoweb, daInserire.sitoweb, 0, strlen(daInserire.sitoweb)-2);

    // Aggiunge l'elemento alla lista
    p = punt;
    punt = (struct elemento *)malloc(sizeof(struct elemento));
    punt->inf = daInserire;
    punt->pun = p;

    // azzera il valore di daInserire
    daInserire = vuoto;

  } // WHILE - CLOSE

  // Chiude il file
  fflush(stream);
  fclose(stream);

  // Esce dalla funzione e restituisce la lista
  return(punt);

} // leggiDaFile - CLOSE

/*
 * Salvo i contatti inseriti in un file di nome rubrica.txt
 */
int salvaSuFile(struct elemento *p)
{ // salvaSuFile() - OPEN

  // Apre il file
  FILE *stream = fopen("rubrica.txt","w");

  // Salvo la lista di contatti
  while (p != NULL)
  { // WHILE - OPEN

    // Scrive sul file
    fprintf(stream, "%s,%s,%s,%s,%s\n", p->inf.nome, p->inf.cognome, p->inf.telefono, p->inf.email, p->inf.sitoweb);
    // legge l'emento successivo
    p = p->pun;

  } // WHILE - CLOSE

  // Chiude il file
  fflush(stream);
  fclose(stream);

  // Esce dalla funzione
  return;

} // salvaSuFile() - CLOSE

/*
 * Funzione che prende una sottostringa partendo da un'indice
 * base per un numero di caratteri pari a dimensione
 */
void substring(char* dest, char *source, int i_base, int i_dim)
{ // substring() - OPEN

  int i = 0;

  for (i=i_base; i<i_dim+1; i++)
  { // FOR - OPEN

    dest[i-i_base] = source[i];

  } // FOR - CLOSE

  dest[i]='\0';

  return;

} // substring() - CLOSE

/*
 * Funzione che blocca lo schermo ad aspetta che l'utente
 * prema INVIO
 */
void pausa()
{ // pausa() - OPEN

  char invio;
  printf("\n\n - premi INVIO per continuare -\n");
  invio = getchar();
  return;

} // pausa() - CLOSE
