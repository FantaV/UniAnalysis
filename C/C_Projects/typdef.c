/* Il typedef in C viene utilizzato per assegnare un nome alternativo a un tipo di dato esistente, semplificando
la scrittura e migliorando la leggibilità del codice.
 */

#include <stdio.h>
#include <stdlib.h>

// 1️⃣ Definizione di un alias per un tipo di dato base
typedef unsigned long int GrandeNumero;

// 2️⃣ Definizione di un alias per una struct
typedef struct {
    int x;
    int y;
} Punto;

// 3️⃣ Alias per un puntatore a intero
typedef int* PtrInt;

// 4️⃣ Alias per un array 3x3
typedef int Matrice3x3[3][3];

// 5️⃣ Struct più complessa con `typedef`
typedef struct {
    char nome[50];
    int eta;
    float altezza;
} Persona;

// Funzione che utilizza `typedef` con struct
void stampaPersona(Persona p) {
    printf("Nome: %s, Età: %d, Altezza: %.2f\n", p.nome, p.eta, p.altezza);
}

int main() {
    // Esempio 1: Utilizzo di typedef con un tipo base
    GrandeNumero numeroGrande = 123456789;
    printf("Numero grande: %lu\n", numeroGrande);

    // Esempio 2: Uso di struct alias
    Punto p1 = {10, 20};
    printf("Punto: (%d, %d)\n", p1.x, p1.y);

    // Esempio 3: Uso di typedef con puntatori
    int a = 5;
    PtrInt ptr = &a;
    printf("Valore di a tramite ptr: %d\n", *ptr);

    // Esempio 4: Uso di typedef con array
    Matrice3x3 mat = {
        {1, 2, 3},
        {4, 5, 6},
        {7, 8, 9}
    };
    printf("Elemento mat[1][1]: %d\n", mat[1][1]);

    // Esempio 5: Uso di typedef con struct e funzione
    Persona persona1 = {"Mario Rossi", 30, 1.75};
    stampaPersona(persona1);

    return 0;
}