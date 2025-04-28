#include <stdio.h>
#include <stdbool.h>
#include <math.h>
#include <locale.h>

int varGlobal = 10;        // variabile globale

int main() {

    int x = 9;                // variabile locale, si trova in un blocco, in questo caso il main

    printf("x = %d\n", varGlobal);        // anche dal main ho accesso alla variabile globale

    return 0;
}