#include <stdio.h>

int cube(int);                        // prototipo visibile in tutto il file

int main(void)                        // visibile in tutto il file
{
    for(int x=1;x<=10;x++)            // variabile x, visibile solo nel blocco della funzione
        printf("%u\n",cube(x));

    return 0;
}
int cube(int y)                       // variabile y visibile solo nel blocco della funzione CUBE
{
    return y*y*y;
}
