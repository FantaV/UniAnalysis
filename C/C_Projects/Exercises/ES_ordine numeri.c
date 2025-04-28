/* Scrivere un programma in linguaggio C che, letti tre numeri interi a, b, c dallo standard
input, stampi a terminale la sequenza dei tre numeri in ordine monotono non decrescente. */

#include <stdio.h>

int main()
{
    int a, b, c;

    printf("Inserisci il primo numero: ");
    scanf("%d", &a);
    printf("Inserisci il secondo numero: ");
    scanf("%d", &b);
    printf("Inserisci il terzo numero: ");
    scanf("%d", &c);

    if (a == b && b == c)
     printf("\nI numeri sono uguali: %d %d %d\n", a, b, c);

    if (a < b)
    {
      if (b < c)
       printf("L'ordine è %d %d %d. Primo IF\n", a,b,c);
      else
       printf("L'ordine è %d %d %d. Primo ELSE\n", a,c,b);
    }
    if (a > b)
    {
      if (b > c)
       printf("L'ordine è %d %d %d. Secondo IF\n", c,b,a);
      else
       printf("L'ordine è %d %d %d. Secondo ELSE\n", b,c,a);
    }

    return 0;
}