// Convertire e creare una tabella con instestazioni da Fahrenheit a Celsius

#include <stdio.h>

int main()
{
  float fahr, celsius;
  int lower, upper, step;
  char *tab1 = "Fahrenheit", *tab2 = "Celsius";

  lower = 0;      // valore minimo in gradi F
  upper = 300;    // valore massimo in gradi F
  step = 20;      // intervallo tra due tempeterature in gradi F

  printf("%-15s %-15s\n", tab1, tab2 ); // stampa intestazioni
  fahr = lower;

  while (fahr <= upper)
  {
    celsius = (5.0/9.0) * (fahr - 32.0);
    printf("%-15.0f %-15.1f\n", fahr, celsius);
    // il - serve ad allineare a sinistra (di default sarebbe allineato a destra).
    fahr = fahr + step;
  }
  return 0;
}