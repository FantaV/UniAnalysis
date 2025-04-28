#include <stdio.h>

int main()
{
  int fahr;
  char *tab1 = "Fahrenheit", *tab2 = "Celsius";

  printf("%s %11s\n", tab1, tab2);

  for (fahr = 140; fahr >= -40; fahr -= 20)
    printf("%-3d %15.1f\n", fahr, (5.0/9.0) * (fahr - 32) );

  return 0;
}