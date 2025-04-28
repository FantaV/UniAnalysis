#include <iostream>
#include "definizione_automobile.h"
#include <string>


string Automobile::getMarca(void)
{
  return this->marca;
}
string Automobile::getModello(void) {
  return this->modello;
}
double Automobile::getPrezzo(void) {
  return this->prezzo;
}
string Automobile::getAnno(void) {
  return this->anno;
}
string Automobile::getColore(void) {
  return this->colore;
}
string Automobile::getTarga(void) {
  return this->targa;
}
