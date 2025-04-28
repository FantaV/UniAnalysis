#ifndef AUTOMOBILE_H
#define AUTOMOBILE_H

#include <iostream>
#include <string>
using namespace std;

class Automobile {

  public:
    // Costruttore default:
    Automobile(string,string,string,string,string,double);
    Automobile();            // COSTRUTTORE
    ~Automobile();          // DISTRUTTORE
    string getTarga(void);
    string getModello(void);
    double getPrezzo(void);
    string getMarca(void);          // METODI
    string getColore(void);
    string getAnno(void);

  private:
    string targa;
    string modello;
    double prezzo;                // ATTRIBUTI: che definiscono lo stato dell'oggetto
    string marca;
    string colore;
    string anno;
};



#endif //AUTOMOBILE_H
