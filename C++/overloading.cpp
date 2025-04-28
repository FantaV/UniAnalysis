/* L’overloading (sovraccarico) in C++ è un concetto che permette di definire più funzioni o operatori con lo
 * stesso nome, ma con differenti parametri (tipi o numero di argomenti). Il compilatore è in grado di determinare
 * quale versione della funzione o dell’operatore invocare in base al tipo e al numero di argomenti passati.
*/

#include <iostream>
using namespace std;

class Aritmetica {
public:
    // Funzione per sommare due numeri interi
    int somma(int a, int b) {
        return a + b;
    }

    // Funzione per sommare due numeri in virgola mobile (float)
    float somma(float a, float b) {
        return a + b;
    }

    // Funzione per sommare tre numeri interi
    int somma(int a, int b, int c) {
        return a + b + c;
    }
};

int main() {
    Aritmetica aritmetica;

    // Chiamata alla funzione somma con due interi
    cout << "Somma di due interi: " << aritmetica.somma(5, 10) << endl;

    // Chiamata alla funzione somma con due float
    cout << "Somma di due float: " << aritmetica.somma(5.5f, 10.5f) << endl;

    // Chiamata alla funzione somma con tre interi
    cout << "Somma di tre interi: " << aritmetica.somma(2, 3, 4) << endl;

    return 0;
}