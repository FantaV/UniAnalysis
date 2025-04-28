/* Il concetto di costruttore prevede che il nostro oggetto in fase di creazione abbia GIÀ DEGLI ATTRIBUTI.
 * Con l'esempio di persona si può dire che quando vado a definire Persona persona1, questa abbia già degli attributi
 * Quindi possiamo sapere già tutti i dati oppure solo parzialmente
 */

#include <iostream>
#include <string>
#include <vector>

using std::string;
using std::vector;

class Persona {

public:

    string nome = "Antonio";  // nome default
    string cognome;
    int età;

    // OVERLOADING di costruttori: possono essere definiti più costruttori. In questo caso il primo non ha parametri

    // COSTRUTTORE
    // età è già inizializzato sopra senza valori. Ma in questo primo caso noi diciamo, NO, lo facciamo partire da 1000
    Persona() {
      età = 1000;
    }

    Persona(string nome) {
        this->nome = nome;
    }

    Persona(string nome, string cognome) {
        this->nome = nome;
        this->cognome = cognome;
    }

    // in questo caso invece andiamo a passare i parametri in fase di costruzione, utilizzando il THIS del puntatore
    Persona(string nome, string cognome, int età) {
        this->nome = nome;      // il nome Antonio è assegnato di default e la letture della riga è:
                                // this nome è uguale al nome che mi viene passato. Quindi cambia valore!
        this->cognome = cognome;
        this->età = età;
    }


    void saluta() {
        std::cout << "Ciao sono " << nome << " ed ho " << età << std::endl;
    }
};

int main() {

    Persona persona1; // non serve aprire le parentesi perché richiama il primo costruttore con la classe di default
    Persona persona2("Gianni");
    Persona persona3("Francesco", "De la Cuenta");

    // costruttore 4: dobbiamo aprire le parentesi come fosse una funzione e assegnare valori
    Persona persona4("Paolo", "Di Giannantonio", 34);

    persona1.saluta();
    persona2.saluta();
    persona3.saluta();
    persona4.saluta();
}