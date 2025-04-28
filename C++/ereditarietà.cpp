#include <iostream>
#include <string>
#include <vector>

using std::string;
using std::vector;

class Persona {

public:

    string nome = "Antonio";  // nome default
    string cognome;

    // aggiunta costruttore: che deve essere gestito anche dagli eredi
    Persona(string nome, string cognome) {
        this-> nome = nome;
        this-> cognome = cognome;
    }

    void saluta() {
        std::cout << "Ciao sono " << nome << std::endl;
    }
};

// CLASSE EREDE: Studente "estende" Persona, "deriva", "appartiene" e con esso proprietà e funzioni
class Studente : public Persona {

    public:     // public va RIaggiunto se inseriamo nuove proprietà alla classe erede

    string classe = "L-41";

    // costruttore dell'erede
    Studente(string nome, string cognome, string classe) : Persona(nome, cognome) {
        this-> classe = classe;
    }

    void saluta() {
        std::cout << "Ciao a tutti! " << std::endl;   // questo saluta è diverso da quello genitore e lo sovrascrive
    }
    void presentati() {
        std::cout << "Sono " << nome << " della classe " << classe << std::endl;
    }

};

class Insegnante : public Persona {

};

int main() {
    Persona p("Marlon", "Brando");
    Studente s("Furio", "di Roberto", "L-98");

    // s.nome = "Piero";

    p.saluta();
    s.saluta();
    s.presentati();

  return 0;
}
