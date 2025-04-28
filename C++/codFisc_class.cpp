#include <iostream>
#include <string>
#include <vector>
#include "codFisc.h"
#include <fstream>

using namespace std;

// implementazione classe Persona
Persona::Persona() {
}

Persona::Persona(string nome, string cognome, string codFiscale, string residenza, int anni, double reddito) {
    this->nome = nome;
    this->cognome = cognome;
    this->codFiscale = codFiscale;
    this->anni = anni;
    this->reddito = reddito;
    this->residenza = residenza;
}

string Persona::getNome() {
    return this->nome;
}

string Persona::getCognome() {
    return this->cognome;
}

string Persona::getCodFiscale() {
    return this->codFiscale;
}

int Persona::getAnni() {
    return this->anni;
}

double Persona::getReddito() {
    return this->reddito;
}

string Persona::getResidenza() {
    return this->residenza;
}

int main() {
    // dichiarazione variabili
    Persona q;
    double reddito;
    string nome, cognome, codFiscale, residenza;
    int nPersone, anni, n;
    vector<Persona> vPersona;
    bool flagTrovato;

    // fase di input
    cout << "Inserisci il numero di persone da inserire:" << endl;
    cin >> nPersone;

    for (int i = 0; i < nPersone; i++) {
        cout << "===== Persona =====" << i + 1 << "====" << endl;
        cout << "Inserisci il nome" << endl;
        cin >> nome;
        cout << "Inserisci il cognome" << endl;
        cin >> cognome;
        cout << "Inserisci il codice Fiscale" << endl;
        cin >> codFiscale;
        cout << "Inserisci gli anni" << endl;
        cin >> anni;
        cout << "Inserisci il reddito" << endl;
        cin >> reddito;
        cin.ignore(); // toglie dal buffer i caratteri di fine linea in modo da poter usare getline
        cout << "Inserisci la residenza" << endl;
        getline(cin, residenza);
    };

    // salvataggio in variabile di appoggio
    q.setNome(nome);
    q.setCognome(cognome);
    q.setCodiceFiscale(codFiscale);
    q.setAnni(anni);
    q.setResidenza(residenza);
    q.setReddito(reddito);

    // inserimento del record nell'array
    vPersona.push_back(q);

    cout << "================= RICERCA =================" << endl;
    cout << "Inserisci il codice Fiscale da cercare" << endl;
    cin >> codFiscale;

    // ciclo di ricerca
    flagTrovato = false;

    for (int i = 0; (i < nPersone) && (flagTrovato == false); i++) {
        if (vPersona[i].getCodFiscale() == codFiscale) {
            cout << "Trovato il record!" << endl;
            cout << vPersona[1].getNome() << " " << vPersona[1].getCognome() << endl;
            flagTrovato = true;
        }
    }
    if (!flagTrovato)
        cout << "Il codice Fiscale é:" << codFiscale << " non esiste!" << endl;
}
