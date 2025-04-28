#include <iostream>
#include <string>  // Necessario per std::string
using namespace std;

int main() {
    // 1. Creazione di una stringa
    string str1;  // Stringa vuota
    string str2 = "C++ è fantastico!";  // Stringa inizializzata
    string str3(5, 'A');  // Crea una stringa di 5 caratteri 'A'

    cout << "Stringa 1 (vuota): " << str1 << endl;
    cout << "Stringa 2: " << str2 << endl;
    cout << "Stringa 3: " << str3 << endl;

    // 2. Concatenazione di stringhe
    string str4 = str1 + "Hello, ";
    str4 += "World!";  // Concatenazione usando l'operatore +=

    cout << "Stringa concatenata con '+': " << str4 << endl;

    // 3. Modifica di una stringa
    str2[0] = 'c';  // Modifica del primo carattere
    cout << "Stringa 2 modificata: " << str2 << endl;

    // 4. Aggiungere una sottostringa
    str2.append(" Molto interessante!");
    cout << "Stringa 2 con 'append': " << str2 << endl;

    // 5. Rimuovere una parte della stringa
    str2.erase(0, 5);  // Rimuove i primi 5 caratteri
    cout << "Stringa 2 dopo 'erase': " << str2 << endl;

    // 6. Confronto di stringhe
    string str5 = "C++";
    if (str5 == str2) {
        cout << "str5 è uguale a str2." << endl;
    } else {
        cout << "str5 è diverso da str2." << endl;
    }

    // 7. Ricerca di una sottostringa
    size_t pos = str2.find("interessante");
    if (pos != string::npos) {
        cout << "'interessante' trovato alla posizione: " << pos << endl;
    }

    // 8. Estrazione di una sottostringa
    string substr = str2.substr(3, 6);  // Estrae 6 caratteri a partire dalla posizione 3
    cout << "Sottostringa estratta: " << substr << endl;

    // 9. Lunghezza della stringa
    cout << "Lunghezza della stringa: " << str2.size() << endl;

    // 10. Controllo se la stringa è vuota
    if (str1.empty()) {
        cout << "str1 è vuota." << endl;
    } else {
        cout << "str1 non è vuota." << endl;
    }

    // 11. Sostituire una parte della stringa
    str2.replace(3, 6, "esempio");  // Sostituisce i 6 caratteri dalla posizione 3 con "esempio"
    cout << "Stringa 2 dopo 'replace': " << str2 << endl;

    // 12. Inserimento di una sottostringa
    str2.insert(7, " di C++");
    cout << "Stringa 2 dopo 'insert': " << str2 << endl;

    // 13. Verifica se una stringa contiene un'altra stringa
    if (str2.find("C++") != string::npos) {
        cout << "La stringa contiene 'C++'." << endl;
    } else {
        cout << "La stringa non contiene 'C++'." << endl;
    }

    // 14. Esempio con 'resize' per cambiare la lunghezza
    str2.resize(15);  // Riduce la lunghezza della stringa a 15 caratteri
    cout << "Stringa 2 dopo 'resize': " << str2 << endl;

    return 0;
}