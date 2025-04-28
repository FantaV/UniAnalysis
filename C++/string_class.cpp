#include <iostream>
#include <cstring> // Per l'uso di strlen, strcpy, ecc.

class String {
private:
    char* str;  // Puntero per la stringa
    size_t length;  // Lunghezza della stringa

public:
    // Costruttore vuoto (crea una stringa vuota)
    String() {
        str = new char[1]; // Alloca spazio per un carattere (null terminator)
        str[0] = '\0';  // Stringa vuota
        length = 0;
    }

    // Costruttore con una stringa passata come parametro
    String(const char* s) {
        length = strlen(s);  // Calcola la lunghezza della stringa
        str = new char[length + 1];  // Alloca spazio per la stringa più il carattere finale '\0'
        strcpy(str, s);  // Copia la stringa nella memoria
    }

    // Distruttore (libera la memoria quando la stringa non serve più)
    ~String() {
        delete[] str;
    }

    // Metodo per ottenere la lunghezza della stringa
    size_t size() const {
        return length;  // Restituisce la lunghezza della stringa
    }

    // Metodo per visualizzare la stringa
    void print() const {
        std::cout << str << std::endl;  // Stampa la stringa
    }

    // Metodo per concatenare due stringhe (aggiunge una stringa alla fine di un'altra)
    String append(const String& other) const {
        size_t newLength = length + other.length;  // Calcola la nuova lunghezza
        char* newStr = new char[newLength + 1];  // Alloca memoria per la nuova stringa

        strcpy(newStr, str);  // Copia la stringa originale
        strcat(newStr, other.str);  // Aggiunge l'altra stringa

        String newString(newStr);  // Crea un nuovo oggetto String
        delete[] newStr;  // Libera la memoria temporanea

        return newString;  // Restituisce la nuova stringa concatenata
    }

    // Metodo per confrontare due stringhe (verifica se sono uguali)
    bool equals(const String& other) const {
        return strcmp(str, other.str) == 0;  // Restituisce true se sono uguali, false altrimenti
    }
};

int main() {
    String s1("Ciao");
    String s2(" Mondo");

    // Stampa le stringhe originali
    std::cout << "Stringa 1: ";
    s1.print();
    std::cout << "Stringa 2: ";
    s2.print();

    // Concatenazione di due stringhe
    String s3 = s1.append(s2);
    std::cout << "Stringa concatenata: ";
    s3.print();  // Dovrebbe stampare "Ciao Mondo"

    // Confronto tra stringhe
    if (s1.equals(s2)) {
        std::cout << "Le stringhe sono uguali!" << std::endl;
    } else {
        std::cout << "Le stringhe sono diverse!" << std::endl;  // Sarà questa la risposta
    }

    return 0;
}