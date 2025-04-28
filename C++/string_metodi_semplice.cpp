#include <iostream>
#include <cstring>  // Per usare strlen, strcpy, strcat, ecc.

int main() {
    // Dichiarazione e inizializzazione di stringhe
    char str1[50] = "Ciao";   // Stringa 1
    char str2[50] = " Mondo"; // Stringa 2

    // Stampa delle stringhe originali
    std::cout << "Stringa 1: " << str1 << std::endl;
    std::cout << "Stringa 2: " << str2 << std::endl;

    // Concatenazione delle stringhe
    strcat(str1, str2);  // Aggiunge str2 alla fine di str1

    // Stampa la stringa concatenata
    std::cout << "Stringa concatenata: " << str1 << std::endl;

    // Confronto tra due stringhe (str1 e str2)
    if (strcmp(str1, str2) == 0) {
        std::cout << "Le stringhe sono uguali!" << std::endl;
    } else {
        std::cout << "Le stringhe sono diverse!" << std::endl;
    }

    // Lunghezza della stringa
    std::cout << "La lunghezza della stringa 1 è: " << strlen(str1) << std::endl;

    return 0;
}