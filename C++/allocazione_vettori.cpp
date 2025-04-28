#include <iostream>
#include <vector>

int main() {
    // Dichiarazione di un vettore di interi
    std::vector<int> numeri;

    // Aggiunta di elementi al vettore
    numeri.push_back(10);
    numeri.push_back(20);
    numeri.push_back(30);
    numeri.push_back(40);
    numeri.push_back(50);

    // Stampa degli elementi del vettore
    std::cout << "Elementi del vettore: ";
    for (int num : numeri) {
        std::cout << num << " ";
    }
    std::cout << std::endl;

    // Accesso agli elementi tramite indice
    std::cout << "Elemento in posizione 2: " << numeri[2] << std::endl;

    // Modifica di un elemento
    numeri[2] = 100;
    std::cout << "Dopo modifica, elemento in posizione 2: " << numeri[2] << std::endl;

    // Rimozione dell'ultimo elemento
    numeri.pop_back();
    std::cout << "Dopo rimozione, elementi del vettore: ";
    for (int num : numeri) {
        std::cout << num << " ";
    }
    std::cout << std::endl;

    return 0;
}
