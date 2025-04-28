#include <iostream>
#include <vector>  // Necessario per std::vector
using namespace std;

int main() {
    // 1. Creazione di un vector
    vector<int> vec1;  // Vettore vuoto
    vector<int> vec2;  // Vettore vuoto, successivamente riempito
    vec2.push_back(1);  // Aggiunge il primo elemento
    vec2.push_back(2);  // Aggiunge il secondo elemento
    vec2.push_back(3);  // Aggiunge il terzo elemento
    vec2.push_back(4);  // Aggiunge il quarto elemento
    vec2.push_back(5);  // Aggiunge il quinto elemento
    vector<int> vec3(5, 10);  // Vettore con 5 elementi inizializzati a 10

    // 2. Visualizzare il contenuto del vector
    cout << "Vettore 1 (vuoto): ";
    for (int num : vec1) {
        cout << num << " ";
    }
    cout << endl;

    cout << "Vettore 2: ";
    for (int num : vec2) {
        cout << num << " ";
    }
    cout << endl;

    cout << "Vettore 3: ";
    for (int num : vec3) {
        cout << num << " ";
    }
    cout << endl;

    // 3. Aggiungere elementi con push_back()
    vec1.push_back(5);  // Aggiunge 5 alla fine
    vec1.push_back(10);  // Aggiunge 10 alla fine

    cout << "Vettore 1 dopo 'push_back': ";
    for (int num : vec1) {
        cout << num << " ";
    }
    cout << endl;

    // 4. Rimuovere l'ultimo elemento con pop_back()
    vec1.pop_back();  // Rimuove l'ultimo elemento (10)

    cout << "Vettore 1 dopo 'pop_back': ";
    for (int num : vec1) {
        cout << num << " ";
    }
    cout << endl;

    // 5. Aggiungere più elementi con insert()
    vec2.insert(vec2.begin() + 2, 99);  // Inserisce 99 alla posizione 2
    vec2.insert(vec2.end(), 55);  // Aggiunge 55
    vec2.insert(vec2.end(), 77);  // Aggiunge 77 alla fine

    cout << "Vettore 2 dopo 'insert': ";
    for (int num : vec2) {
        cout << num << " ";
    }
    cout << endl;

    // 6. Rimuovere elementi con erase()
    vec2.erase(vec2.begin() + 1);  // Rimuove l'elemento alla posizione 1

    cout << "Vettore 2 dopo 'erase': ";
    for (int num : vec2) {
        cout << num << " ";
    }
    cout << endl;

    // 7. Modificare un elemento direttamente tramite l'indice
    vec2[2] = 100;  // Modifica l'elemento alla posizione 2

    cout << "Vettore 2 dopo modifica: ";
    for (int num : vec2) {
        cout << num << " ";
    }
    cout << endl;

    // 8. Accedere all'elemento con at()
    cout << "Elemento alla posizione 2: " << vec2.at(2) << endl;

    // 9. Verifica la dimensione e la capacità
    cout << "Dimensione di vec2: " << vec2.size() << endl;
    cout << "Capacità di vec2: " << vec2.capacity() << endl;

    // 10. Controllo se il vettore è vuoto
    if (vec1.empty()) {
        cout << "vec1 è vuoto." << endl;
    } else {
        cout << "vec1 non è vuoto." << endl;
    }

    // 11. Ridimensionare un vettore
    vec3.resize(7, 20);  // Ridimensiona a 7 elementi, nuovi valori sono 20
    cout << "Vettore 3 dopo 'resize': ";
    for (int num : vec3) {
        cout << num << " ";
    }
    cout << endl;

    // 12. Esempio di uso di 'clear' per rimuovere tutti gli elementi
    vec2.clear();
    cout << "Vettore 2 dopo 'clear' (vettore vuoto): ";
    for (int num : vec2) {
        cout << num << " ";
    }
    cout << endl;

    return 0;
}