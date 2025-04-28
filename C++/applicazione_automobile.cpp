#include "automobile.h"
#include <iostream>
#include <string>

using namespace std;

int main() {
    Automobile Fiat;
    Automobile *Panda;
    Panda = new Automobile;

    cout << Panda -> getPrezzo() << endl;
    cout << Panda -> getMarca() << endl;
    delete Panda;
    return 0;
}