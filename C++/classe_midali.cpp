#include <iostream>
#include <string>
#include <vector>

using std::string;
using std::vector;

class Persona {

    public:
      string nome;
      string cognome;
      int età;
};

int main() {

  Persona persona1;
  Persona persona2;

  std::cout << persona1.nome << std::endl;

  return 0;
}