#include <iostream>
#include <vector>

class pila {

  public:
    pila(int);          // costruttore
    int push(int);      // inserimento di un elemento
    int pop();          // estrazione di un elemento
    bool isEmpty();     // controllo se pila vuota
    bool isFull();      // controllo se pila piena
    void stampaPila();  // stampa della pila

    private:
      std::vector<int> elementi;  // utilizzo contenitore vettore
      int nElementi;     // elementi presenti nella pila
      int max_Elementi;  // numero massimo di elementi
};

// DEFINIZIONE COSTRUTTORE
pila::pila(int max) {
  this->max_Elementi = max;
  this->nElementi = 0;
}

/* pila::push() significa “il metodo push() appartiene alla classe pila”.
   Serve a definire una funzione membro fuori dalla dichiarazione della classe. */
int pila::push(int valore) {
  if(!isFull()) {         // l’operatore di negazione logica (NOT)
    this-> nElementi++;
    elementi.push_back(valore);
    return 0;
  }
  else
    return -1;
}

int pila::pop() {
  if(!isEmpty())
    return -1;
  else {
    int n = elementi.back();
    this-> nElementi--;
    elementi.pop_back();
    return n;
  }
}

bool pila::isEmpty() {
  if(elementi.size() == 0)
    return true;
  else
    return false;
}

bool pila::isFull() {
  if(elementi.size() == max_Elementi)
    return true;
  else
    return false;
}

void pila::stampaPila() {
  for( int i = 0; i < (this-> nElementi); i++)
    std::cout << elementi[i] << std::endl;
  return;
}

int main() {

  pila p(5);
  int m;

  if(!p.isFull())
    p.push(10);
  else
    std::cout << "Pila piena" <<std::endl;

  if(!p.isFull())
    p.push(20);
  else
    std::cout << "Pila piena" <<std::endl << std::endl;

  if(!p.isEmpty()) {
    p.stampaPila();
    m = p.pop();
    std::cout << "Elemento top estratto: " << m << std::endl;
    p.stampaPila();
  }
  return 0;
}