#include <iostream>

class animali {

  public:
    animali(); // costruttore
    virtual void print(void);   //  VIRTUAL: consente alle classi derivate di sovrascrivere quel metodo
                                // e sono OBBLIGATE A FARLO per poter utilizzare il metodo

  // PROTECTED: non sono accessibili dall’esterno della classe, quindi solo dalle classi eredi
  protected:
    int ngambe; // membro variabile
};

class pesci :public animali {
  public:
    pesci();
    pesci(int);
    void print(void);
};

class uccelli : public animali {
  public:
    uccelli();
    uccelli(int);
    void print(void);
};

class mammiferi : public animali {
  public:
    mammiferi();
    mammiferi(int);
    void print(void);
};

// DEFINIZIONE COSTRUTTORI
animali::animali() { ngambe = -1; }  // Default: Animale sconosciuto
pesci::pesci() { ngambe = 0; }
pesci::pesci(int gambe) { ngambe = gambe; }

uccelli::uccelli() { ngambe = 2; }
uccelli::uccelli(int gambe) { ngambe = gambe; }

mammiferi::mammiferi() { ngambe = 4; }
mammiferi::mammiferi(int gambe) { ngambe = gambe; }


// DEFINIZIONE METODI PRINT
void animali::print(void) {
  std::cout << "Animale sconosciuto"  << std::endl;
  return;
}

void pesci::print(void) {
  std::cout << "I pesci hanno " << ngambe << " zampe" << std::endl;
  return;
}

void uccelli::print(void) {
  std::cout << "Gli uccelli hanno " << ngambe << " zampe" << std::endl;
  return;
}

void mammiferi::print(void) {
  std::cout << "I mammiferi hanno " << ngambe << " zampe " << std::endl;
  return;
}

int main() {

  animali *p[4];
  p[0] = new uccelli(0);
  p[1] = new uccelli(2);
  p[2] = new mammiferi(4);
  p[3] = new animali;

  for(int i = 0; i < 4; i++)
    p[i]->print();

  return 0;
}
