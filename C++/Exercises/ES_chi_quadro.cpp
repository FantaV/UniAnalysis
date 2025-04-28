#include <iostream>
#include <cmath>

using namespace std;

// Funzione che calcola e aggiorna il grado di associazione in base a cramer_V
void gradoAss(double cramer_V, string &associazione) {
    // Calcolo dell'associazione in base al valore di cramer_V
    if (cramer_V > 0 && cramer_V < 0.1)
        associazione = "Molto debole";
    else if (cramer_V >= 0.1 && cramer_V < 0.3)
        associazione = "Debole";
    else if (cramer_V >= 0.3 && cramer_V < 0.5)
        associazione = "Moderata";
    else if (cramer_V >= 0.5)
        associazione = "Forte";
}

double coefficienteContingenza(double chi_square, double n) {
    return sqrt(chi_square / (chi_square + n));
}

int main() {
    // Dichiarazione delle variabili per i valori osservati
    double O11, O12, O21, O22;
    string associazione;

    // Input dei valori osservati
    cout << "Inserisci i valori osservati per la tabella 2x2:\n";
    cout << "Riga 1, Colonna 1: "; cin >> O11;
    cout << "Riga 1, Colonna 2: "; cin >> O12;
    cout << "Riga 2, Colonna 1: "; cin >> O21;
    cout << "Riga 2, Colonna 2: "; cin >> O22;

    // Calcolo dei totali
    double row1 = O11 + O12;
    double row2 = O21 + O22;
    double col1 = O11 + O21;
    double col2 = O12 + O22;
    double total = row1 + row2;

    // Calcolo dei valori attesi
    double E11 = (row1 * col1) / total;
    double E12 = (row1 * col2) / total;
    double E21 = (row2 * col1) / total;
    double E22 = (row2 * col2) / total;

    // Calcolo del valore del chi-quadro
    double chi_square = pow(O11 - E11, 2) / E11 +
                        pow(O12 - E12, 2) / E12 +
                        pow(O21 - E21, 2) / E21 +
                        pow(O22 - E22, 2) / E22;

    // Calcolo dell'Indice di Cramér (V)
    double cramer_V = sqrt(chi_square / total);  // (k-1) = 1 per una tabella 2x2

    // Calcolo del coefficiente di contingenza
    double C = coefficienteContingenza(chi_square, total);

    // Output del valore del chi-quadro
    cout << "\nIl valore del chi-quadro è: " << chi_square << endl;

    // Passaggio del valore cramer_V alla funzione gradoAss per determinare l'associazione
    gradoAss(cramer_V, associazione);

    cout << "L'indice di Cramér (V) è: " << cramer_V << endl;
    cout << "L'associazione tra le variabili è: " << associazione << endl;
    cout << "Il coefficiente di contingenza (C) è: " << C << endl;

    return 0;
}