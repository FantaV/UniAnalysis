#include <iostream>
#include <ctime>
#include <chrono>
#include <iomanip>

using namespace std;

// Funzione per calcolare la differenza in giorni tra due date
time_t convertToTimeT(int year, int month, int day) {
    tm timeStruct = {};
    timeStruct.tm_year = year - 1900;
    timeStruct.tm_mon = month - 1;
    timeStruct.tm_mday = day;
    return mktime(&timeStruct);
}

int main() {
    // Ottieni la data di oggi
    time_t now = time(0);
    tm *ltm = localtime(&now);
    int currentYear = 1900 + ltm->tm_year;
    int currentMonth = 1 + ltm->tm_mon;
    int currentDay = ltm->tm_mday;

    cout << "Oggi e': " << setfill('0') << setw(2) << currentDay << "/"
         << setw(2) << currentMonth << "/" << currentYear << endl;

    // Inserimento data dell'esame
    int examYear, examMonth, examDay;
    cout << "Inserisci la data dell'esame (YYYY MM DD): ";
    cin >> examYear >> examMonth >> examDay;

    // Calcola giorni rimanenti
    time_t examTime = convertToTimeT(examYear, examMonth, examDay);
    int daysRemaining = difftime(examTime, now) / (60 * 60 * 24);

    if (daysRemaining < 0) {
        cout << "La data dell'esame e' gia' passata!" << endl;
        return 1;
    }

    cout << "Giorni rimanenti: " << daysRemaining << endl;

    // Inserimento numero moduli
    int totalModules, currentModule;
    cout << "Inserisci il numero totale di moduli: ";
    cin >> totalModules;
    cout << "Inserisci il numero del modulo in cui sei attualmente: ";
    cin >> currentModule;

    int remainingModules = totalModules - currentModule;
    if (remainingModules <= 0) {
        cout << "Hai gia' completato tutti i moduli!" << endl;
        return 1;
    }

    cout << "Moduli rimanenti: " << remainingModules << endl;

    // Calcolo moduli da studiare al giorno
    double modulesPerDay = (double)remainingModules / daysRemaining;
    cout << "Devi studiare circa " << fixed << setprecision(2) << modulesPerDay << " moduli al giorno." << endl;

    return 0;
}