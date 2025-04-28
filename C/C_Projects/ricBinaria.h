#ifndef RICBINARIA_H
#define RICBINARIA_H

int ricBinaria (const int vettore[], int chiave) {
    int posMin = 0, posMax = SIZE - 1, posMedia;
    while ( posMin <= posMax ) {    // ad ogni giro l'array è ridotto
        posMedia = (posMin + posMax) / 2;
        if ( vettore[ posMedia ] == chiave )
            return posMedia;    // Trovato!
        else if (vettore[posMedia] > chiave )
            posMax = posMedia - 1;  // Cerca nella metà inferiore
        else
            posMin = posMedia + 1;  // Cerca nella metà superiore
    }
    return -1;    // valore chiave non trovato
}

#endif //RICBINARIA_H
