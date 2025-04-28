/* Differenze con il trattamento delle funzioni C e C++ */

#include<iostream>

using namespace std;
int fun(int i=2, int l=5);


int main()
{
    int numero;


    cout<<"fun="<<fun();
    return 0;
}

int fun(int a,int b)        // a e b non sono stati dichiarati quindi il compilatore li prende di default: 5 e 2 = 10
{
    return a*b;
}
