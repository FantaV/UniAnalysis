#include <iostream>
#include <fstream>

using namespace std;

int main() {

  ofstream myfile;
  myfile.open("example.txt", ios::app);  // myfile.open(filename, mode)
  myfile << "Scrivi questa riga di append!\n";
  myfile.close();

  return 0;
}