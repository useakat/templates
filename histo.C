{
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>

gROOT->Reset();


TH1F * myhisto = new TH1F("my histo", "my histo", 10, 0., 1.);

ifstream file;
file.open("error.dat", ios::in);

string line;
istringstream tmp;
double val;


while (getline(file,line)){
  tmp.clear();
  tmp.str(line);
  tmp >> val;
  
  myhisto->Fill(val); 
  


}

myhisto->Draw();



}
