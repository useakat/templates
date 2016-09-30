{

#include <string>
#include <iomanip>
#include <fstream>
#include <sstream>
#include <vector>

    gROOT->Reset();

   //   TCanvas *c1 = new TCanvas("c1","Phasespace Dist");
   //   c1->Clear();
   
   std::string fname;
   std::cin>>fname;

   std::string fnin = fname + ".tdr";
   std::string fnout = fname + ".root";
   
   std::ifstream tdr(fnin.c_str());
   TFile* fout = new TFile(fnout.c_str(), "RECREATE");
   
   std::string line;
   
   while (true) {
       std::getline(tdr,line);

       if (tdr.eof()) break;

       //       std::cout<<"line = "<<line<<std::endl;
       std::string s_lpar = "(";

       if (line.find(s_lpar)!=std::string::npos) {

           std::string s_equal = "=";
           std::string s_dots = "......";

           std::string::size_type idx_begin = line.find(s_equal)+1;
           std::string::size_type idx_end = line.substr(idx_begin).find(s_dots);

           //           std::cout<<line.substr(idx_begin,idx_end)<<std::endl;;

           std::istringstream iss;
           iss.clear();
           iss.str(line.substr(idx_begin,idx_end));
           Int_t id;
           iss>>id;

           idx_begin = line.find(s_dots)+s_dots.size();
           idx_end = line.substr(idx_begin).find(s_dots);

           std::ostringstream oss;
           oss<<"id"<<std::setw(5)<<std::setfill('0')<<id;
           std::string name = oss.str();

           //           std::cout<<"name = "<<name<<std::endl;
           
           //           std::cout<<"id = "<<id<<std::endl;

           std::string s_hist = "hist";
           std::string s_title = "title top ";
           std::string s_order = "set order x y dy";
           std::string s_limit = "set limit x from";
           std::string s_apos = "'";
           std::string s_to = " to ";
           std::string s_blank = " ";

           std::string title;

           std::vector<Double_t> xList;
           std::vector<Double_t> yList;
           std::vector<Double_t> dyList;

           xList.clear();
           yList.clear();
           dyList.clear();
           while (true) {
               std::getline(tdr,line);

               //               std::cout<<"line = "<<line<<std::endl;
               if (tdr.eof()) break;

               Bool_t end = false; 
               if (line.find(s_title)!=std::string::npos) {
                   std::string::size_type idx_begin = line.find(s_apos)+s_apos.size();
                   std::string::size_type idx_end = (line.substr(idx_begin)).find(s_apos);
                   title = line.substr(idx_begin,idx_end);
                   std::string::size_type idx_blank;
                   for (Int_t i=title.size();i>0;i--) {
                       if (title.substr(i-1,1)!=s_blank) {
                           idx_blank = i;
                           break;
                       }
                   }
                   title = title.substr(0,idx_blank);
                   //                   std::string::size_type idx_blank = title.rfind_last_not_of(s_blank);
                   //                   title = title.substr(0,idx_blank);
                   //                   std::cout<<"title = "<<title<<std::endl;
               } else if (line.find(s_limit)!=std::string::npos) {
                   std::string::size_type idx_begin = line.find(s_limit);
                   std::string::size_type len =
                       (line.substr(idx_begin+s_limit.size())).find(s_to);
                   std::istringstream iss;
                   iss.clear();
                   iss.str(line.substr(idx_begin+s_limit.size(),len));
                   Double_t xmin,xmax;
                   iss>>xmin;
                   idx_begin = line.find(s_to);
                   iss.clear();
                   iss.str(line.substr(idx_begin+s_to.size()));
                   iss>>xmax;
                   //                   std::cout<<"xmin,max = "<<xmin<<", "<<xmax<<std::endl;
               } else if (line.find(s_order)!=std::string::npos) {

                   while (true) {
                       std::getline(tdr,line);
                       if (line.find(s_hist)!=std::string::npos) {
                           end = true;
                           break;
                       }

                       std::istringstream iss;
                       iss.clear();
                       iss.str(line);
                       Double_t x,y,dy;
                       iss>>x>>y>>dy;

                       xList.push_back(x);
                       yList.push_back(y);
                       dyList.push_back(dy);
                       
                   }

               }
               if (end) break;

           }
           Int_t ndiv = xList.size();

           std::cout<<" name, title = "<<name<<", "<<title<<std::endl;
           std::cout<<" ndiv,xmin,xmax = "<<ndiv<<", "<<xmin<<", "<<xmax<<std::endl;
           std::cout<<std::endl;
           TH1D* hist = new TH1D(name.c_str(),title.c_str(),
                                 ndiv, xmin, xmax);
           for (Int_t i=0;i<ndiv;i++) {
               hist->SetBinContent(i+1,yList[i]);
               hist->SetBinError(i+1,dyList[i]);
           }
           
       }

   }
   
   tdr.close();
   
   fout->Write();
   fout->Close();
   
}
