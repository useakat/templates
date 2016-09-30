#include <iostream>
#include <iomanip>
#include <fstream>
#include <sstream>
#include <cmath>
#include <vector>

#define LHEAnalysis_cxx
#include "LHEAnalysis.h"
#include "TH2.h"
#include "TStyle.h"
#include "TCanvas.h"
#include "TFile.h"
#include "TLorentzVector.h"
#include "TRandom3.h"


Int_t bFlag;
Double_t sigmaGen;

void LHEAnalysis::Loop()
{
   if (fChain == 0) return;

// Constants

   // PDG codes
   const Int_t PDGElec = 11;
   const Int_t PDGMuon = 13;
   const Int_t PDGNeuE = 12;
   const Int_t PDGNeuM = 14;
   const Int_t PDGGlu = 21;
   const Int_t PDGGamma = 22;
   const Int_t PDGdQk = 1;
   const Int_t PDGuQk = 2;
   const Int_t PDGsQk = 3;
   const Int_t PDGcQk = 4;
   const Int_t PDGbQk = 5;
   const Int_t PDGgld = 1000039;
   const Int_t PDGpgld = 2000039;

   // Selecton cuts
   const Double_t pTJetCut = 0.;
   const Double_t pTbCut = 0.;
   const Double_t pTGammaCut = 10.;
   const Double_t pTElecCut = 10.;
   const Double_t pTMuonCut = 10.;
   const Double_t EtaJetCut = 5.;
   const Double_t EtabCut = 5.;
   const Double_t EtaGammaCut = 2.5;
   const Double_t EtaElecCut = 2.5;
   const Double_t EtaMuonCut = 2.5;
   const Double_t drCut = 0.4;
   const Double_t pTmaxCut = 0.;
   const Double_t MissPTCut = 0.;
   const Int_t nbJetsCutLow  = 1;
   const Int_t nbJetsCutHigh = 2;

   // b-tagging parameters
   const Double_t effBTag = 0.70;
   const Double_t misBTag = 0.08;

// Histograms
   TH1D* h_PtGamma1 = new TH1D("PtGamma1","",50,0.,700.);
   TH1D* h_MissPT = new TH1D("MissPT","",50,0.,600.);

   std::vector<Int_t> iElecList;
   std::vector<Int_t> iMuonList;
   std::vector<Int_t> iLeptonList;
   std::vector<Int_t> iGammaList;
   std::vector<Int_t> iJetList;
   std::vector<Int_t> ibJetList;
   std::vector<TLorentzVector*> GammaList;
   std::vector<TLorentzVector*> jetList;
   std::vector<TLorentzVector*> bJetList;
   
   Double_t nEvents = 0.;
   
   Long64_t nentries = fChain->GetEntriesFast();

   Long64_t nbytes = 0, nb = 0;
   for (Long64_t jentry=0; jentry<nentries;jentry++) {
      Long64_t ientry = LoadTree(jentry);
      if (ientry < 0) break;
      nb = fChain->GetEntry(jentry);   nbytes += nb;
      // if (Cut(ientry) < 0) continue;
      
      iElecList.clear();
      iMuonList.clear();
      iLeptonList.clear();
      iGammaList.clear();
      iJetList.clear();
      ibJetList.clear();
      jetList.clear();
      bJetList.clear();

      Int_t ipLep;
      Int_t ipNeu;
      Double_t qLep;     

      for (Int_t i=0;i<Particle_;++i) {
         if (Particle_Status[i]==1) {
	   Int_t iCode = abs(Particle_PID[i]);

            if (iCode==PDGElec) {
               ipLep = i;
	       if (Particle_PT[i]<pTElecCut) continue;
	       if (Particle_Eta[i]>EtaElecCut) continue;
               if (Particle_PID[i]>0) {
                  qLep = -1.;
               } else {
                  qLep = 1.;
               }
               iElecList.push_back(i);
               iLeptonList.push_back(i);
	    } else if (iCode==PDGMuon) {
               ipLep = i;
	       if (Particle_PT[i]<pTMuonCut) continue;
	       if (Particle_Eta[i]>EtaMuonCut) continue;
               if (Particle_PID[i]>0) {
                  qLep = -1.;
               } else {
                  qLep = 1.;
               }
               iMuonList.push_back(i);
               iLeptonList.push_back(i);
            } else if (iCode==PDGNeuE || iCode==PDGNeuM) {
               ipNeu = i;
            } 

            if (iCode==PDGGamma) {
	       if (Particle_PT[i]<pTGammaCut) continue;
	       if (Particle_Eta[i]>EtaGammaCut) continue;
               iGammaList.push_back(i);
            } 

	    if (iCode==PDGGlu ||
                       (iCode>=PDGdQk && iCode<=PDGcQk)) {
	      if (Particle_PT[i]<pTJetCut) continue;
	      if (Particle_Eta[i]>EtaJetCut) continue; 
               iJetList.push_back(i);
            } else if (iCode==PDGbQk) {
	      if (Particle_PT[i]<pTbCut) continue;
	      if (Particle_Eta[i]>EtabCut) continue;
               ibJetList.push_back(i);
            }
         }
      }

      std::vector<Int_t>::iterator it;
      Double_t pTmax = 0.;
      Int_t iPTmax = -1;
      for (it=iGammaList.begin();it!=iGammaList.end();++it) {
         Int_t ijt = *it;
         if (Particle_PT[ijt]>pTmax) {
            iPTmax = ijt;
            pTmax = Particle_PT[ijt];
         }
      }
      h_PtGamma1->Fill(pTmax);

      TLorentzVector MissMom(0.,0.,0.,0.);
      for (Int_t i=0;i<Particle_;++i) {
	if (Particle_Status[i]==1) {
	  Int_t iCode = abs(Particle_PID[i]);
	  if (iCode==PDGNeuE || iCode==PDGNeuM) continue;
	  if (iCode==PDGgld || iCode==PDGpgld) continue;
	  TLorentzVector ParticleMom(Particle_Px[i],Particle_Py[i],
				     Particle_Pz[i],Particle_E[i]);
	  MissMom = MissMom -ParticleMom;
	}
      }
      Double_t MissPT = MissMom.Pt();
      h_MissPT->Fill(MissPT);

   }
}

int main(int argc, char* argv[])
{

   TROOT app("LHEAnalysis", "LHE Analysis Program");   

// for a single file   
//   TFile* f = new TFile("W4j_unweighted_events.root");
//   TTree* tree = (TTree*)gDirectory->Get("LHEF");

// for multilple files

   if (argc<1) {
      std::cout<<argv[0]<<" file_list"<<std::endl;
      return 1;
   }

   std::string fileList = argv[1];
   std::ifstream ifs(fileList.c_str());
   if (!ifs) {
      std::cout<<"Input file: "<<fileList<<" does not exist."<<std::endl;
      return -1;
   }

   std::string line;
   std::istringstream iss; 
   
   std::getline(ifs,line);
   iss.clear();
   iss.str(line);
   iss>>bFlag;

   std::getline(ifs,line);
   iss.clear();
   iss.str(line);
   iss>>sigmaGen;
   
   TChain* chain = new TChain("LHEF");

   std::cout<<std::endl;
   std::cout<<"  << Input Files >>"<<std::endl<<std::endl;
   
   Int_t nfiles = 0;
   while (true) {
      std::getline(ifs,line);
      if (ifs.eof()) break;
      chain->Add(line.c_str());
      nfiles++;
      std::cout<<"    "<<nfiles<<": "<<line<<std::endl;
   }
   TTree* tree = chain;

   if (nfiles==0) {
      std::cout<<"No. of input files = 0"<<std::endl;
      return 2;
   }

   const std::string strPeriod = ".";

   std::string fileOut;
   std::string::size_type index = fileList.rfind(strPeriod);
   if (index==std::string::npos) {
      fileOut = fileList;
   } else {
      fileOut = fileList.substr(0,index);
   }
   fileOut = "LHEAnalysis_"+fileOut+".root";
   std::cout<<std::endl;
   std::cout<<"  << Output File >>"<<std::endl<<std::endl;
   std::cout<<"    "<<fileOut<<std::endl;

   TFile* fout = new TFile(fileOut.c_str(),"recreate");
   
   LHEAnalysis t(tree);

   t.Loop();

   fout->Write();
   fout->Close();
   
   return 0;
   
}

Double_t LHEAnalysis::DelPhi(Double_t phi1, Double_t phi2)
{
   Double_t x1 = cos(phi1);
   Double_t y1 = sin(phi1);

   Double_t x2 = cos(phi2);
   Double_t y2 = sin(phi2);

   Double_t sinphi = x1*y2-y1*x2;
   Double_t cosphi = x1*x2+y1*y2;

   Double_t phis = asin(sinphi);
   Double_t phic = acos(cosphi);
   
   if (cosphi>=0.) {
      return phis;
   } else if (sinphi>=0.) {
      return phic;
   } else {
      return -phic;
   }
   
}

void LHEAnalysis::AsymmHist(TH1D* hist, Double_t& asym, Double_t& err)
{

   Int_t nbin = hist->GetNbinsX();
   Int_t nbhalf = nbin/2;
   
   Double_t nf = 0.;
   Double_t nb = 0.;
   for (Int_t ibin=1;ibin<=nbin;++ibin) {
      Double_t y = hist->GetBinContent(ibin);
      if (ibin<=nbhalf) {
         nb += y;
      } else {
         nf += y;
      }
   }
   
   asym = (nf-nb)/(nf+nb);
   err = 2./(nf+nb)*sqrt((nf*nf-nf*nb+nb*nb)/(nf+nb));
   
}
