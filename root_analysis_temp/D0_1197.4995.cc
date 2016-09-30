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

   const Int_t PDGdQk = 1;
   const Int_t PDGuQk = 2;
   const Int_t PDGsQk = 3;
   const Int_t PDGcQk = 4;
   const Int_t PDGbQk = 5;

   // Selecton cuts
   const Double_t pTJetCut = 20.;
   const Double_t pTbCut = 20.;
   const Double_t pTElecCut = 20.;
   const Double_t pTMuonCut = 20.;
   const Double_t EtaJetCut = 2.5;
   const Double_t EtabCut = 2.5;
   const Double_t EtaElecCut = 1.1;
   const Double_t EtaMuonCut = 2.0;
   const Double_t drCut = 0.5;
   const Double_t pTmaxCut = 40.;
   const Double_t MissPTCut = 20.;
   const Int_t nbJetsCutLow  = 1;
   const Int_t nbJetsCutHigh = 2;
   const Double_t mttCut1 = 400.;
   const Double_t mttCut2 = 500.;
   
   // Mass and width
   const Double_t mW = 80.4;
   const Double_t mt = 172.5;

   const Double_t widthFactor = 0.28;
   Double_t dMWCut = mW * widthFactor;
   Double_t dMtCut = mt * widthFactor;

   // b-tagging parameters
   const Double_t effBTag = 0.70;
   const Double_t misBTag = 0.08;
   
   // Luminosity value
   const Double_t intLumi = 1.;

   std::cout<<std::endl;
   std::cout<<"  << Flags >>"<<std::endl;
   std::cout<<std::endl;
   std::cout<<"    b-Jets in final states : "<<bFlag<<std::endl;
   std::cout<<std::endl;
   std::cout<<"  << Sigma/Luminosity >>"<<std::endl;
   std::cout<<std::endl;
   std::cout<<"    Sigma Gen [pb]         : "<<sigmaGen<<std::endl;
   std::cout<<std::endl;
   std::cout<<"  << Selection Conditions >>"<<std::endl;
   std::cout<<std::endl;
   std::cout<<"    Leading pT jets        : "<<pTmaxCut<<std::endl;
   std::cout<<"    Mtt cut - 1            : "<<mttCut1<<std::endl;
   std::cout<<"    Mtt cut - 2            : "<<mttCut2<<std::endl;
   std::cout<<std::endl;
   std::cout<<"  << Mass and Width >>"<<std::endl;
   std::cout<<std::endl;
   std::cout<<"    W mass                 : "<<mW<<std::endl;
   std::cout<<"    W width cut            : "<<dMWCut<<std::endl;
   std::cout<<"    t mass                 : "<<mt<<std::endl;
   std::cout<<"    t width cut            : "<<dMtCut<<std::endl;
   std::cout<<std::endl;
   std::cout<<"  << b-Tagging Parameters >>"<<std::endl;
   std::cout<<std::endl;
   std::cout<<"    b-Tagg. Eff.           : "<<effBTag<<std::endl;
   std::cout<<"    mis-b-Tagg. Prob.      : "<<misBTag<<std::endl;
   std::cout<<std::endl;
   
   // List of selections
   std::vector<std::string> selectionList;
   
   selectionList.push_back("Input                : ");
   selectionList.push_back("Kinematical Cuts     : ");
   selectionList.push_back("Highest Jet pT       : ");
   selectionList.push_back("No. of b-Jets        : ");
   selectionList.push_back("x for W->lv rec.     : ");
   selectionList.push_back("No. of W->jj cand.   : ");
   selectionList.push_back("No. of tt-bar cand.  : ");
   
// Histograms
   Int_t nSelections = selectionList.size();
   Double_t dSelections = selectionList.size();
   TH1D* h_EventCounter = new TH1D("EventCounter", "Info",
                                   nSelections,0.,dSelections);
   TH1D* h_PtLep = new TH1D("PtLep","",50,0.,200.);
   TH1D* h_YLep = new TH1D("YLep","",60,-3.,3.);
   TH1D* h_PtNeu = new TH1D("PtNeu","",50,0.,200.);
   TH1D* h_MTLepNeu = new TH1D("MTLepNeu","",50,0.,200.);
   TH1D* h_xWlv = new TH1D("xWlv","",50,0.,10.);
   TH1D* h_DeltaEtaTrue = new TH1D("DeltaEtaTrue","",50,-1.,1.);
   TH1D* h_DeltaEtaRec = new TH1D("DeltaEtaRec","",50,0.,10.);
   TH1I* h_nWlv = new TH1I("nWlv","",5,0,5);
   TH1D* h_PtJet = new TH1D("PtJet","",50,0.,200.);
   TH1D* h_EtaJet = new TH1D("EtaJet","",60,-3.,3.);
   TH1D* h_PtBjt = new TH1D("PtBjt","",50,0.,200.);
   TH1D* h_EtaBjt = new TH1D("EtaBjt","",60,-3.,3.);
   TH1D* h_DeltaRLepJet = new TH1D("DeltaRLepJet","",60,0.,6.);
   TH1D* h_DeltaRLepBjt = new TH1D("DeltaRLepBjt","",60,0.,6.);
   TH1D* h_DeltaRJetJet = new TH1D("DeltaRJetJet","",60,0.,6.);
   TH1D* h_DeltaRJetBjt = new TH1D("DeltaRJetBjt","",60,0.,6.);
   TH1D* h_DeltaRBjtBjt = new TH1D("DeltaRBjtBjt","",60,0.,6.);
   TH1D* h_HighestPt = new TH1D("HighestPt","",50,0.,200.);
   TH1D* h_Mjj = new TH1D("Mjj","",50,0.,200.);
   TH1I* h_nJetsGen = new TH1I("nJetsGen","",6,0,6);
   TH1I* h_nbJetsGen = new TH1I("nbJetsGen","",6,0,6);
   TH1I* h_nJetsRec = new TH1I("nJetsRec","",6,0,6);
   TH1I* h_nbJetsRec = new TH1I("nbJetsRec","",6,0,6);
   TH1I* h_nWjj = new TH1I("nWjj","",10,0,10);
   TH1D* h_Mlvb = new TH1D("Mlvb","",50,100.,300.);
   TH1D* h_Mjjb = new TH1D("Mjjb","",50,100.,300.);
   TH1D* h_MlvbRec = new TH1D("MlvbRec","",50,100.,300.);
   TH1D* h_MjjbRec = new TH1D("MjjbRec","",50,100.,300.);
   TH1D* h_MttRec = new TH1D("MttRec","",100,200.,1200.);
   TH1D* h_MttScaled = new TH1D("MttScaled","",100,200.,1200.);
   TH2D* h_MttRec_MttScaled = new TH2D("MttRec_MttScaled","",
                                       500,300.,800.,500,300.,800.);
   TH1D* h_YLepRec = new TH1D("YLepRec","",30,-3.,3.);
   TH1D* h_YLepRec_MttRecS = new TH1D("YLepRec_MttRecS","",30,-3.,3.);
   TH1D* h_YLepRec_MttRecM = new TH1D("YLepRec_MttRecM","",30,-3.,3.);
   TH1D* h_YLepRec_MttRecL = new TH1D("YLepRec_MttRecL","",30,-3.,3.);
   TH1D* h_YLepRec_MttScaledS = new TH1D("YLepRec_MttScaledS","",30,-3.,3.);
   TH1D* h_YLepRec_MttScaledM = new TH1D("YLepRec_MttScaledM","",30,-3.,3.);
   TH1D* h_YLepRec_MttScaledL = new TH1D("YLepRec_MttScaledL","",30,-3.,3.);
   TH2D* h_MttRec_YLepRec = new TH2D("MttRec_YLepRec","",
                                     500,300.,800.,600,-3.,3.);
   TH2D* h_MttScaled_YLepRec = new TH2D("MttScaled_YLepRec","",
                                        500,300.,800.,600,-3.,3.);
   TH1D* h_YlvbRec = new TH1D("YlvbRec","",50,-5.,5.);
   TH2D* h_MttRec_YlvbRec = new TH2D("MttRec_YlvbRec","",
                                     500,300.,800.,500,-5.,5.);
   TH2D* h_MttScaled_YlvbRec = new TH2D("MttScaled_YlvbRec","",
                                        500,300.,800.,500,-5.,5.);
   TH1D* h_YjjbRec = new TH1D("YjjbRec","",50,-5.,5.);
   TH2D* h_MttRec_YjjbRec = new TH2D("MttRec_YjjbRec","",
                                     500,300.,800.,500,-5.,5.);
   TH2D* h_MttScaled_YjjbRec = new TH2D("MttScaled_YjjbRec","",
                                        500,300.,800.,500,-5.,5.);
   TH1D* h_DeltaYlvbjjb = new TH1D("DeltaYlvbjjb","",40,-4.,4.); 
   TH1D* h_DeltaYlvbjjb_MttRecS = new TH1D("DeltaYlvbjjb_MttRecS","",
                                           40,-4.,4.); 
   TH1D* h_DeltaYlvbjjb_MttRecM = new TH1D("DeltaYlvbjjb_MttRecM","",
                                           40,-4.,4.); 
   TH1D* h_DeltaYlvbjjb_MttRecL = new TH1D("DeltaYlvbjjb_MttRecL","",
                                           40,-4.,4.); 
   TH1D* h_DeltaYlvbjjb_MttScaledS = new TH1D("DeltaYlvbjjb_MttScaledS","",
                                              40,-4.,4.); 
   TH1D* h_DeltaYlvbjjb_MttScaledM = new TH1D("DeltaYlvbjjb_MttScaledM","",
                                              40,-4.,4.); 
   TH1D* h_DeltaYlvbjjb_MttScaledL = new TH1D("DeltaYlvbjjb_MttScaledL","",
                                              40,-4.,4.); 
   TH2D* h_MttRec_DeltaYlvbjjb = new TH2D("MttRec_DeltaYlvbjjb","",
                                          500,300.,800.,500,-4.,4.);
   TH2D* h_MttScaled_DeltaYlvbjjb = new TH2D("MttScaled_DeltaYlvbjjb","",
                                             500,300.,800.,500,-4.,4.);
   TH1D* h_PTttScaled = new TH1D("PTttScaled","",50,0.,200.); 
   Double_t xbins[] = {0,15,30,45,100};
   TH1D* h_PTttScaled2 = new TH1D("PTttScaled2","",4,xbins); 

// Object Lists
   std::vector<Int_t> iElecList;
   std::vector<Int_t> iMuonList;
   std::vector<Int_t> iLeptonList;
   std::vector<Int_t> iGammaList;
   std::vector<Int_t> iJetList;
   std::vector<Int_t> ibJetList;
   std::vector<TLorentzVector*> wlvList(2);
   std::vector<TLorentzVector*> jetList;
   std::vector<TLorentzVector*> bJetList;
   std::vector<Int_t> iWjjList;
   std::vector<Int_t> jWjjList;
   
// Event Loop
   Double_t nEvents = 0.;
   Long64_t nentries = fChain->GetEntriesFast();
   Long64_t nbytes = 0, nb = 0;
   for (Long64_t jentry=0; jentry<nentries;jentry++) {
      Long64_t ientry = LoadTree(jentry);
      if (ientry < 0) break;
      nb = fChain->GetEntry(jentry);   nbytes += nb;

// Initialize Object Lists      
      iElecList.clear();
      iMuonList.clear();
      iLeptonList.clear();
      iGammaList.clear();
      iJetList.clear();
      ibJetList.clear();
      jetList.clear();
      bJetList.clear();
      wlvList.clear();
      iWjjList.clear();
      jWjjList.clear();

// Count Input events      
      h_EventCounter->Fill(0.);
      nEvents++;

// Listing Particles
// Kinematical cuts
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

// Missing PT cut
      TLorentzVector MissMom(0.,0.,0.,0.);
      for (Int_t i=0;i<Particle_;++i) {
	if (Particle_Status[i]==1) {
	  Int_t iCode = abs(Particle_PID[i]);
	  if (iCode==PDGNeuE || iCode==PDGNeuM) continue;
	  TLorentzVector ParticleMom(Particle_Px[i],Particle_Py[i],
				     Particle_Pz[i],Particle_E[i]);
	  MissMom = MissMom -ParticleMom;
	}
      }
      Double_t MissPT = MissMom.Pt();
      if (iElecList.size() == 1) {
	if (MissPT<MissPTCut) continue;
    } else if (iMuonList.size() == 1) {
	if (MissPT<MissPTCut || MissPT>250 ) continue;
    } else { 
	continue; 
    }
      
// Lepton-Jet DeltaR cut
      std::vector<Int_t>::iterator it1;
      std::vector<Int_t>::iterator it2;
      for (it2=iLeptonList.begin();it2!=iLeptonList.end();++it2) {
	Int_t iit2 = *it2;
	Double_t Eta2 = Particle_Eta[iit2];
	Double_t Phi2 = Particle_Phi[iit2];
	for (it1=iJetList.begin();it1!=iJetList.end();++it1) {
	  Int_t iit1 = *it1;
	  Double_t Eta1 = Particle_Eta[iit1];
	  Double_t Phi1 = Particle_Phi[iit1];
	  Double_t Dr12 = sqrt( (Eta1 -Eta2)*(Eta1 -Eta2) +(Phi1 -Phi2)*(Phi1 -Phi2) );
	  if ( Dr12<drCut ) {
	    it1 = iJetList.erase(it1);
	    break;
	  }
      	}
      } 
      
// Lepton-b DeltaR cut
      for (it2=iLeptonList.begin();it2!=iLeptonList.end();++it2) {
	Int_t iit2 = *it2;
	Double_t Eta2 = Particle_Eta[iit2];
	Double_t Phi2 = Particle_Phi[iit2];
	for (it1=ibJetList.begin();it1!=ibJetList.end();++it1) {
	  Int_t iit1 = *it1;
	  Double_t Eta1 = Particle_Eta[iit1];
	  Double_t Phi1 = Particle_Phi[iit1];
	  Double_t Dr12 = sqrt( (Eta1 -Eta2)*(Eta1 -Eta2) +(Phi1 -Phi2)*(Phi1 -Phi2) );
	  if ( Dr12<drCut ) {
	    it1 = ibJetList.erase(it1);
	    break;
	  }
      	}
      } 
      
// Jet multiplicity cut
      Int_t nJets = iJetList.size();
      Int_t nbJets = ibJetList.size();
      Int_t ntotalJets = nJets +nbJets;
      if ( ntotalJets < 4 ) continue;

// Lepton multiplicity cut
      Int_t nLepton = iLeptonList.size();
      if (nLepton != 1) continue;

      h_EventCounter->Fill(1.);
      h_nJetsGen->Fill(nJets);
      h_nbJetsGen->Fill(nbJets);
      
// Highest pT cut      
      std::vector<Int_t>::iterator it;
      Double_t pTmax = 0.;
      Int_t iPTmax = -1;
      for (it=iJetList.begin();it!=iJetList.end();++it) {
         Int_t ijt = *it;
         if (Particle_PT[ijt]>pTmax) {
            iPTmax = ijt;
            pTmax = Particle_PT[ijt];
         }
      }
      for (it=ibJetList.begin();it!=ibJetList.end();++it) {
         Int_t ijt = *it;
         if (Particle_PT[ijt]>pTmax) {
            iPTmax = ijt;
            pTmax = Particle_PT[ijt];
         }
      }
      h_HighestPt->Fill(pTmax);
      if (pTmax<pTmaxCut) continue;
      h_EventCounter->Fill(2.);

//  b-tagging       
      TRandom3 rndm(0);
      for (it=ibJetList.begin();it!=ibJetList.end();) {
	if (rndm.Rndm()>effBTag) {
	  it = ibJetList.erase(it);
	  continue;
	}
	++it;
      }
      for (it=iJetList.begin();it!=iJetList.end();) {
	Int_t ijt = *it;
	if (rndm.Rndm()<misBTag) {
	  ibJetList.push_back(ijt);
	  it = iJetList.erase(it);
	  continue;
	}
	++it;
      }

// re-conostruct the q-jet list
      iJetList.clear();
      for (Int_t i=0;i<Particle_;++i) {
	if (Particle_Status[i]==1) {
	  Int_t iCode = abs(Particle_PID[i]);
	  if (iCode==PDGGlu ||
	      (iCode>=PDGdQk && iCode<=PDGbQk)) {
	    bool found = false;
	    for (it=ibJetList.begin();it!=ibJetList.end();++it) {
	      if (*it==i) {
		found = true;
		break;
	      }
	    }
	    if (!found) {
	      iJetList.push_back(i);
	    }
	  }
	}
      }
   
// no. of reconstructed jets after b-tagging      
// b multiplicity cut
      nJets = iJetList.size();
      h_nJetsRec->Fill(nJets);
      nbJets = ibJetList.size();
      h_nbJetsRec->Fill(nbJets);
      if (nbJets<nbJetsCutLow || nbJets>nbJetsCutHigh) continue;
      h_EventCounter->Fill(3.);

// reconstruct W->lv      
      TLorentzVector lep(Particle_Px[ipLep],Particle_Py[ipLep],
                         Particle_Pz[ipLep],Particle_E[ipLep]);

      TLorentzVector neu(Particle_Px[ipNeu],Particle_Py[ipNeu],
                         Particle_Pz[ipNeu],Particle_E[ipNeu]);

      h_PtLep->Fill(lep.Pt());
      h_YLep->Fill(lep.Rapidity());

      TLorentzVector lepT(lep.Px(),lep.Py(),0.,
                          sqrt(lep.Px()*lep.Px()+lep.Py()*lep.Py()));
      TLorentzVector neuT(neu.Px(),neu.Py(),0.,
                          sqrt(neu.Px()*neu.Px()+neu.Py()*neu.Py()));
      h_MTLepNeu->Fill((lepT+neuT).M());
      
      h_PtNeu->Fill(neu.Pt());

   // check two possible solutions
      Double_t x = mW*mW/(2.*lep.Pt()*neu.Pt())
         + cos(DelPhi(lep.Phi(),neu.Phi()));
      h_xWlv->Fill(x);

      if (x<1.) continue;
      h_EventCounter->Fill(4.);
      
      Double_t etaNeu = neu.Eta();
      Double_t etaNeu1 = lep.Eta()+acosh(x);
      Double_t etaNeu2 = lep.Eta()-acosh(x);
      h_DeltaEtaTrue->Fill(etaNeu1-etaNeu);
      h_DeltaEtaTrue->Fill(etaNeu2-etaNeu);
      h_DeltaEtaRec->Fill(etaNeu1-etaNeu2);

      Double_t phiMiss = atan2(neu.Py(),neu.Px());
      Double_t pTMiss = neu.Pt();
      
      for (Int_t i=0;i<2;i++) {
         Double_t sign = (Double_t)(2*i-1);
         Double_t etaMiss = lep.Eta()-sign*acosh(x);
         TLorentzVector neuRec;
         neuRec.SetPtEtaPhiM(pTMiss,etaMiss,phiMiss,0.);
         TLorentzVector* wlv = new TLorentzVector(lep+neuRec);
         wlvList.push_back(wlv);
      }
      Int_t nWlv = wlvList.size();
      h_nWlv->Fill(nWlv);
      
      for (it=iJetList.begin();it!=iJetList.end();++it) {
         Int_t ipjt = *it;
         TLorentzVector* jet = new TLorentzVector(Particle_Px[ipjt],
                                                  Particle_Py[ipjt],
                                                  Particle_Pz[ipjt],
                                                  Particle_E[ipjt]);
         jetList.push_back(jet);
         h_PtJet->Fill(jet->Pt());
         h_EtaJet->Fill(jet->Eta());

         h_DeltaRLepJet->Fill(lep.DeltaR(*jet));
      }
      
      for (it=ibJetList.begin();it!=ibJetList.end();++it) {
         Int_t ipbjt = *it;
         TLorentzVector* bjet = new TLorentzVector(Particle_Px[ipbjt],
                                                   Particle_Py[ipbjt],
                                                   Particle_Pz[ipbjt],
                                                   Particle_E[ipbjt]);
         bJetList.push_back(bjet);
         h_PtBjt->Fill(bjet->Pt());
         h_EtaBjt->Fill(bjet->Eta());

         h_DeltaRLepBjt->Fill(lep.DeltaR(*bjet));
      }

      for (Int_t i=0;i<nJets-1;++i) {
         TLorentzVector ijet = *(jetList.at(i));
         for (Int_t j=i+1;j<nJets;++j) {
            TLorentzVector jjet = *(jetList.at(j));
            h_DeltaRJetJet->Fill(ijet.DeltaR(jjet));
            Double_t mjj = (ijet+jjet).M();
            h_Mjj->Fill(mjj);
            if (mjj>mW-dMWCut && mjj<mW+dMWCut) {
               iWjjList.push_back(i);
               jWjjList.push_back(j);
            }
         }
      }

      for (Int_t i=0;i<nbJets-1;++i) {
         TLorentzVector ibjet = *(bJetList.at(i));
         for (Int_t j=i+1;j<nbJets;++j) {
            TLorentzVector jbjet = *(bJetList.at(j));
            h_DeltaRBjtBjt->Fill(ibjet.DeltaR(jbjet));
         }
      }
      
      for (Int_t i=0;i<nJets;++i) {
         TLorentzVector jet = *(jetList.at(i));
         for (Int_t j=0;j<nbJets;++j) {
            TLorentzVector bjet = *(bJetList.at(j));
            h_DeltaRJetBjt->Fill(jet.DeltaR(bjet));
         }
      }

      Int_t nWjj = iWjjList.size();
      h_nWjj->Fill(nWjj);
      if (nWjj<1) continue;
      h_EventCounter->Fill(5.);

      Int_t ittMin = -1;
      Double_t deltaMMin = 0.;

      TLorentzVector wjj;
      TLorentzVector bjet1;
      TLorentzVector bjet2;

      TLorentzVector lvb;
      TLorentzVector jjb;

      Double_t mlvb;
      Double_t mjjb;
      
      for (Int_t i=0;i<nWjj;++i) {

         Int_t iWjj = iWjjList.at(i);
         Int_t jWjj = jWjjList.at(i);

         wjj = *(jetList.at(iWjj))+*(jetList.at(jWjj));
         
         bjet1 = *(bJetList.at(0));
         
         if (nbJets==1) {
            
            for (Int_t j=0;j<nJets;++j) {
               if (j!=iWjj && j!=jWjj) {
                  bjet2 = *(jetList.at(j));
                  break;
               }
            }
            
         } else if (nbJets==2){
            
            bjet2 = *(bJetList.at(1));
            
         }
         
         for (Int_t j=0;j<nWlv;++j) {

            TLorentzVector wlv = *(wlvList.at(j));
            
            mlvb = (wlv+bjet1).M();
            mjjb = (wjj+bjet2).M();

            h_Mlvb->Fill(mlvb);
            h_Mjjb->Fill(mjjb);
            
            if ((mlvb>mt-dMtCut && mlvb<mt+dMtCut) &&
                (mjjb>mt-dMtCut && mjjb<mt+dMtCut)) {
               
               Double_t deltaM = (mlvb-mt)*(mlvb-mt) + (mjjb-mt)*(mjjb-mt);
               
               if (ittMin==-1 || deltaM<deltaMMin) {
                  ittMin=1;
                  deltaMMin = deltaM;
                  lvb = wlv+bjet1;
                  jjb = wjj+bjet2;
               }
               
            }

            mlvb = (wlv+bjet2).M();
            mjjb = (wjj+bjet1).M();
            
            h_Mlvb->Fill(mlvb);
            h_Mjjb->Fill(mjjb);
            
            if ((mlvb>mt-dMtCut && mlvb<mt+dMtCut) &&
                (mjjb>mt-dMtCut && mjjb<mt+dMtCut)) {
               
               Double_t deltaM = (mlvb-mt)*(mlvb-mt) + (mjjb-mt)*(mjjb-mt);
               
               if (ittMin==-1 || deltaM<deltaMMin) {
                  ittMin=1;
                  deltaMMin = deltaM;
                  lvb = wlv+bjet2;
                  jjb = wjj+bjet1;
               }
               
            }
            
         }

      }

      if (ittMin==-1) continue;
      h_EventCounter->Fill(6.);

// final

      Double_t yLep = qLep*lep.Rapidity();
      
      h_YLepRec->Fill(yLep);
      
      mlvb = lvb.M();
      mjjb = jjb.M();

      h_MlvbRec->Fill(mlvb);
      h_MjjbRec->Fill(mjjb);

      Double_t ylvb = lvb.Rapidity();
      Double_t yjjb = jjb.Rapidity();

      h_YlvbRec->Fill(qLep*ylvb);
      h_YjjbRec->Fill(qLep*yjjb);

      Double_t deltaYlvbjjb = qLep * (ylvb-yjjb);
      h_DeltaYlvbjjb->Fill(deltaYlvbjjb);
      
      TLorentzVector tt(lvb+jjb);

      Double_t mttRec = tt.M();

      h_MttRec->Fill(mttRec);

      h_MttRec_YLepRec->Fill(mttRec,qLep*yLep);

      h_MttRec_YlvbRec->Fill(mttRec,qLep*ylvb);
      h_MttRec_YjjbRec->Fill(mttRec,qLep*yjjb);

      h_MttRec_DeltaYlvbjjb->Fill(mttRec,deltaYlvbjjb);

      if (mttRec<mttCut1) {
         h_YLepRec_MttRecS->Fill(qLep*yLep);
         h_DeltaYlvbjjb_MttRecS->Fill(deltaYlvbjjb);
      } else if (mttRec<mttCut2) {
         h_YLepRec_MttRecM->Fill(qLep*yLep);
         h_DeltaYlvbjjb_MttRecM->Fill(deltaYlvbjjb);
      } else {
         h_YLepRec_MttRecL->Fill(qLep*yLep);
         h_DeltaYlvbjjb_MttRecL->Fill(deltaYlvbjjb);
      }
      
      TLorentzVector lvbScaled = (mt/mlvb)*lvb;
      TLorentzVector jjbScaled = (mt/mjjb)*jjb;

      TLorentzVector ttScaled(lvbScaled+jjbScaled);

      Double_t mttScaled = ttScaled.M();
      Double_t ptttScaled = ttScaled.Pt();

      h_PTttScaled->Fill(ptttScaled);
      h_PTttScaled2->Fill(ptttScaled);

      h_MttScaled->Fill(mttScaled);
      h_MttRec_MttScaled->Fill(mttRec,mttScaled);
      
      h_MttScaled_YLepRec->Fill(mttScaled,qLep*yLep);

      h_MttScaled_YlvbRec->Fill(mttScaled,qLep*ylvb);
      h_MttScaled_YjjbRec->Fill(mttScaled,qLep*yjjb);

      h_MttScaled_DeltaYlvbjjb->Fill(mttScaled,deltaYlvbjjb);

      if (mttScaled<mttCut1) {
         h_YLepRec_MttScaledS->Fill(qLep*yLep);
         h_DeltaYlvbjjb_MttScaledS->Fill(deltaYlvbjjb);
      } else if (mttScaled<mttCut2) {
         h_YLepRec_MttScaledM->Fill(qLep*yLep);
         h_DeltaYlvbjjb_MttScaledM->Fill(deltaYlvbjjb);
      } else {
         h_YLepRec_MttScaledL->Fill(qLep*yLep);
         h_DeltaYlvbjjb_MttScaledL->Fill(deltaYlvbjjb);
      }
      
      std::vector<TLorentzVector*>::iterator ijt;

      for (ijt=wlvList.begin();ijt!=wlvList.end();++ijt) {
         delete *ijt;
      }

      for (ijt=jetList.begin();ijt!=jetList.end();++ijt) {
         delete *ijt;
      }

      for (ijt=bJetList.begin();ijt!=bJetList.end();++ijt) {
         delete *ijt;
      }


//      std::cout<<"end of event loop"<<std::endl;
      
   }

   std::cout<<std::endl<<std::endl;
   std::cout<<"  << Event Statistics (no. of events: raw) >>"<<std::endl;
   std::cout<<std::endl;
   for (Int_t i=0;i<nSelections;i++) {
      if (i==0) {
         std::cout<<std::fixed<<"    ["<<std::setw(2)<<i<<"] "
                  <<std::setw(10)<<std::setprecision(0)<<selectionList[i]
                  <<std::setw(6)<<h_EventCounter->GetBinContent(i+1)<<std::endl;      } else {
         if (h_EventCounter->GetBinContent(i)>0) {
            std::cout<<std::fixed<<"    ["<<std::setw(2)<<i<<"] "
                     <<std::setw(10)<<std::setprecision(0)<<selectionList[i]
                     <<std::setw(6)<<h_EventCounter->GetBinContent(i+1)
                     <<" ("<<std::setw(6)<<std::setprecision(2)
                     <<100.*h_EventCounter->GetBinContent(i+1)/h_EventCounter->GetBinContent(1)
                     <<"%, "
                     <<std::setw(6)<<std::setprecision(2)
                     <<100.*h_EventCounter->GetBinContent(i+1)/h_EventCounter->GetBinContent(i)
                     <<"%) "<<std::endl;
         } else {
            std::cout<<"    ["<<std::setw(2)<<i<<"] "<<selectionList[i]
                     <<std::setw(6)<<h_EventCounter->GetBinContent(i+1)
                     <<std::endl;
         }
      }
   }

   if (sigmaGen>0.) {
      
      Double_t intLumiGen = nEvents/sigmaGen;
      
      Double_t normLumi = intLumi/intLumiGen;
      
//   m_normTot = m_normLumi*m_normIneff;
      Double_t normTot = normLumi*bTagEnhanceFactor;
      
      std::cout<<std::endl;
      std::cout<<"  << Normalization Factors >>"<<std::endl;
      std::cout<<std::endl;
      std::cout<<"    Sigma Gen [fb]      : "<<sigmaGen<<std::endl;
      std::cout<<"    No. of Total Events : "
               <<std::setprecision(0)<<nEvents<<std::endl;
      std::cout<<"    Lumi Gen  [fb-1]    : "
               <<std::setprecision(2)<<intLumiGen<<std::endl;
      std::cout<<"    Fact (lumi)         : "
               <<std::setprecision(5)<<normLumi<<std::endl;
      std::cout<<"    Fact (b-enhance)    : "
               <<std::setprecision(5)<<bTagEnhanceFactor<<std::endl;
      std::cout<<"    Fact (total)        : "
               <<std::setprecision(5)<<normTot<<std::endl;
      
      std::cout<<std::endl;
      std::cout<<"  << Event Statistics (normalized) ("
               <<std::setprecision(2)<<intLumi
               <<" fb-1) >>"<<std::endl;
      std::cout<<std::endl;
      for (Int_t i=0;i<nSelections;++i) {
         if (i==0) {
            std::cout<<"    ["<<std::setw(2)<<i<<"] "<<selectionList[i]
                     <<std::setw(6)
                     <<normTot*h_EventCounter->GetBinContent(i+1)<<std::endl;
         } else {
            if (h_EventCounter->GetBinContent(i)>0) {
               std::cout<<"    ["<<std::setw(2)<<i<<"] "<<selectionList[i]
                        <<std::setw(6)
                        <<normTot*h_EventCounter->GetBinContent(i+1)
                        <<" ("<<std::setw(6)
                        <<100.*h_EventCounter->GetBinContent(i+1)/h_EventCounter->GetBinContent(1)
                        <<"%, "
                        <<std::setw(6)
                        <<100.*h_EventCounter->GetBinContent(i+1)/h_EventCounter->GetBinContent(i)
                        <<"%) "<<std::endl;
            } else {
              std::cout<<"    ["<<std::setw(2)<<i<<"] "<<selectionList[i]
                        <<std::setw(6)
                       <<normTot*h_EventCounter->GetBinContent(i+1)<<std::endl;
            }
         }
      }

      TH1D* h_YLepRec_Norm = (TH1D*)h_YLepRec->Clone("YLepRec_Norm");
      TH1D* h_YlvbRec_Norm = (TH1D*)h_YlvbRec->Clone("YlvbRec_Norm");
      TH1D* h_YjjbRec_Norm = (TH1D*)h_YjjbRec->Clone("YjjbRec_Norm");
      TH1D* h_DeltaYlvbjjb_Norm =
         (TH1D*)h_DeltaYlvbjjb->Clone("DeltaYlvbjjb_Norm");

      TH1D* h_YLepRec_MttRecS_Norm =
         (TH1D*)h_YLepRec_MttRecS->Clone("YLepRec_MttRecS_Norm");
      TH1D* h_YLepRec_MttRecM_Norm =
         (TH1D*)h_YLepRec_MttRecM->Clone("YLepRec_MttRecM_Norm");
      TH1D* h_YLepRec_MttRecL_Norm =
         (TH1D*)h_YLepRec_MttRecL->Clone("YLepRec_MttRecL_Norm");

      TH1D* h_DeltaYlvbjjb_MttRecS_Norm =
         (TH1D*)h_DeltaYlvbjjb_MttRecS->Clone("DeltaYlvbjjb_MttRecS_Norm");
      TH1D* h_DeltaYlvbjjb_MttRecM_Norm =
         (TH1D*)h_DeltaYlvbjjb_MttRecM->Clone("DeltaYlvbjjb_MttRecM_Norm");
      TH1D* h_DeltaYlvbjjb_MttRecL_Norm =
         (TH1D*)h_DeltaYlvbjjb_MttRecL->Clone("DeltaYlvbjjb_MttRecL_Norm");
      
      TH1D* h_YLepRec_MttScaledS_Norm =
         (TH1D*)h_YLepRec_MttScaledS->Clone("YLepRec_MttScaledS_Norm");
      TH1D* h_YLepRec_MttScaledM_Norm =
         (TH1D*)h_YLepRec_MttScaledM->Clone("YLepRec_MttScaledM_Norm");
      TH1D* h_YLepRec_MttScaledL_Norm =
         (TH1D*)h_YLepRec_MttScaledL->Clone("YLepRec_MttScaledL_Norm");

      TH1D* h_DeltaYlvbjjb_MttScaledS_Norm =
         (TH1D*)h_DeltaYlvbjjb_MttScaledS->Clone("DeltaYlvbjjb_MttScaledS_Norm");
      TH1D* h_DeltaYlvbjjb_MttScaledM_Norm =
         (TH1D*)h_DeltaYlvbjjb_MttScaledM->Clone("DeltaYlvbjjb_MttScaledM_Norm");
      TH1D* h_DeltaYlvbjjb_MttScaledL_Norm =
         (TH1D*)h_DeltaYlvbjjb_MttScaledL->Clone("DeltaYlvbjjb_MttScaledL_Norm");
      
      h_YLepRec_Norm->Scale(normTot/h_YLepRec_Norm->GetBinWidth(1));
      h_YlvbRec_Norm->Scale(normTot/h_YlvbRec_Norm->GetBinWidth(1));
      h_YjjbRec_Norm->Scale(normTot/h_YjjbRec_Norm->GetBinWidth(1));
      h_DeltaYlvbjjb_Norm->Scale(normTot/h_DeltaYlvbjjb_Norm->GetBinWidth(1));

      h_YLepRec_MttRecS_Norm
         ->Scale(normTot/h_YLepRec_MttRecS_Norm->GetBinWidth(1));
      h_YLepRec_MttRecM_Norm
         ->Scale(normTot/h_YLepRec_MttRecM_Norm->GetBinWidth(1));
      h_YLepRec_MttRecL_Norm
         ->Scale(normTot/h_YLepRec_MttRecL_Norm->GetBinWidth(1));

      h_DeltaYlvbjjb_MttRecS_Norm
         ->Scale(normTot/h_DeltaYlvbjjb_MttRecS_Norm->GetBinWidth(1));
      h_DeltaYlvbjjb_MttRecM_Norm
         ->Scale(normTot/h_DeltaYlvbjjb_MttRecM_Norm->GetBinWidth(1));
      h_DeltaYlvbjjb_MttRecL_Norm
         ->Scale(normTot/h_DeltaYlvbjjb_MttRecL_Norm->GetBinWidth(1));

      h_YLepRec_MttScaledS_Norm
         ->Scale(normTot/h_YLepRec_MttScaledS_Norm->GetBinWidth(1));
      h_YLepRec_MttScaledM_Norm
         ->Scale(normTot/h_YLepRec_MttScaledM_Norm->GetBinWidth(1));
      h_YLepRec_MttScaledL_Norm
         ->Scale(normTot/h_YLepRec_MttScaledL_Norm->GetBinWidth(1));

      h_DeltaYlvbjjb_MttScaledS_Norm
         ->Scale(normTot/h_DeltaYlvbjjb_MttScaledS_Norm->GetBinWidth(1));
      h_DeltaYlvbjjb_MttScaledM_Norm
         ->Scale(normTot/h_DeltaYlvbjjb_MttScaledM_Norm->GetBinWidth(1));
      h_DeltaYlvbjjb_MttScaledL_Norm
         ->Scale(normTot/h_DeltaYlvbjjb_MttScaledL_Norm->GetBinWidth(1));
      
      std::cout<<std::endl;
      std::cout<<"  << Asymmetries >>"<<std::endl;
      std::cout<<std::endl;

      Double_t asymm, err;
      AsymmHist(h_YLepRec, asymm, err);
      std::cout<<"    Rec. Lepton          : "<<asymm<<" +- "<<err<<std::endl;
      AsymmHist(h_DeltaYlvbjjb, asymm, err);
      std::cout<<"    Delta_y_t            : "<<asymm<<" +- "<<err<<std::endl;
      AsymmHist(h_YlvbRec, asymm, err);
      std::cout<<"    Rec. Top (lep)       : "<<asymm<<" +- "<<err<<std::endl;
      AsymmHist(h_YjjbRec, asymm, err);
      std::cout<<"    Rec. Top (jet)       : "<<asymm<<" +- "<<err<<std::endl;
      std::cout<<std::endl;
      
   }

   std::cout<<std::endl;

}

int main(int argc, char* argv[])
{

   TROOT app("LHEAnalysis", "LHE Analysis Program");   

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
