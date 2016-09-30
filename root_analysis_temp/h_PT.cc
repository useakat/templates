void h_PT()
{
  TFile* K1_M1 = new TFile("LHEAnalysis_K1_MpG1.root");
  TFile* K1_M100 = new TFile("LHEAnalysis_K1_MpG100.root");
  TFile* K1_M138 = new TFile("LHEAnalysis_K1_MpG138.root");
  TFile* K10_M1 = new TFile("LHEAnalysis_K10_MpG1.root");
  TFile* K10_M100 = new TFile("LHEAnalysis_K10_MpG100.root");
  TFile* K10_M138 = new TFile("LHEAnalysis_K10_MpG138.root");

  TH1D* PT_K1_M1 = (TH1D*)K1_M1->Get("PtGamma1");
  TH1D* MET_K1_M1 = (TH1D*)K1_M1->Get("MissPT");
  TH1D* PT_K1_M100 = (TH1D*)K1_M100->Get("PtGamma1");
  TH1D* MET_K1_M100 = (TH1D*)K1_M100->Get("MissPT");
  TH1D* PT_K1_M138 = (TH1D*)K1_M138->Get("PtGamma1");
  TH1D* MET_K1_M138 = (TH1D*)K1_M138->Get("MissPT");
  TH1D* PT_K10_M1 = (TH1D*)K10_M1->Get("PtGamma1");
  TH1D* MET_K10_M1 = (TH1D*)K10_M1->Get("MissPT");
  TH1D* PT_K10_M100 = (TH1D*)K10_M100->Get("PtGamma1");
  TH1D* MET_K10_M100 = (TH1D*)K10_M100->Get("MissPT");
  TH1D* PT_K10_M138 = (TH1D*)K10_M138->Get("PtGamma1");
  TH1D* MET_K10_M138 = (TH1D*)K10_M138->Get("MissPT");

  Double_t nevent_K1_M1 = PT_K1_M1->GetEntries();
  Double_t nevent_K1_M100 = PT_K1_M100->GetEntries();
  Double_t nevent_K1_M138 = PT_K1_M138->GetEntries();
  Double_t nevent_K10_M1 = PT_K10_M1->GetEntries();
  Double_t nevent_K10_M100 = PT_K10_M100->GetEntries();
  Double_t nevent_K10_M138 = PT_K10_M138->GetEntries();

  Double_t scale_K1_M1 = 1.;
  Double_t scale_K1_M100 = nevent_K1_M1/nevent_K1_M100;
  Double_t scale_K1_M138 = nevent_K1_M1/nevent_K1_M138;
  Double_t scale_K10_M1 = 1.;
  Double_t scale_K10_M100 = nevent_K10_M1/nevent_K10_M100;
  Double_t scale_K10_M138 = nevent_K10_M1/nevent_K10_M138;

  PT_K1_M100->Scale(scale_K1_M100);
  PT_K1_M138->Scale(scale_K1_M138);
  PT_K10_M100->Scale(scale_K10_M100);
  PT_K10_M138->Scale(scale_K10_M138);
  MET_K1_M100->Scale(scale_K1_M100);
  MET_K1_M138->Scale(scale_K1_M138);
  MET_K10_M100->Scale(scale_K10_M100);
  MET_K10_M138->Scale(scale_K10_M138);

  PT_K1_M1->SetLineColor(1);
  PT_K1_M100->SetLineColor(2);
  PT_K1_M138->SetLineColor(3);
  PT_K10_M1->SetLineColor(1);
  PT_K10_M100->SetLineColor(2);
  PT_K10_M138->SetLineColor(3);
  MET_K1_M1->SetLineColor(1);
  MET_K1_M100->SetLineColor(2);
  MET_K1_M138->SetLineColor(3);
  MET_K10_M1->SetLineColor(1);
  MET_K10_M100->SetLineColor(2);
  MET_K10_M138->SetLineColor(3);
 
  TCanvas* c0 = new TCanvas("c0","PT_K1",0,0,300,300);
  PT_K1_M100->Draw("");
  PT_K1_M1->Draw("same");
  //  PT_K1_M138->Draw("same");
  TLegend* legend = new TLegend(0.68,0.52,0.88,0.82,"","NDC");
  legend->AddEntry(PT_K1_M1, "Mpgld=1GeV");
  legend->AddEntry(PT_K1_M100, "Mpgld=100GeV");
  //legend->AddEntry(PT_K1_M138, "Mpgld=138GeV");
  legend->Draw("same");

  TCanvas* c1 = new TCanvas("c1","PT_K10",320,0,300,300);
  PT_K10_M100->Draw("");
  PT_K10_M1->Draw("same");
  //PT_K10_M138->Draw("same");
  TLegend* legend = new TLegend(0.68,0.52,0.88,0.82,"","NDC");
  legend->AddEntry(PT_K10_M1, "Mpgld=1GeV");
  legend->AddEntry(PT_K10_M100, "Mpgld=100GeV");
  //legend->AddEntry(PT_K10_M138, "Mpgld=138GeV");
  legend->Draw("same");

  TCanvas* c2 = new TCanvas("c2","MET_K1",0,320,300,300);
  MET_K1_M100->Draw("");
  MET_K1_M1->Draw("same");
  //MET_K1_M138->Draw("same");
  TLegend* legend = new TLegend(0.68,0.52,0.88,0.82,"","NDC");
  legend->AddEntry(MET_K1_M1, "Mpgld=1GeV");
  legend->AddEntry(MET_K1_M100, "Mpgld=100GeV");
  //legend->AddEntry(MET_K1_M138, "Mpgld=138GeV");
  legend->Draw("same");

  TCanvas* c3 = new TCanvas("c3","MET_K10",320,320,300,300);
  MET_K10_M100->Draw("");
  MET_K10_M1->Draw("same");
  //MET_K10_M138->Draw("same");
  TLegend* legend = new TLegend(0.68,0.52,0.88,0.82,"","NDC");
  legend->AddEntry(MET_K10_M1, "Mpgld=1GeV");
  legend->AddEntry(MET_K10_M100, "Mpgld=100GeV");
  //legend->AddEntry(MET_K10_M138, "Mpgld=138GeV");
  legend->Draw("same");

  c0->SaveAs("plots/PT_K1.eps");
  c1->SaveAs("plots/PT_K10.eps");
  c2->SaveAs("plots/MET_K1.eps");
  c3->SaveAs("plots/MET_K10.eps");
  }
