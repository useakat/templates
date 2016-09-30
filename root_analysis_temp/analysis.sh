#!/bin/bash
make clean
make
./LHEAnalysis K1_MpG1.txt
#./LHEAnalysis K5_MpG1.txt
./LHEAnalysis K10_MpG1.txt
./LHEAnalysis K1_MpG100.txt
#./LHEAnalysis K5_MpG100.txt
./LHEAnalysis K10_MpG100.txt
./LHEAnalysis K1_MpG138.txt
#./LHEAnalysis K5_MpG138.txt
./LHEAnalysis K10_MpG138.txt