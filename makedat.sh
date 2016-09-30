#!/bin/bash

# script file to make .dat file in MG/ME/Myproc/Events/ (KM: 2011/12/02)

cd rs1000_se12/
grep '0.0000E+00' ma_plot004 | cut -c5-100 > pt.dat
grep '0.0000E+00' ma_plot007 | cut -c5-100 > e.dat
grep '0.0000E+00' ma_plot010 | cut -c5-100 > y.dat
grep '0.0000E+00' ma_plot013 | cut -c5-100 > cos.dat
grep '0.0000E+00' ma_plot019 | cut -c5-100 > minv.dat
cd ..

cd rs1000/
grep '0.0000E+00' ma_plot004 | cut -c5-100 > pt.dat
grep '0.0000E+00' ma_plot007 | cut -c5-100 > e.dat
grep '0.0000E+00' ma_plot010 | cut -c5-100 > y.dat
grep '0.0000E+00' ma_plot013 | cut -c5-100 > cos.dat
grep '0.0000E+00' ma_plot019 | cut -c5-100 > minv.dat
cd ..

cd rs1000_se4/
grep '0.0000E+00' ma_plot004 | cut -c5-100 > pt.dat
grep '0.0000E+00' ma_plot007 | cut -c5-100 > e.dat
grep '0.0000E+00' ma_plot010 | cut -c5-100 > y.dat
grep '0.0000E+00' ma_plot013 | cut -c5-100 > cos.dat
grep '0.0000E+00' ma_plot019 | cut -c5-100 > minv.dat
cd ..
