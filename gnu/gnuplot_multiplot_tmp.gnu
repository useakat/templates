set terminal postscript eps enhanced "Times-Roman" color 17
set output "mean_free_path_p.eps"
set logscale x
set logscale y
set format x "%L"
set format y "%L"
set xtics (0.0010.01,0.1,1,10,1.0E2,1.0E3,1.0E4,1.0E5,1.0E6,1.0E7,1.0E8,1.0E9,1.0E10,1.0E11,1E12,1E13,1E14,1E15,1E16)
set ytics (1,10,1E2,1E3,1E4,1E5,1E6,1E7,1E8,1E9,1E10)
set tics scale 2
set grid
set key at 1.0E3,1.0E7 samplen 2
set key spacing 1.5
#set key 7.0E10,1.0E-3
set xlabel 'log_{/=10 10}E_p (GeV)' offset -1,0
set ylabel 'log_{/=10 10} L (Mpc)' offset 1,0
set xrange [1E0:2.0E16]
set yrange [1.0:1.0E10]

set multiplot

plot \
'mean_free_path_mb=50_1.dat' every ::1::26 u 1:2 t 'CTEQ'  w l lt 1 lc rgb 'red' lw 8 ,\
'mean_free_path_mb=50_1.dat' every ::26::32 u 1:2 t '{/=1 .}'  w l lt 2 lc rgb 'red' lw 8 ,\
'mean_free_path2_mb=50_1.dat' every ::7::82 u 1:2 t 'BBT'  w l lt 1 lc rgb 'red' lw 1 ,\
'mean_free_path2_mb=50_1.dat' every ::1::7 u 1:2 t '{/=1 .}'  w l lt 2 lc rgb 'red' lw 1 ,\
'mean_free_path_mb=100_1.dat' every ::1::26u 1:2 notitle '{/=17 100 GeV (CTEQ)}'  w l lt 1 lc rgb 'red' lw 8 ,\
'mean_free_path_mb=100_1.dat' every ::26::32 u 1:2 notitle '{/=17 100 GeV (CTEQ)}'  w l lt 2 lc rgb 'red' lw 8 ,\
'mean_free_path2_mb=100_1.dat' every ::7::82 u 1:2 notitle '{/=17 (BBT)}'  w l lt 1 lc rgb 'red' lw 1 ,\
'mean_free_path2_mb=100_1.dat' every ::1::7 u 1:2 notitle '{/=17 (BBT)}'  w l lt 2 lc rgb 'red' lw 1 ,\
'mean_free_path_mb=200_1.dat' every ::1::26 u 1:2 notitle '{/=17 200 GeV (CTEQ)}'  w l lt 1 lc rgb 'red' lw 8 ,\
'mean_free_path_mb=200_1.dat' every ::26::32 u 1:2 notitle '{/=17 200 GeV (CTEQ)}'  w l lt 2 lc rgb 'red' lw 8 ,\
'mean_free_path2_mb=200_1.dat' every ::7::82 u 1:2 notitle '{/=17 (BBT)}'  w l lt 1 lc rgb 'red' lw 1 ,\
'mean_free_path2_mb=200_1.dat' every ::1::7 u 1:2 notitle '{/=17 (BBT)}'  w l lt 2 lc rgb 'red' lw 1 ,\
'mean_free_path.dat' u ($1*1.0E-9):2 notitle '{/=17 p-{/Symbol \147}}' w l lt 1 lc rgb 'blue' lw 3 ,\
4.75E3 notitle '{/Times-Roman=17 Size of the Universe}' lt 1 lc rgb 'green' lw 1 

set nokey
set border 0
set tics scale 0.8
set xtics (2E-3,3E-3,4E-3,5E-3,6E-3,7E-3,8E-3,9E-3,2E-2,3E-2,4E-2,5E-2,6E-2,7E-2,8E-2,9E-2,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,2,3,4,5,6,7,8,9,2E1,3E1,4E1,5E1,6E1,7E1,8E1,9E1,2E2,3E2,4E2,5E2,6E2,7E2,8E2,9E2,2E3,3E3,4E3,5E3,6E3,7E3,8E3,9E3,2E4,3E4,4E4,5E4,6E4,7E4,8E4,9E4,2E5,3E5,4E5,5E5,6E5,7E5,8E5,9E5,2E6,3E6,4E6,5E6,6E6,7E6,8E6,9E1,2E7,3E7,4E7,5E7,6E7,7E7,8E7,9E7,2E8,3E8,4E8,5E8,6E8,7E8,8E8,9E8,2E9,3E9,4E9,5E9,6E9,7E9,8E9,9E9,2E10,3E10,4E10,5E10,6E10,7E10,8E10,9E10,2E11,3E11,4E11,5E11,6E11,7E11,8E11,9E11,2E12,3E12,4E12,5E12,6E12,7E12,8E12,9E12,2E13,3E13,4E13,5E13,6E13,7E13,8E13,9E13,2E14,3E14,4E14,5E14,6E14,7E14,8E14,9E14,2E15,3E15,4E15,5E15,6E15,7E15,8E15,9E15)
set ytics (2,3,4,5,6,7,8,9,2E1,3E1,4E1,5E1,6E1,7E1,8E1,9E1,2E2,3E2,4E2,5E2,6E2,7E2,8E2,9E2,2E3,3E3,4E3,5E3,6E3,7E3,8E3,9E3,2E4,3E4,4E4,5E4,6E4,7E4,8E4,9E4,2E5,3E5,4E5,5E5,6E5,7E5,8E5,9E5,2E6,3E6,4E6,5E6,6E6,7E6,8E6,9E6,2E7,3E7,4E7,5E7,6E7,7E7,8E7,9E7,2E8,3E8,4E8,5E8,6E8,7E8,8E8,9E8,2E9,3E9,4E9,5E9,6E9,7E9,8E9,9E9)
set origin 0,0
set nogrid
set xlabel ''
set ylabel ''
set format x ""
set format y ""
set bmargin 4.2
set lmargin 6.2
plot \
'mean_free_path.dat' every ::1::1 u 1:2 notitle w l lt 5 lc rgb 'red' lw 3

set key at 1E8,6.2E8 samplen -1
plot \
'mean_free_path.dat' every ::1::1 u 1:2 t '{/=12 m_{~B{.5\176}}=50 GeV}' w l lt 5 lc rgb 'red' lw 1

set key at 1E8,6E7 samplen -1
plot \
'mean_free_path.dat' every ::1::1 u 1:2 t '{/=12 m_{~B{.5\176}}=100 GeV}' w l lt 5 lc rgb 'red' lw 1

set key at 1E8,1E7 samplen -1
plot \
'mean_free_path.dat' every ::1::1 u 1:2 t '{/=12 m_{~B{.5\176}}=200 GeV}' w l lt 5 lc rgb 'red' lw 1

set key at 1E14,30 samplen -1
plot \
'mean_free_path.dat' every ::1::1 u 1:2 t '{/=17 p-{/=15 CMB{/ Symbol \147}}}' w l lt 5 lc rgb 'red' lw 1

set key at 1E4,6E3 samplen -1
plot \
'mean_free_path.dat' every ::1::1 u 1:2 t '{/=15 size of the universe}' w l lt 5 lc rgb 'red' lw 1

set nomultiplot



#Set output
#set term x11
#replot
reset
