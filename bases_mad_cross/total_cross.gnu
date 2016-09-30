set terminal postscript eps enhanced "Times-Italic" color 17
set output "total_cross.ps"
set logscale x
set logscale y
set format x "10^{%L}"
set format y "10^{%L}"
#set xtics (0.0010.01,0.1,1,10,1.0E2,1.0E3,1.0E4,1.0E5,1.0E6,1.0E7,1.0E8,1.0E9,1.0E10,1.0E11,1E12,1E13,1E14,1E15,1E16)
#set ytics (1,10,1E2,1E3,1E4,1E5,1E6,1E7,1E8,1E9,1E10)
set tics scale 2
set grid
#set key 1.0E-1,0.7E10 samplen 2
set key spacing 1.0
#set key 7.0E10,1.0E-3
#set title 'm_{~q{.5\176}}=51 GeV, m_{~B{.6\176}}=50 GeV'
#set xlabel 'sqrts (GeV)' -1,0
set ylabel 'Cross Section (ub)'
set xrange [1E0:1.0E2]
set yrange [1E2:1.0E11]

#set multiplot
plot \
'total_cross_cm.dat' u 1:($2*1E-3) notitle  w l lt 1 lc rgb 'red' lw 1

#set nokey
#set noborder 
#set tics scale 0.8
#set origin 0,0
#set nogrid
#set xlabel ''
#set ylabel ''
#set format x ""
#set format y ""
#set bmargin 4
#set lmargin 7.5
#plot \
#'total_cross_cm.dat' every ::1::1 u 1:2 notitle w l lt 5 lc rgb 'red' lw 3

#set nomultiplot

#set output
#set term x11
#replot
reset
