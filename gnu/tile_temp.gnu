###################### Options ###########################################
#set logscale x
#set logscale y
#set format x '%L'
#set format y '10^{%L}'
#set xtics (2,3,4,5,6)
#set ytics (1,10,1E2,1E3,1E4,1E5,1E6,1E7,1E8,1E9,1E10)
#set tics scale 2
#set key at 1.0E3,1.0E7 samplen 2
#set xrange [0:1]
#set yrange [1E-5:2E8]
####################### Definitions ######################################
file1 = '1.dat'
file2 = '2.dat'
file3 = '3.dat'
file4 = '4.dat'
c1 = 'red'
c2 = 'blue'
c3 = '#006400' # dark green
c4 = 'purple'
c5 = '#ff33ff'
c6 = '#cc6600' # dark orange
##########################################################################
set terminal postscript eps enhanced 'Times-Roman' color 20
set output 'figure.eps'
set grid
set key spacing 1.5 samplen 2
set multiplot layout 2,2
 
set tmargin 0
set bmargin 0
set lmargin 0
set rmargin 0
#set title '{/=28 Probability, Kr CP=0 NH}'
#set xlabel '{/=24 Enu [GeV]}'
#set ylabel '{/=24 Prob}' offset 1.5,0
unset xtics
unset ytics
#################### plot ##########################################
set label 'm > m' at graph 0.4,0.8
plot \
file1 u ($1-0.005):2 title 'Okamura' w l lt 1 lw 3 lc rgb c1, \
file2 u 1:2 title 'Takaesu' w l lt 1 lw 3 lc rgb c2
unset label
set label 'm > e' at graph 0.4,0.8
plot \
file1 u ($1-0.005):3 notitle w l lt 1 lw 3 lc rgb c1, \
file3 u 1:2 notitle w l lt 1 lw 3 lc rgb c2
unset label
set label 'e > m' at graph 0.4,0.8
plot \
file1 u ($1-0.005):4 notitle w l lt 1 lw 3 lc rgb c1, \
file4 u 1:2 notitle w l lt 1 lw 3 lc rgb c2
unset label
set label 'e > e' at graph 0.4,0.8
plot \
file1 u ($1-0.005):5 notitle w l lt 1 lw 3 lc rgb c1, \
file5 u 1:2 notitle w l lt 1 lw 3 lc rgb c2
###########################################################################
set nomultiplot
reset

#file2 u 1:2 title 'Takaesu' w l lt 1 lw 3 lc rgb c2
#file2 u 1:2 title 'Takaesu' w points lc rgb c2