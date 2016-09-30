###################### Options ###########################################
#set title "gg -> ng Cross Section"
#set logscale x
#set logscale y
#set format x "%L"
#set format y "10^{%L}"
#set xtics (2,3,4,5,6)
#set ytics (1,10,1E2,1E3,1E4,1E5,1E6,1E7,1E8,1E9,1E10)
#set tics scale 2
#set key at 1.0E3,1.0E7 samplen 2
#set xlabel 'Final gluons' offset 0,0
#set ylabel 'Cross Section (fb)' offset 0,0
#set xrange [1:7]
#set yrange [1E-5:2E8]
##########################################################################

set terminal postscript eps enhanced "Times-Roman" color 20
set output "xsec.eps"
set grid
set key spacing 1.5 samplen 1

set multiplot

#################### Upper plot ##########################################
set size 1, 0.9
set title "gg -> ng Cross Section"
set format x ""
set logscale y
set format y "10^{%L}"
set yrange [1.1E8:1E12]
set ylabel 'Cross Section (fb)' offset 0,0
set tmargin 0
set bmargin 9.5
plot \
'xsec.dat' u 1:2 title "LCS" w points pt 4 lc rgb 'red' lw 3 ,\
'xsec.dat' u 1:4 title "LCS 1ch" w points pt 3 lc rgb 'blue' lw 3 ,\
'xsec.dat' u 1:8 title "MG5" w points pt 2 lc rgb 'green' lw 3 ,\
'xsec.dat' u 1:6 title "Sherpa" w points pt 1 lc rgb 'purple' lw 3
################### Lower Plot ########################################### 
set size 1, 0.35
set notitle
set format x "%.0f"
set nologscale y
set format y "%.1f"
set xtics (2,3,4,5,6)
set ytics ("0.9" 0.9,"" 0.95,"1.0" 1,"" 1.05,"1.1" 1.1)
set xlabel 'Final gluons' offset 0,0
set ylabel 'Ratio' offset 0,0
set yrange [0.9:1.1]
set nokey
set bmargin 3.5
plot \
'xsec.dat' u 1:($2/$4) title "LCS" w points pt 4 lc rgb 'red' lw 3 ,\
'xsec.dat' u 1:($4/$4) title "LCS 1ch" w points pt 3 lc rgb 'blue' lw 3 ,\
'xsec.dat' u 1:($8/$4) title "MG5" w points pt 2 lc rgb 'green' lw 3 ,\
'xsec.dat' u 1:($6/$4) title "Sherpa" w points pt 1 lc rgb 'purple' lw 3
###########################################################################
set nomultiplot

reset
