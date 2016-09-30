###################### Options ###########################################
#set logscale x
set logscale y
#set format x '%L'
set format y '10^{%L}'
#set xtics (2,3,4,5,6)
#set ytics (1,10,1E2,1E3,1E4,1E5,1E6,1E7,1E8,1E9,1E10)
#set tics scale 2
#set key at 1.0E3,1.0E7 samplen 2
#set xrange [1:7]
#set yrange [1E-5:2E8]
####################### Definitions ######################################
file1 = 'beam/oab25n.dat'
file2 = 'beam/oab25n.dat'
file3 = 'beam/oab25n.dat'
file4 = 'beam/oab25n.dat'
c1 = 'red'
c2 = 'blue'
c3 = '#006400' # dark green
c4 = 'purple'
c5 = '#ff33ff'
c6 = '#cc6600' # dark orange
##########################################################################
set terminal postscript eps enhanced 'Times-Roman' color 16
set output 'beam_oab2.5_nm_100km.eps'
set grid
set key spacing 1.3 samplen 2 at graph 0.99,0.99
set multiplot layout 2,2

set xrange [0:3] 

set yrange [10:4E5] 
set size 0.52, 0.5
set lmargin 7
set rmargin 1.6
set label '2.5{/Symbol \260} OAB' at graph 0.4,0.9
set title '{/=20 {/Symbol n}_{/Symbol m} Focusing Beam}'
set ylabel '{/=18 Flux/cm^2/10^{21}POT}'
set xlabel ''
plot \
file1 u 1:2 title '{/Symbol n}_{/Symbol m}' w steps lt 1 lw 3 lc rgb c1, \
file1 u 1:3 title '~{/Symbol n}{.4-}_{/Symbol m}' w steps lt 2 lw 3 lc rgb c1, \
file1 u 1:4 title '{/Symbol n}_e' w steps lt 1 lw 3 lc rgb c2, \
file1 u 1:5 title '~{/Symbol n}{.4-}_e' w steps lt 2 lw 3 lc rgb c2
unset label

set size 0.48, 0.5
set lmargin 4.6
set rmargin 1
set label '2.5{/Symbol \260} OAB' at graph 0.4,0.9
set title '{/=20 ~{/Symbol n}{.4-}_{/Symbol m} Focusing Beam}'
set ylabel ''
set xlabel ''
plot \
file2 u 1:2 title '{/Symbol n}_{/Symbol m}' w steps lt 1 lw 3 lc rgb c1, \
file2 u 1:3 title '~{/Symbol n}{.4-}_{/Symbol m}' w steps lt 2 lw 3 lc rgb c1, \
file2 u 1:4 title '{/Symbol n}_e' w steps lt 1 lw 3 lc rgb c2, \
file2 u 1:5 title '~{/Symbol n}{.4-}_e' w steps lt 2 lw 3 lc rgb c2
unset label

set size 0.52, 0.5
set lmargin 7
set rmargin 1.6    
set bmargin 3.5
set label '0.5{/Symbol \260} OAB' at graph 0.4,0.9
unset title
set ylabel '{/=18 Flux/cm^2/10^{21}POT}'
set xlabel '{/=18 E_{/Symbol n} [GeV]}' offset 1.5,0
plot \
file3 u 1:2 title '{/Symbol n}_{/Symbol m}' w steps lt 1 lw 3 lc rgb c1, \
file3 u 1:3 title '~{/Symbol n}{.4-}_{/Symbol m}' w steps lt 2 lw 3 lc rgb c1, \
file3 u 1:4 title '{/Symbol n}_e' w steps lt 1 lw 3 lc rgb c2, \
file3 u 1:5 title '~{/Symbol n}{.4-}_e' w steps lt 2 lw 3 lc rgb c2
unset label

set size 0.48, 0.5
set lmargin 4.6
set rmargin 1
set label '0.5{/Symbol \260} OAB' at graph 0.4,0.9
unset title
set ylabel ''
set xlabel '{/=18 E_{/Symbol n} [GeV]}' offset 1.5,0
plot \
file4 u 1:2 title '{/Symbol n}_{/Symbol m}' w steps lt 1 lw 3 lc rgb c1, \
file4 u 1:3 title '~{/Symbol n}{.4-}_{/Symbol m}' w steps lt 2 lw 3 lc rgb c1, \
file4 u 1:4 title '{/Symbol n}_e' w steps lt 1 lw 3 lc rgb c2, \
file4 u 1:5 title '~{/Symbol n}{.4-}_e' w steps lt 2 lw 3 lc rgb c2
unset label

set nomultiplot
reset
