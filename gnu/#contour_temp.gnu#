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
####################### Definitions ######################################
file1 = 'ae_xsec.dat'
c1 = 'red'			
c2 = 'blue'
c3 = '#006400' # dark green
c4 = 'purple'
c5 = '#ff33ff'
c6 = '#cc6600' # dark orange
c7 = 'black'

#set style line 1 lt 2 lc rgb c7 lw 3
##########################################################################
set terminal postscript eps enhanced "Times-Roman" color 20
set output "xsec_ratio.eps"
set grid
set key spacing 1.5 samplen 2
#set lmargin 5.5
#set rmargin 1.8
#set bmargin 3.5
#set tmargin 0.5
#set origin -0.032,0.035
#set size 1.191, 1.154

#set multiplot layout 2,1
#set multiplot

set style increment user
set contour
unset surface
set view map  # for without multiplot
#set pm3d map # for multiplot with 'plot'
set contour base
set cntrparam order 3
set cntrparam bspline
set cntrparam levels discrete 1e-2,1e-3,1e-4,1e-5,1e-6,1e-7,1e-8
#set cntrparam levels 10

set title "{/=28 Xsec Ratio to Total CC Xsec (anti-nu_e)}"
set xlabel '{/=24 E_{nu}'
set ylabel '{/=24 Ratio}' offset 1.5,0
#################### plot ##########################################
splot file11 u 1:2:3 notitle w l
####################################################################

#set nomultiplot
reset

#{/Symbol ~n{.4-}}