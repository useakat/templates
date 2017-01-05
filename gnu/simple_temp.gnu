###################### Syntax ###########################################
#{/Symbol ~n{.4-}}
##########################################################################
fix_bbox = 0  # 0:Do not fix Bounding Box of an eps file  1:Fix Bounding Box
eps_output = 0  # 0:no eps output 1:eps output
pdf_output = 1  # 0:no pdf output 1:pdf output
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
deg = '{/Symbol \260}'
##########################################################################
set terminal postscript eps enhanced "Times-Roman" color 20
set grid
#set key at 1.0E3,1.0E7 samplen 2
set key spacing 1.5 samplen 2
#set lmargin 5.5
#set rmargin 1.8
#set bmargin 3.5
#set tmargin 0.5
#set origin -0.032,0.035
#set size 1.191, 1.154
#set size square
#set title "gg -> ng Cross Section"
#set logscale x
#set logscale y
#set format x "%L"
#set format y "10^{%L}"
#set xtics (2,3,4,5,6)
#set ytics (1,10,1E2,1E3,1E4,1E5,1E6,1E7,1E8,1E9,1E10)
#set tics scale 2
#set xlabel 'Final gluons' offset 0,0
#set ylabel 'Cross Section (fb)' offset 0,0
#set xrange [1:7]
#set yrange [1E-5:2E8]

output_file = "xsec_ratio.eps"
set output output_file
#set multiplot layout 2,1
set multiplot

set title "{/=28 Xsec Ratio to Total CC Xsec (anti-nu_e)}"
set xlabel '{/=24 E_{nu}'
set ylabel '{/=24 Ratio}' offset 1.5,0
#################### plot ##############################################
plot \
file1 u 1:($2/$6) title "CCQE H" w l lt 1 lw 3 lc rgb c1 ,\
file1 u 1:($3/$6) title "CCQE O" w l lt 1 lw 3 lc rgb c2 
#########################################################################
set nomultiplot
reset
################## fix BoundingBox ################################
if (fix_bbox == 1) \
   command = sprintf("epstool --copy --bbox %s tmp.eps", output_file); system command \
   ; command = sprintf("mv tmp.eps %s", output_file); system command \
   ; command = "rm -rf tmp.eps"; system command
if (pdf_output == 1) \
   command = sprintf("ps2pdf -dEPSCrop %s", output_file); system command
if (eps_output == 0) \
   command = "rm -rf *.eps"; system command