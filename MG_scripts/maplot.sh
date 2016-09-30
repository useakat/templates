#!/bin/bash
if [[ "$1" == "-h" ]]; then
    echo ""
    echo "Usage: maplot.sh [run_name]"
    echo ""
    exit
fi

t=$1
main_dir=`pwd`
dirbin=${main_dir}/bin_ma
MADIR=$PACKAGES/MadAnalysis_v1.1.4
TD=$PACKAGES/td_linux


cd Events
gunzip -c ${t}/unweighted_events.lhe.gz > unweighted_events.lhe

if [[ (-x $MADIR/plot_events) && (-e unweighted_events.lhe) && (-e ../Cards/plot_card.dat) ]]; then
    echo "Creating Plots" 
    mkdir $t
    cd $t
    echo "../unweighted_events.lhe" > events.list
    $dirbin/plot $MADIR $TD
    cd ..
    $dirbin/plot_page-pl $t parton
    mv -f plots.html ${t}_plots.html
fi

rm unweighted_events.lhe
