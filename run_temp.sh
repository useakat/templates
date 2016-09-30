#!/bin/bash
if [[ "$1" == "-h" ]]; then
    echo ""
    echo "Usage: run.sh [run_name]"
    echo ""
    exit
fi

if [[ $1 = "" ]]; then
    echo "input run name"
    read run
else
    run=$1
fi    

make clean >/dev/null 2>&1
start_time=`date '+%s'`
echo `date '+%T'`

run_dir=rslt_$run
if [ -e $run_dir ]; then
    rm -rf ${run_dir}/*
else
    mkdir ${run_dir}
fi

### start program ###

### end program ###

end_time=`date '+%s'`
SS=`expr ${end_time} - ${start_time}` 
HH=`expr ${SS} / 3600` 
SS=`expr ${SS} % 3600` 
MM=`expr ${SS} / 60` 
SS=`expr ${SS} % 60` 
elapsed_time="${HH}:${MM}:${SS}" 
echo "Elapsed time:" $elapsed_time
echo "" >> ${run_dir}/summary.txt
echo "total time = " $elapsed_time >> ${run_dir}/summary.txt
echo ""
echo `date '+%T'`