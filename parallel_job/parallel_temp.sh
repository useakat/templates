#!/bin/bash

run=$1
mail=$2

output=mdm_cB.dat

min=50
max=350
ndiv=30
logflag=0

#job_system=condor
#que=madgraph
job_system=bsub
que=s
######################################################
start=`date`
echo $start

x=$min
xx=1
i=1
while [ $xx -eq 1 ];do
    job=cBscan$i
    ./submit_job.sh $job_system $que $i $job "make; ./param_scan.py" $x
    if [ $logflag -eq 1 ];then
	x=`echo "scale=5; e( (l($min)/l(10) +(l($max)/l(10) -l($min)/l(10))/$ndiv*($i-1))*l(10) )" | bc -l`
    else
	x=`echo "scale=5; $min +($max -$min)/$ndiv*$i" | bc -l`
    fi
    xx=`echo "scale=5; if( $x < $max ) 1 else 0" | bc`
    i=`expr $i + 1`
done
n=$i

./monitor

rsltdir=rslt_$run
if [ -e $rsltdir ];then
    a=3
else
    mkdir $rsltdir
fi
rm -rf $rsltdir/$output
touch $rsltdir/$output

i=1
while [ $i -lt $n ];do
    cat par_${i}/$output >> $rsltdir/$output
    echo "" >> $rsltdir/$output
    i=`expr $i + 1`
done

cp -rf par_1/Cards/param_card.dat $rsltdir/.
cp -rf param_scan.py $rsltdir/.
cp -rf cB_scan.sh $rsltdir/.

echo "finished!"
echo $start
echo `date`

rm -rf par_*

if [ $mail -eq 1 ];then
    bsub -q e -J cbscan -u takaesu@post.kek.jp nulljob.sh >/dev/null 2>&1
fi