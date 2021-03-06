#!/bin/bash
selfdir=$(cd $(dirname $0);pwd)
if [[ "$1" == "-h" ]]; then
    echo ""
    echo "Usage: submit_job.sh [job_system] [que] [it] [jobname] [command]"
    echo ""
    exit
fi

job_system=$1
que=$2
i=$3
jobname=$4
command="$5"

dir=par_$i
mkdir $dir
cd $dir
#mkdir files
#mv * files/. 2>/dev/null

echo "#!/bin/bash" > ajob$i
echo "date" >> ajob$i
#echo "cp -rf $selfdir/$dir/files/* ." >> ajob$i
echo "rm -rf $selfdir/$dir/wait.ajob$i" >> ajob$i
echo "touch $selfdir/$dir/run.ajob$i" >> ajob$i
echo "$command" >> ajob$i
echo "rm -rf $selfdir/$dir/run.ajob$i" >> ajob$i
echo "touch $selfdir/$dir/done.ajob$i" >> ajob$i
chmod +x ajob$i
touch wait.ajob$i

if [ $job_system == "bsub" ]; then
    bsub -q $que -J $jobname ./ajob$i 1>/dev/null
elif [ $job_system == "qsub" ]; then
    qsub -N $jobname ./ajob$i 1>/dev/null    
elif [ $job_system == "condor" ]; then
    echo "# here goes your shell script" > condor.cmd
    echo "executable     = ajob$i" >> condor.cmd
    echo " " >> condor.cmd
    echo "# here you specify where to put .log, .out and .err files" >> condor.cmd
    echo "output         = condor.out" >> condor.cmd
    echo "error          = condor.err" >> condor.cmd
    echo "log            = condor.log" >> condor.cmd
    echo " " >> condor.cmd
    echo "# the following two parameters enable the file transfer mechanism" >> condor.cmd
    echo "# and specify that the output files should be transferred back" >> condor.cmd
    echo "# to the submit machine from the remote machine where the job executes" >> condor.cmd
    echo "should_transfer_files   = YES" >> condor.cmd
    echo "when_to_transfer_output = ON_EXIT" >> condor.cmd
    echo " " >> condor.cmd
    echo "# the following two parameters are required for the ingrid cluster" >> condor.cmd
    echo "universe       = vanilla" >> condor.cmd
    echo "requirements   = (CMSFARM =?= TRUE)" >> condor.cmd
    echo "#for Madgraph users replace the previous line by:" >> condor.cmd
    echo "#requirements   = (MADGRAPH =?= TRUE)" >> condor.cmd
    echo " " >> condor.cmd
    echo "queue 1" >> condor.cmd 
    condor_submit condor.cmd 1>/dev/null
fi

cd ..
