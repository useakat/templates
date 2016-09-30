#!/bin/bash
if [[ "$1" == "-h" ]]; then
    echo ""
    echo "Usage: "
    echo ""
    exit
fi
maindir=`cat maindir.txt`
selfdir=$(cd $(dirname $0);pwd)
if [[ $1 = "h" ]]; then
    echo "run name"
    read run
else
    if [ $# -ge 1 ];then
	run=$1
    fi
fi

    