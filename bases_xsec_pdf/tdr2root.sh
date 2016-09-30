#!/bin/sh
fn=$1
root -l -b -q ../tdr2root.C <<EOF 
${fn}
EOF
