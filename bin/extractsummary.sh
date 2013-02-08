#!/bin/bash

#usage: extractsummary.sh <outputdir>

#Extracts the Summary.OUT from the results dir of DSSAT runs

mkdir summaries

for i in $( find $1 -name "*.tar.gz" )
do
    bname=`basename $i output.tar.gz`
    echo $bname
    tar --strip-components=1 -xzf $i output/Summary.OUT
    mv Summary.OUT summaries/${bname}Summary.OUT
done
