#!/bin/bash

for file in $( find parts -type f ); do
   lat=$( basename $( dirname $file ) )
   lon=$( basename $file .psims.nc )
   echo [$lat][$lon] $file
done
