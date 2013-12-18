#!/bin/bash


echo "Cleaning up results dir ... OK? (y/n)"

read line

if [ $line = 'y' -o $line = 'Y' ]
then
   rm -rfv quadoutdir
fi

rm -rf *.log 
