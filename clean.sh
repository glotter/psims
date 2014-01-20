#!/bin/bash

# use as 
# ./clean.sh '19|20|21'
# etc. with quotes around the list of rundirs to exclude

exclude=$1
num=$$
trash=/project/joshuaelliott/trash


for i in `ls | grep run | grep -Ev $exclude`; do
  echo $i
  parts=`ls $i/parts | grep -c ""`
  echo $parts

  mkdir $trash/$num
  mv $i/swiftwork $trash/$num/
  mv $i/var_files $trash/$num/
  mv $i ./outputs/
  num=$((num+123214))
done
cd outputs
./clean.sh
