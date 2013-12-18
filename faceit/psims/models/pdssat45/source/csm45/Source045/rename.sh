#!/bin/bash

for i in `find ./*.FOR`; do
 file=`basename $i .FOR`
 echo $file
 mv $file".FOR" ./$file".f90"
done

for j in `find ./*.for`; do
 file=`basename $j .for`
 echo $file
 mv $file".for" ./$file".f90"
done

