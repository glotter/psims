#!/bin/bash

shopt -s extglob
for dir in run*
do
   cd $dir
   rm -rf !(*.nc4|output|paramfile|parts)
   cd ..
done