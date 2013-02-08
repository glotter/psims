#!/bin/bash

dir=$1

if [ -z "$dir" ]; then
   echo "Usage: $0 dir"
   exit 1
fi

file=$( mktemp )
cd $dir
find */* -type d > $file
echo Temporary gridList file: $file
