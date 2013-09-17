#!/bin/bash

# Useage: ./bin/create_gridList_multiple.sh [/path/to/wth] [/path/to/scen] [wthflag] [scenflag] [outputfilename]

dir1=$1
dir2=$2
inp1=$3
inp2=$4
out=$5

if [ -z "$dir1" ] || [ -z "$dir2" ] ; then
   echo "Usage: $0 dir"
   exit 1
fi

file1=$( mktemp )
file2=$( mktemp )
file3=$( mktemp )

cd $dir1
for i in `find -L -name *$inp1*` ; do 
 dir=`dirname $i` 
 echo ${dir:2} >> $file1
done

cd $dir2
for i in `find -L -name *$inp2*` ; do 
 dir=`dirname $i` 
 echo ${dir:2} >> $file2
done

grep -f $file1 $file2 > $file3
rm $file1
rm $file2

chmod a+r $file3
echo Temporary gridList file: $file3
mv $file3 /project/joshuaelliott/psims/gridLists/$out