#!/bin/bash

batchno=$1
numbatches=$2

dir=/project/joshuaelliott/psims/data/clim/wfdei
metafile=/project/joshuaelliott/data/WDLSTCOM.dat
csvfile=/project/joshuaelliott/data/NASA.CO2.Annual.1850-2013.csv
outdir=/project/joshuaelliott/data/wfdei.gpcc.epic.ggcmi
app=/project/joshuaelliott/psims/data/tapps/psims2dly.py

files=(`ls $dir`)
numlats=${#files[@]}

bz=$(echo "($numlats + $numbatches - 1)/$numbatches" | bc) # batch size (use ceiling)
si=$(echo "$bz*($batchno-1)" | bc)
if [ $batchno -eq $numbatches ]; then
  ei=$numlats
else
  ei=$(($si+$bz))
  # check for end index out of bounds
  if [ $ei -gt $numlats ]; then
    ei=$numlats
  fi
fi
if [ $si -ge $numlats ]; then
  echo "No jobs for processor to perform. Exiting . . ."
  exit 0
fi

for ((fi = $si; fi < $ei; fi++)); do
  i=${files[$fi]}
  for j in `ls $dir/$i`; do
    file=$dir/$i/$j/$i"_"$j.psims.nc
    lat=$(echo `ncks -v latitude $file` | sed -s 's/.*latitude\[0\]=\(.*\) degrees_north/\1/')
    lon=$(echo `ncks -v longitude $file` | sed -s 's/.*longitude\[0\]=\(.*\) degrees_east/\1/')
    OLD_IFS=$IFS
    IFS=","
    src=$(echo "`printf "%9.2f" $lon``printf "%9.2f" $lat`")
    s=`cat $metafile | grep "$src"`
    IFS=$OLD_IFS
    s=($s) # break along spaces
    if [ "${s[1]}" ]; then
      echo "Processing lat = "$lat", lon = "$lon
      outfile=${s[1]}
      `$app -i $file -v rsds,tasmax,tasmin,pr_gpcc,hur,wind -o $outdir/$outfile -c $csvfile`
    else
      echo "Skipping . . ."
    fi
  done
done
