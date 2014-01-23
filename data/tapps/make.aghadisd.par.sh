#!/bin/bash

batchno=$1
numbatches=$2

agmerra_dir=/project/joshuaelliott/psims/data/clim/agmerra.15min
hadisd_dir=/project/joshuaelliott/psims/data/clim/hadisd/daily
outdir=/project/joshuaelliott/data/hadisd_agmerra

agmerra_delta=15 # delta in arcminutes
hadisd_delta=0.5

latidxs=(`ls $agmerra_dir`)
numlats=${#latidxs[@]}

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

for ((i = $si; i < $ei; i++)); do
  latidx=${latidxs[$i]}
  for lonidx in `ls $agmerra_dir/$latidx`; do
    echo "Processing latidx = "$latidx", lonidx = "$lonidx
    # find agmerra box
    lat1=$(echo "scale=15;90-$agmerra_delta/60*($latidx-1)" | bc)
    lat2=$(echo "scale=15;90-$agmerra_delta/60*$latidx" | bc)
    lon1=$(echo "scale=15;-180+$agmerra_delta/60*($lonidx-1)" | bc)
    lon2=$(echo "scale=15;-180+$agmerra_delta/60*$lonidx" | bc)
    # find indices in hadisd
    hlatidx1=$(echo "scale=15;60*(90+(-1*$lat1))/$hadisd_delta+0.5" | bc)
    hlatidx2=$(echo "scale=15;60*(90+(-1*$lat2))/$hadisd_delta+0.5" | bc)
    hlonidx1=$(echo "scale=15;60*($lon1+180)/$hadisd_delta+0.5" | bc)
    hlonidx2=$(echo "scale=15;60*($lon2+180)/$hadisd_delta+0.5" | bc)
    # get floor and ceiling
    hlatidx1=`echo $hlatidx1 | python -c "from math import ceil; print int(ceil(float(raw_input())))"`
    hlatidx2=`echo $hlatidx2 | python -c "from math import floor; print int(floor(float(raw_input())))"`
    hlonidx1=`echo $hlonidx1 | python -c "from math import ceil; print int(ceil(float(raw_input())))"`
    hlonidx2=`echo $hlonidx2 | python -c "from math import floor; print int(floor(float(raw_input())))"`
    echo "  hlatidx = ["$hlatidx1", "$hlatidx2"]; hlonidx = ["$hlonidx1", "$hlonidx2"]"
    # iterate over all hadisd points
    found_pt=False
    for ((j = $hlatidx1; j <= $hlatidx2; j++)); do
      for ((k = $hlonidx1; k <= $hlonidx2; k++)); do
        if [ -d $hadisd_dir/$j/$k ]; then
          # found hadisd point
          echo "  Found hadisd point! at latidx = "$j", lonidx = "$k
          if [ ! -d $outdir/$latidx ]; then
            # make lat directory
            mkdir $outdir/$latidx
          fi
          if [ ! -d $outdir/$latidx/$lonidx ]; then
            # make lon directory
            mkdir $outdir/$latidx/$lonidx
          fi
          cp $hadisd_dir/$j/$k/$j"_"$k.psims.nc $outdir/$latidx/$lonidx/$j"_"$k.hadisd.psims.nc
          found_pt=True
        fi
      done
    done
    if [ $found_pt == True ]; then
      # copy agmerra data
      f=$latidx/$lonidx/$latidx"_"$lonidx
      cp $agmerra_dir/$f.psims.nc $outdir/$f.agmerra.psims.nc
    fi
  done
done
