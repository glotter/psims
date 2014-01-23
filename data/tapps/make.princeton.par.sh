#!/bin/bash

batchno=$1
numbatches=$2

hadisd_agmerra_dir=/project/joshuaelliott/data/hadisd_agmerra
princeton_dir=/project/joshuaelliott/psims/data/clim/princeton
outdir=/project/joshuaelliott/data/princeton

agmerra_delta=15 # delta in arcminutes
princeton_delta=30

latidxs=(`ls $hadisd_agmerra_dir`)
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
  for lonidx in `ls $hadisd_agmerra_dir/$latidx`; do
    echo "Processing latidx = "$latidx", lonidx = "$lonidx
    aglat=$(echo "scale=15;90-$agmerra_delta/60*($latidx-0.5)" | bc)
    aglon=$(echo "scale=15;-180+$agmerra_delta/60*($lonidx-0.5)" | bc)
    # princeton
    princetonlatidx=$(echo "scale=15;60*(90+(-1*$aglat))/$princeton_delta+1" | bc)
    princetonlonidx=$(echo "scale=15;60*($aglon+180)/$princeton_delta+1" | bc)
    princetonlatidx=`echo $princetonlatidx | python -c "from math import floor; print int(floor(float(raw_input())))"`
    princetonlonidx=`echo $princetonlonidx | python -c "from math import floor; print int(floor(float(raw_input())))"`
    princetonlatidx=`printf "%03d" $princetonlatidx`
    princetonlonidx=`printf "%03d" $princetonlonidx`
    princetonlat1=$(echo "scale=15;90-$princeton_delta/60*($princetonlatidx-1)" | bc)
    princetonlat2=$(echo "scale=15;90-$princeton_delta/60*$princetonlatidx" | bc)
    princetonlon1=$(echo "scale=15;-180+$princeton_delta/60*($princetonlonidx-1)" | bc)
    princetonlon2=$(echo "scale=15;-180+$princeton_delta/60*$princetonlonidx" | bc)
    ch1=$(bc <<< "$aglat <= $princetonlat1") 
    ch2=$(bc <<< "$aglat >= $princetonlat2")
    ch3=$(bc <<< "$aglon >= $princetonlon1")
    ch4=$(bc <<< "$aglon <= $princetonlon2")
    ch=$(($ch1*$ch2*$ch3*$ch4))
    if [ $ch -eq 0 ]; then
      echo "Failed!"
    fi
    echo "  princetonlatidx = "$princetonlatidx", princetonlonidx = "$princetonlonidx
    if [ -d $princeton_dir/$princetonlatidx/$princetonlonidx ]; then
      if [ ! -d $outdir/$latidx ]; then
        mkdir $outdir/$latidx
      fi
      if [ ! -d $outdir/$latidx/$lonidx ]; then
        mkdir $outdir/$latidx/$lonidx
      fi
      cp $princeton_dir/$princetonlatidx/$princetonlonidx/$princetonlatidx"_"$princetonlonidx.psims.nc $outdir/$latidx/$lonidx/$princetonlatidx"_"$princetonlonidx.princeton.psims.nc
    fi
  done
done
