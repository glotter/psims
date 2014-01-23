#!/bin/bash

batchno=$1
numbatches=$2

agmerra_dir=/project/joshuaelliott/psims/data/clim/agmerra.15min
hadisd_dir=/project/joshuaelliott/psims/data/clim/hadisd/daily
outdir=/project/joshuaelliott/data/hadisd_agmerra

agmerra_delta=15 # delta in arcminutes
hadisd_delta=0.5

latidxs=(`ls $hadisd_dir`)
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
  for lonidx in `ls $hadisd_dir/$latidx`; do
    echo "Processing latidx = "$latidx", lonidx = "$lonidx
    hlat=$(echo "scale=15;90-$hadisd_delta/60*($latidx-0.5)" | bc)
    hlon=$(echo "scale=15;-180+$hadisd_delta/60*($lonidx-0.5)" | bc)
    aglatidx=$(echo "scale=15;60*(90+(-1*$hlat))/$agmerra_delta+1" | bc)
    aglonidx=$(echo "scale=15;60*($hlon+180)/$agmerra_delta+1" | bc)
    aglatidx=`echo $aglatidx | python -c "from math import floor; print int(floor(float(raw_input())))"`
    aglonidx=`echo $aglonidx | python -c "from math import floor; print int(floor(float(raw_input())))"`
    aglatidx=`printf "%03d" $aglatidx`
    aglonidx=`printf "%03d" $aglonidx`
    aglat1=$(echo "scale=15;90-$agmerra_delta/60*($aglatidx-1)" | bc)
    aglat2=$(echo "scale=15;90-$agmerra_delta/60*$aglatidx" | bc)
    aglon1=$(echo "scale=15;-180+$agmerra_delta/60*($aglonidx-1)" | bc)
    aglon2=$(echo "scale=15;-180+$agmerra_delta/60*$aglonidx" | bc)
    ch1=$(bc <<< "$hlat <= $aglat1") 
    ch2=$(bc <<< "$hlat >= $aglat2")
    ch3=$(bc <<< "$hlon >= $aglon1")
    ch4=$(bc <<< "$hlon <= $aglon2")
    ch=$(($ch1*$ch2*$ch3*$ch4))
    if [ $ch -eq 0 ]; then
      echo "Failed!"
    fi
    echo "  aglatidx = "$aglatidx", aglonidx = "$aglonidx
    if [ -d $agmerra_dir/$aglatidx/$aglonidx ]; then
      f1=$latidx/$lonidx/$latidx"_"$lonidx
      f2=$aglatidx/$aglonidx/$aglatidx"_"$aglonidx
      if [ ! -d $outdir/$aglatidx ]; then
        mkdir $outdir/$aglatidx
      fi
      if [ ! -d $outdir/$aglatidx/$aglonidx ]; then
        mkdir $outdir/$aglatidx/$aglonidx
      fi
      cp $hadisd_dir/$f1.psims.nc $outdir/$aglatidx/$aglonidx/$latidx"_"$lonidx.hadisd.psims.nc
      if [ ! -f $outdir/$f2.agmerra.psims.nc ]; then
        cp $agmerra_dir/$f2.psims.nc $outdir/$f2.agmerra.psims.nc
      fi
    fi
  done
done
