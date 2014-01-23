#!/bin/bash

batchno=$1
numbatches=$2

hadisd_agmerra_dir=/project/joshuaelliott/data/hadisd_agmerra
cfsr_dir=/project/joshuaelliott/psims/data/clim/cfsr
grasp_dir=/project/joshuaelliott/psims/data/clim/grasp
outdir=/project/joshuaelliott/data/cfsr_grasp

agmerra_delta=15 # delta in arcminutes
cfsr_delta=30
grasp_delta=30

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
    # cfsr
    cfsrlatidx=$(echo "scale=15;60*(90+(-1*$aglat))/$cfsr_delta+1" | bc)
    cfsrlonidx=$(echo "scale=15;60*($aglon+180)/$cfsr_delta+1" | bc)
    cfsrlatidx=`echo $cfsrlatidx | python -c "from math import floor; print int(floor(float(raw_input())))"`
    cfsrlonidx=`echo $cfsrlonidx | python -c "from math import floor; print int(floor(float(raw_input())))"`
    cfsrlatidx=`printf "%03d" $cfsrlatidx`
    cfsrlonidx=`printf "%03d" $cfsrlonidx`
    cfsrlat1=$(echo "scale=15;90-$cfsr_delta/60*($cfsrlatidx-1)" | bc)
    cfsrlat2=$(echo "scale=15;90-$cfsr_delta/60*$cfsrlatidx" | bc)
    cfsrlon1=$(echo "scale=15;-180+$cfsr_delta/60*($cfsrlonidx-1)" | bc)
    cfsrlon2=$(echo "scale=15;-180+$cfsr_delta/60*$cfsrlonidx" | bc)
    ch1=$(bc <<< "$aglat <= $cfsrlat1") 
    ch2=$(bc <<< "$aglat >= $cfsrlat2")
    ch3=$(bc <<< "$aglon >= $cfsrlon1")
    ch4=$(bc <<< "$aglon <= $cfsrlon2")
    ch=$(($ch1*$ch2*$ch3*$ch4))
    if [ $ch -eq 0 ]; then
      echo "Failed on CFSR!"
    fi
    # grasp
    grasplatidx=$(echo "scale=15;60*(90+(-1*$aglat))/$grasp_delta+1" | bc)
    grasplonidx=$(echo "scale=15;60*($aglon+180)/$grasp_delta+1" | bc)
    grasplatidx=`echo $grasplatidx | python -c "from math import floor; print int(floor(float(raw_input())))"`
    grasplonidx=`echo $grasplonidx | python -c "from math import floor; print int(floor(float(raw_input())))"`
    grasplatidx=`printf "%03d" $grasplatidx`
    grasplonidx=`printf "%03d" $grasplonidx`
    grasplat1=$(echo "scale=15;90-$grasp_delta/60*($grasplatidx-1)" | bc)
    grasplat2=$(echo "scale=15;90-$grasp_delta/60*$grasplatidx" | bc)
    grasplon1=$(echo "scale=15;-180+$grasp_delta/60*($grasplonidx-1)" | bc)
    grasplon2=$(echo "scale=15;-180+$grasp_delta/60*$grasplonidx" | bc)
    ch1=$(bc <<< "$aglat <= $grasplat1")
    ch2=$(bc <<< "$aglat >= $grasplat2")
    ch3=$(bc <<< "$aglon >= $grasplon1")
    ch4=$(bc <<< "$aglon <= $grasplon2")
    ch=$(($ch1*$ch2*$ch3*$ch4))
    if [ $ch -eq 0 ]; then
      echo "Failed on GRASP!"
    fi
    echo "  cfsrlatidx = "$cfsrlatidx", cfsrlonidx = "$cfsrlonidx
    echo "  grasplatidx = "$grasplatidx", grasplonidx = "$grasplonidx
    if [ -d $cfsr_dir/$cfsrlatidx/$cfsrlonidx ]; then
      if [ ! -d $outdir/$latidx ]; then
        mkdir $outdir/$latidx
      fi
      if [ ! -d $outdir/$latidx/$lonidx ]; then
        mkdir $outdir/$latidx/$lonidx
      fi
      cp $cfsr_dir/$cfsrlatidx/$cfsrlonidx/$cfsrlatidx"_"$cfsrlonidx.psims.nc $outdir/$latidx/$lonidx/$cfsrlatidx"_"$cfsrlonidx.cfsr.psims.nc
    fi
    if [ -d $grasp_dir/$grasplatidx/$grasplonidx ]; then
      if [ ! -d $outdir/$latidx ]; then
        mkdir $outdir/$latidx
      fi
      if [ ! -d $outdir/$latidx/$lonidx ]; then
        mkdir $outdir/$latidx/$lonidx
      fi
      cp $grasp_dir/$grasplatidx/$grasplonidx/$grasplatidx"_"$grasplonidx.psims.nc $outdir/$latidx/$lonidx/$grasplatidx"_"$grasplonidx.grasp.psims.nc
    fi    
  done
done
