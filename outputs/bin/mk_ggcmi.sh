#!/bin/bash

function cropfullname {
  local crop=$1
  if [[ $crop == "mai" ]]; then
    cropfull="maize" 
  elif [[ $crop == "whe" ]]; then
    cropfull="wheat"
  elif [[ $crop == "soy" ]]; then
    cropfull="soy"
  elif [[ $crop == "ric" ]]; then
    cropfull="rice"
  elif [[ $crop == "sor" ]]; then
    cropfull="sorghum"
  elif [[ $crop == "mil" ]]; then
    cropfull="millet"
  elif [[ $crop == "mgr" ]]; then
    cropfull="managed_grass"
  else
    cropfull=""
  fi
  echo $cropfull
}

function climcapitalized {
  local clim=$1
  if [[ $clim == "agcfsr" ]]; then
    climcap="AgCFSR 1980 2010"
  elif [[ $clim == "agmerra" ]]; then
    climcap="AgMERRA 1980 2010"
  elif [[ $clim == "cfsr" ]]; then
    climcap="CFSR 1980 2010"
  elif [[ $clim == "erai" ]]; then
    climcap="ERAI 1979 2010"
  elif [[ $clim == "grasp" ]]; then
    climcap="GRASP 1961 2010"
  elif [[ $clim == "princeton" ]]; then
    climcap="Princeton 1948 2008"
  elif [[ $clim == "watch" ]]; then
    climcap="WATCH 1958 2001"
  elif [[ $clim == "wfdei_cru" ]]; then
    climcap="WFDEI.CRU 1979 2012"
  elif [[ $clim == "wfdei_gpcc" ]]; then
    climcap="WFDEI.GPCC 1979 2009"
  else
    climcap=""
  fi
  echo $climcap
}

# read from command line
ggcmidir=$1
outdir=$2
overwrite=$3

# translator
tapp=/project/joshuaelliott/psims/outputs/bin/out2GGCMI.py

# output directories
pdssatpmdir=$outdir/pDSSAT.pm
pdssatptdir=$outdir/pDSSAT.pt
papsimdir=$outdir/pAPSIM

# variable names
pdssatvars=yield,pirrww,biom,aet,plant-day,anth-day,maty-day,gsprcp,gsrsds,sumt
numdssatvars=10
papsimvars=yield,pirrww,biom,aet,plant-day,anth-day,maty-day,initr,leach,sco2,sn2o,gsprcp,gsrsds,sumt
numapsimvars=14

# scenario names
pdssatpmscens=fullharm_noirr,fullharm_firr,default_noirr,default_firr,harmnon_noirr,harmnon_firr
pdssatptscens=fullharm_noirr,fullharm_firr
papsimscens=$pdssatpmscens

# process all DSSAT and APSIM directories
for d in `ls $ggcmidir | egrep 'dssat|apsim'`; do
  # break down directory name
  OLD_IFS=$IFS
  IFS="."
  splitdir=($d)
  IFS=$OLD_IFS
  if [ ${#splitdir[@]} -ne 3 ]; then
    echo "!!! Skipping directory "$d" . . ."
    continue
  fi

  # get model name
  mod=${splitdir[0]}
  if [[ $mod != "dssat45" ]] && [[ $mod != "apsim75" ]]; then
    echo "!!! Unrecognized model in directory "$d" . . ."
  fi
  
  # get climate name and years
  clim=${splitdir[1]}
  climcap=`climcapitalized $clim`
  if [[ $climcap == "" ]]; then
    echo "!!! Unrecognized climate in directory "$d" . . ."
    continue
  fi
  climcap=($climcap) # split along spaces
  yr0=${climcap[1]}
  yr1=${climcap[2]}
  climcap=${climcap[0]}

  # get crop name
  crop=${splitdir[2]}
  cropfull=`cropfullname $crop`
  if [[ $cropfull == "" ]]; then
    echo "!!! Unrecognized crop in directory "$d" . . ."
    continue
  fi

  # print diagnostic
  echo "model = "$mod", climate = "$clim" ("$climcap"), crop = "$crop" ("$cropfull"), yr0 = "$yr0", yr1 = "$yr1
 
  # name of out psims file and GGCMI file suffix 
  outfile=$ggcmidir/$d/out.psims.$mod.$clim.$cropfull.nc4
  suffix=annual_$yr0"_"$yr1

  # replace _ with . in climate name
  clim=`echo $clim | tr _ .`
 
  if [[ $mod == "dssat45" ]]; then # dssat
    # make output directories if necessary
    pmdir=$pdssatpmdir/$climcap # pm
    if [[ ! -d $pmdir ]]; then mkdir $pmdir; fi
    pmdir=$pmdir/$cropfull
    if [[ ! -d $pmdir ]]; then mkdir $pmdir; fi
    ptdir=$pdssatptdir/$climcap # pt
    if [[ ! -d $ptdir ]]; then mkdir $ptdir; fi
    ptdir=$ptdir/$cropfull
    if [[ ! -d $ptdir ]]; then mkdir $ptdir; fi
    # create GGCMI files if directory is incomplete
    if [[ $overwrite == "True" ]] || [[ `ls $pmdir | wc -l` -ne $(($numdssatvars*6)) ]]; then
      echo "   Processing pDSSAT.pm for directory "$d
      prefix="pdssat.pm_"$clim"_hist"
      `$tapp -i $outfile -v "$pdssatvars" -p $prefix -s $suffix -c $crop --scens 1,2,3,4,5,6 --scen_names "$pdssatpmscens"`
      mv *pdssat.pm* $pmdir # move files
    else
      echo "   pDSSAT.pm already processed for "$d
    fi
    if [[ $overwrite == "True" ]] || [[ `ls $ptdir | wc -l` -ne $(($numdssatvars*2)) ]]; then
      echo "Processing pDSSAT.pt for directory "$d
      prefix="pdssat.pt_"$clim"_hist"
      `$tapp -i $outfile -v "$pdssatvars" -p $prefix -s $suffix -c $crop --scens 7,8 --scen_names "$pdssatptscens"`
      mv *pdssat.pt* $ptdir # move files
    else
      echo "   pDSSAT.pt already processed for "$d
    fi
  else # apsim
    # make output directory if necessary
    apsimdir=$papsimdir/$climcap
    if [[ ! -d $apsimdir ]]; then mkdir $apsimdir; fi
    apsimdir=$apsimdir/$cropfull
    if [[ ! -d $apsimdir ]]; then mkdir $apsimdir; fi
    # create GGCMI files if directory is incomplete
    if [[ $overwrite == "True" ]] || [[ `ls $apsimdir | wc -l` -ne $(($numapsimvars*6)) ]]; then
      echo "   Processing pAPSIM for directory "$d
      prefix="papsim_"$clim"_hist"
      `$tapp -i $outfile -v "$papsimvars" -p $prefix -s $suffix -c $crop --scens 1,2,3,4,5,6 --scen_names "$papsimscens"`
      mv *papsim* $apsimdir # move filese
    else
      echo "   pAPSIM already processed for "$d
    fi
  fi
done
