#!/bin/bash -x

tar_out=$1
executable=$2
outtypes=$3
postprocess=$4
tappwth=$5
tappinp=$6
tappcamp=$7
latidx=$8
lonidx=$9
model=${10}
ref_year=${11}
delta=${12}
num_years=${13}
scens=${14}
variables=${15}
long_names=${16}
var_units=${17}
num_lats=${18}
num_lons=${19}
lat_zero=${20}
lon_zero=${21}
part_out=${22}
common_files=${23}
swift_pid=${24}
shift 24

timestamp() {
  date +"%T"
}

exec > >(tee /scratch/midway/davidkelly999/temp/$latidx.$lonidx.log)
exec 2>&1

timestamp
hostname -f
ulimit -a
TIMEARGS=(-o swiftapp.resources -f APP_RESOURCES=real_secs:%e,kernel_secs:%S,user_secs:%U,percent_cpu:%P,max_rss:%M,avg_rss:%t,avg_tot_vm:%K,avg_priv_data:%D,avg_priv_stack:%p,avg_shared_text:%X,page_size:%Z,major_pgfaults:%F,minor_pgfaults:%R,swaps:%W,invol_context_switches:%c,vol_waits:%w,fs_reads:%I,fs_writes:%O,sock_recv:%r,sock_send:%s,signals:%k,exit_status:%x)

dateString=$( date +%Y%m%d )
psims_cache_prefix="/scratch/midway/$USER/psims.tmp"
psims_cache="$psims_cache_prefix/$dateString.$swift_pid"
psims_cache_tmp="$psims_cache.tmp"
orig_dir=$PWD

echo Creating cache
timestamp

# Node caching for common data
if [ ! -d "$psims_cache" ]; then
  mkdir -p $psims_cache_prefix
  if mkdir "$psims_cache_tmp" 2>/dev/null; then
     for dir in $psims_cache_prefix/*
     do
        if [[ "$dir" != *.tmp* ]]; then
           rm -rf $dir >> $HOME/clean.log 2>&1
        fi
     done
     for file in $common_files
     do
        echo psims_cache_tmp is $psims_cache_tmp
        ls -l $psims_cache_tmp
        dd if=/$file of=$psims_cache_tmp/$(basename $file) bs=16M
     done
     mv $psims_cache_tmp $psims_cache
  else
    while [ ! -d "$psims_cache" ]; do
      sleep 1;
    done
  fi
fi

# Link common data from cached directory
for file in $common_files
do
   local_copy=$psims_cache/$(basename $file)
   ln -s $local_copy
done

# All other arguments beyond this are inputs files unique to each grid
for var in "$@" ; do
  for file in $var ; do
    dd if=/$file of=$(basename $file) bs=16M
done ; done

# Make all python, perl, and bash scripts executable. If tapps or other stuff in any 
# other language, add suffix here.
chmod +x *.py *.pl *.sh *.EXE *.exe

##################################################################################
# Run tappcamp application to generate the experiment file from the campaign file.
# Input campaign file called something like campaign.nc; defines the spatial experiment. 
# This file is a single netcdf file that contains arbitary number of variables 
# that must be changed spatially in the experiment files. The experiment 
# file is model agnostic and always called experiment.json. 
timestamp
tappcamp="$( echo $tappcamp | sed s/:/' '/g ) --latidx $latidx --lonidx $lonidx --ref_year $ref_year --delta $delta " 
echo Campaign translator command: $tappcamp
ls -l experiment.json 1>&2
./$tappcamp
ls -l experiment.json 1>&2
timestamp
#ln -s $PWD/experiment.json data/experiment.json

###############################################################################
# Run tappinp application to generate the input files (.XXX and .SOL for DSSAT 
# and .apsim for APSIM) from the experiment.json file
if [ "$model" == "dssat45" ] ; then suff=".SOL .MZX .RIX .WHX .SBX" ; fi
if [ "$model" == "apsim75" ] ; then suff=".apsim" ; fi
if [ "$model" == "cenw" ]    ; then suff=".PJ!" ; fi
for sf in $suff ; do ls -l *$sf 1>&2 ; done 

timestamp
tappinp="$( echo $tappinp | sed s/:/' '/g ) "
echo Input translator command: $tappinp
./$tappinp                # run the input translator app
timestamp

#for sf in $suff ; do     # since tappinp can create multipl files, loop over suffixes
# for file in $( ls *$sf ) ; do # link the input files to the data directory
#   ln -s $PWD/$file data/$( basename $file ) 
# done ; done
for sf in $suff ; do ls -l *$sf 1>&2 ; done 

###############################################################################
# Run tappwth application to generate the weather file from the .psims file
# input files usually of form latidx_lonidx.psims.nc and outputs files with 
# generic names like GENERIC.WTH (for DSSAT) and Generic.met (for APSIM). 
if [ "$model" == "dssat45" ] ; then suff=".WTH" ; fi
if [ "$model" == "apsim75" ] ; then suff=".met" ; fi
if [ "$model" == "cenw" ]    ; then suff=".CL!" ; fi
ls -l *$suff 1>&2

for file in $( ls *.psims.nc ) ; do # build the command line for the weather translator
  timestamp
  tappwth="$( echo $tappwth | sed s/:/' '/g ) -i $file" 
  echo Weather translator command: $tappwth
  ./$tappwth                         # run weather translator app 
  timestamp
done
#for file in $( ls *$suff ) ; do      # link the .wth type files to the data directory
#  ln -s $PWD/$file data/$( basename $file ) 
#done
ls -l *$suff 1>&2

#########################################################
################# Run the impact model #################
#cd data
timestamp
executable=$( echo $executable | sed s/:/' '/g )
timestamp

   ###########
   # DSSAT45 #
if [ "$model" == "dssat45" ] ; then 
 commandToRun="$executable"
 echo Executable: $commandToRun
 ./$commandToRun > RESULT.OUT 2>&1
fi 

   ###########
   # APSIM75 #
if [ "$model" == "apsim75" ] ; then
 timestamp
 tar -xzvf Apsim75.exe.tar.gz  # expand exe & files needed to run (have correct permissions)
 mv *.xml Model/               # If user adds custom [crop].xml file, overwrites the default
 mv ./Model/Apsim.xml ./
 ./paths.sh                    # Sets boost and mono and ld_lib paths for the worker node
 timestamp

 # strace -t -f -e open,close,fork,execve -o /scratch/midway/davidkelly999/temp4/strace.$latidx.$lonidx.log mono ./Model/ApsimToSim.exe Generic.apsim        # converts .apsim file into N .sim files
 mono ./Model/ApsimToSim.exe Generic.apsim        # converts .apsim file into N .sim files
 timestamp

 for file in $( ls *.sim ) ; do
  timestamp
  commandToRun="$executable $file"
  echo Executable: $commandToRun                  # run the model on each of the .sim files
  # strace -t -f -e open,close,fork,execve -o /scratch/midway/davidkelly999/temp4/strace.$latidx.$lonidx.$(basename $file ).log $commandToRun >> RESULT.out 2>&1
  $commandToRun >> RESULT.out 2>&1
  timestamp
 done
fi

   ##########
   # CENW40 #
if [ "$model" == "cenw" ]; then
 mv CenW.CL! CenW-T.CL!
  for tt in {0..95..5} ; do
    let "tlin = ( 129 - $tt )*1461/4"
    lin=$( printf "%0.f\n" $tlin )
    tailcom="tail -$lin CenW-T.CL! "
    $tailcom > CenW.CL!
    for cap in {1..7} ; do
      tail -3652 CenW-T.CL! >> CenW.CL!
    done
    for hh in {1..9} ; do
      cp PJHead.TMP CenW.PJ!
      cat Scene-$hh.PJ! >> CenW.PJ!
      ./$commandToRun > RESULT-T.OUT 2>&1
      echo "Scenario $hh planting year $tt" >> RESULT.OUT
      cat RESULT-T.OUT >> RESULT.OUT
      rm RESULT-T.OUT
      if [ $tt -eq 0 -a $hh -eq 1  ] ; then
       head -105 CenW.DT! > CenW-101.DT!
       else
       tail -102 CenW.DT! | head -101 >> CenW-101.DT!
      fi
      rm CenW.DT!
    done
  done
fi
exit_status=$?
#cd ..

# Tar and compress output
mkdir -p output
for oo in $( echo $outtypes | sed s/,/' '/g ) ; do
   for file in *$oo
   do
      dd if=$file of=output/$( basename $file )
   done
done
timestamp
tar czf output.tar.gz output
timestamp

# tar czf output.tar.gz *
timestamp
mkdir -p $( dirname $tar_out )
dd if=output.tar.gz of=$tar_out bs=16M
timestamp

#################################################################################
# Extract data from output files into a single 'psims.nc' file with all variables
if [ _$postprocess != __ ]; then
 timestamp
 postprocess=$( echo $postprocess | sed s/:/' '/g )
 mkdir -p parts/$latidx
 postprocessToRun="$postprocess --latidx $latidx --lonidx $lonidx --ref_year $ref_year --delta $delta -y $num_years -s $scens -v $variables -u $var_units --output parts/$latidx/$lonidx.psims.nc"
 ./$postprocessToRun
 timestamp
 mkdir -p $( dirname $part_out )
 timestamp
 dd if=parts/$latidx/$lonidx.psims.nc of=$part_out bs=16M
 timestamp
fi

exit 0
