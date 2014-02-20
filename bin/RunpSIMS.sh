#!/bin/bash -x

latidx=$1
lonidx=$2
params=$3
tar_out=$4

source $params

# run_command - print time stamp before and after run
# Could also use 'time' here, but this is better in
# in situations where a program can hang
run_command() {
   command="$@"
   echo Running \"$command\" at $( date +"%T" )
   START=$SECONDS
   $command 2>&1
   result=$?
   STOP=$SECONDS
   if [ "$result" -ne 0 ]; then
      echo "Command \"$command\" failed with an exit code of $result"
      exit 1
   else
      echo "Command succeeded at $( date +"%T" ). It tooks $((STOP - START)) second(s)"
   fi
   echo
}

# Similar to run_command, but redirect command output to a file
run_command_redirect() {
   output=$1
   shift
   command="$@"
   echo Running \"$command\" at $( date +"%T" )
   START=$SECONDS
   $command > $output 2>&1
   STOP=$SECONDS
   echo "Completed at $( date +"%T" ). It tooks $((STOP - START)) second(s)"
   echo
}

# Flatten directory structure
echo "Flattening directory structure"
find $PWD -mindepth 2 -type f -exec ln -s {} \;
find $PWD -mindepth 2 -type l -exec ln -s {} \;

# Make all python, perl, and bash scripts executable. If tapps or other stuff in any 
# other language, add suffix here.
chmod +x *.py *.pl *.sh *.EXE *.exe

##################################################################################
# Run tappcamp application to generate the experiment file from the campaign file.
# Input campaign file called something like campaign.nc; defines the spatial experiment. 
# This file is a single netcdf file that contains arbitary number of variables 
# that must be changed spatially in the experiment files. The experiment 
# file is model agnostic and always called experiment.json. 
tappcamp="$( echo $tappcamp | sed s/:/' '/g ) --latidx $latidx --lonidx $lonidx --ref_year $ref_year --delta $delta --nyers $num_years --nscens $scens" 
run_command ./$tappcamp

###############################################################################
# Run tappinp application to generate the input files (.XXX and .SOL for DSSAT 
# and .apsim for APSIM) from the experiment.json file
if [ "$model" == "dssat45" ] ; then suff=".SOL .MZX .RIX .WHX .SBX" ; fi
if [ "$model" == "apsim75" ] ; then suff=".apsim" ; fi
if [ "$model" == "cenw" ]    ; then suff=".PJ!" ; fi

tappinp="$( echo $tappinp | sed s/:/' '/g ) "
run_command ./$tappinp                

###############################################################################
# Run tappwth application to generate the weather file from the .psims file
# input files usually of form latidx_lonidx.psims.nc and outputs files with 
# generic names like GENERIC.WTH (for DSSAT) and Generic.met (for APSIM). 
if [ "$model" == "dssat45" ] ; then suff=".WTH" ; fi
if [ "$model" == "apsim75" ] ; then suff=".met" ; fi
if [ "$model" == "cenw" ]    ; then suff=".CL!" ; fi
ls -l *$suff 1>&2

for file in *.psims.nc; do 
  tappwth="$( echo $tappwth | sed s/:/' '/g ) -i $file" 
  run_command ./$tappwth                         
done

#########################################################
################# Run the impact model #################
executable=$( echo $executable | sed s/:/' '/g )

   ###########
   # DSSAT45 #
if [ "$model" == "dssat45" ] ; then 
 commandToRun="$executable"
 run_command_redirect RESULT.OUT ./$commandToRun
fi 

   ###########
   # APSIM75 #
if [ "$model" == "apsim75" ] ; then
   run_command tar -xzvf Apsim75.exe.tar.gz  # expand exe & files needed to run (have correct permissions)
   mv *.xml Model/               # If user adds custom [crop].xml file, overwrites the default
   mv ./Model/Apsim.xml ./
   source ./paths.sh                    # Sets boost and mono and ld_lib paths for the worker node
   run_command mono ./Model/ApsimToSim.exe Generic.apsim
   for file in *.sim; do
      commandToRun="$executable $file"
      run_command_redirect RESULT.out $commandToRun
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
         run_command_redirect RESULT-T.OUT ./$commandToRun
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

# Tar and compress output
mkdir -p output
for file in $( ls $( echo $outtypes | sed s/,/' *'/g ) 2>/dev/null); do 
   ln -s $PWD/$file output/
done
run_command tar czhf output.tar.gz output
dd if=output.tar.gz of=$tar_out bs=16777216

#################################################################################
# Extract data from output files into a single 'psims.nc' file with all variables
if [ _$postprocess != __ ]; then
 postprocess=$( echo $postprocess | sed s/:/' '/g )
 mkdir -p parts
 mkdir -p parts/$latidx
 postprocessToRun="$postprocess --latidx $latidx --lonidx $lonidx --ref_year $ref_year --delta $delta -y $num_years -s $scens -v $variables -u $var_units --output parts/$latidx/$lonidx.psims.nc"
 run_command ./$postprocessToRun
 exit_status=$?
fi

exit 0
