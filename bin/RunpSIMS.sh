#!/bin/bash -x

out=$1
executable=$2
outtypes=$3
postprocess=$4
tappwth=$5
tappinp=$6
tappcamp=$7
grid1=$8
grid2=$9
model=${10}
shift 10

# All other arguments beyond this are lists of input files
#mkdir -p data
for var in "$@" ; do
  for file in $var ; do
#    ln -s $PWD/$file data/$( basename $file )
    ln -s $PWD/$file $( basename $file )
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
tappcamp="$( echo $tappcamp | sed s/:/' '/g ) --grid1 $grid1 --grid2 $grid2 " 
echo Campaign translator command: $tappcamp
ls -l experiment.json 1>&2
#strace -o strace.log 
./$tappcamp
ls -l experiment.json 1>&2
#ln -s $PWD/experiment.json data/experiment.json

###############################################################################
# Run tappinp application to generate the input files (.XXX and .SOL for DSSAT 
# and .apsim for APSIM) from the experiment.json file
if [ "$model" == "dssat45" ] ; then suff=".SOL .MZX .RIX .WHX .SBX" ; fi
if [ "$model" == "apsim75" ] ; then suff=".apsim" ; fi
if [ "$model" == "cenw" ]    ; then suff=".PJ!" ; fi
for sf in $suff ; do ls -l *$sf 1>&2 ; done 

tappinp="$( echo $tappinp | sed s/:/' '/g ) "
echo Input translator command: $tappinp
./$tappinp                # run the input translator app

#for sf in $suff ; do     # since tappinp can create multipl files, loop over suffixes
# for file in $( ls *$sf ) ; do # link the input files to the data directory
#   ln -s $PWD/$file data/$( basename $file ) 
# done ; done
for sf in $suff ; do ls -l *$sf 1>&2 ; done 

###############################################################################
# Run tappwth application to generate the weather file from the .psims file
# input files usually of form grid1_grid2.psims.nc and outputs files with 
# generic names like GENERIC.WTH (for DSSAT) and Generic.met (for APSIM). 
if [ "$model" == "dssat45" ] ; then suff=".WTH" ; fi
if [ "$model" == "apsim75" ] ; then suff=".met" ; fi
if [ "$model" == "cenw" ]    ; then suff=".CL!" ; fi
ls -l *$suff 1>&2

for file in $( ls *.psims.nc ) ; do # build the command line for the weather translator
  tappwth="$( echo $tappwth | sed s/:/' '/g ) -i $file" 
  echo Weather translator command: $tappwth
  ./$tappwth                         # run weather translator app 
done
#for file in $( ls *$suff ) ; do      # link the .wth type files to the data directory
#  ln -s $PWD/$file data/$( basename $file ) 
#done
ls -l *$suff 1>&2

#########################################################
################# Run the impact model #################
#cd data
executable=$( echo $executable | sed s/:/' '/g )


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
 tar -xzvf Apsim75.exe.tar.gz  # expand exe & files needed to run (have correct permissions)
 mv *.xml Model/               # If user adds custom [crop].xml file, overwrites the default
 mv ./Model/Apsim.xml ./
 ./paths.sh                    # Sets boost and mono and ld_lib paths for the worker node
 mono ./Model/ApsimToSim.exe Generic.apsim        # converts .apsim file into N .sim files
 for file in $( ls *.sim ) ; do
  commandToRun="$executable $file"
  echo Executable: $commandToRun                  # run the model on each of the .sim files
  $commandToRun >> RESULT.out 2>&1
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
#cp data/
cp *$oo output
done
tar czf $out output

#################################################################################
# Extract data from output files into a single 'psims.nc' file with all variables
if [ _$postprocess != __ ]; then
 postprocess=$( echo $postprocess | sed s/:/' '/g )
 mkdir -p parts
 mkdir -p parts/$grid1
 postprocessToRun="$postprocess --grid1 $grid1 --grid2 $grid2 --output parts/$grid1/$grid2.psims.nc"
 ./$postprocessToRun
 exit_status=$?
fi

exit 0
