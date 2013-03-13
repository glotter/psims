#!/bin/bash -x

out=$1
executable=$2
outtypes=$3
postprocess=$4
model=$5
shift 5


# Replace :'s in executable and postprocess with spaces
executable=$( echo $executable | sed s/:/' '/g )

# All other arguments beyond this are lists of input files
mkdir -p data
for var in "$@"
do
   for file in $var
   do
      ln -s $PWD/$file data/$( basename $file )
      ln -s $PWD/$file $( basename $file )
   done
done

# Run pSIMS
chmod +x *.EXE
cd data
commandToRun="$executable"
# if batch is set to true, we're going to do multiple executable calls with 
# some steps in between. this may be difficult to generalize to all models... 
if [ "$model" == "cenw" ]; then
 mv CenW.CL! CenW-T.CL!
  for tt in {0..95..1} ; do
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
if [ "$model" == "dssat45" ] ; then 
 ./$commandToRun > RESULT.OUT 2>&1
fi
if [ "$model" == "dssat40" ] ; then 
 ./$commandToRun > RESULT.OUT 2>&1
fi
exit_status=$?
cd ..

# Tar and compress output
mkdir -p output
for oo in $( echo $outtypes | sed s/,/' '/g ) ; do
cp data/*$oo output
done
tar czf $out output

# Extract data from Summary.OUT into a single 'part' file with all info
if [ _$postprocess != __ ]; then
 postprocess=$( echo $postprocess | sed s/:/' '/g )
 grid=$( basename $out output.tar.gz )
 gridDir=$( dirname $out | cut -c'8-16' )
 mkdir -p parts
 mkdir -p parts/$gridDir
 touch parts/$gridDir/$grid.part
 chmod +x *.pl
 postprocessToRun="$postprocess --grid $grid --outfile parts/$gridDir/$grid.part"
 ./$postprocessToRun
 exit_status=$?
fi

exit 0
