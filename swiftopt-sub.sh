#!/bin/bash

# Usage: swiftopt.sh [-s sitename] [-p paramfile] [-g gridlist] 
#
# NOTE: this command expects symlink "swift" in the cur dir to point
# to relese installed by setup.sh If you want to run with a different
# swift release, replace symlink "swift" with a link to your swift
# release dir.

usage="$0 [-s sitename] [-p paramfile] [-g gridlist]"
site=$2
# Function to run Swift
runswift() {
   export SWIFT_HEAP_MAX=$ram
   command="swift -tc.file tc.data -sites.file $site.xml -config cf RunpSIMS-nodebug.swift "
   for param in $( awk '{print $1}' paramfile )
   do
      command="$command -$param=${!param}"
   done
   $command 2>&1 | tee swift.out
}

# Default settings
execsite=local
paramfile=local
#ram=4096M
ram=5120M

# Process command line arguments
while [ $# -gt 0 ]; do
  case $1 in
    -g) gridlist=$2; shift 2;;
    -p) paramfile=$2; shift 2;;
    -s) execsite=$2; shift 2;;
    *) echo $usage 1>&2
       exit 1;;
  esac
done

# Create next unique run id and run directory
rundir=$( echo run??? | sed -e 's/^.*run//' | awk '{ printf("run%03d\n", $1+1)}' )

# Exit if rundir already exits. Something is funky
if [ -d $rundir ];
then
    echo "$rundir already exists! exiting." >&2
    exit 2
else
    mkdir $rundir
fi

# Get optimization parameters
if [ ! -f $paramfile ];
then
    echo "Could not find parameter file $paramfile in params!"
    exit 1
fi
cp $paramfile $rundir/paramfile
sed -e '/^[[:space:]]*\(#.*\)*$/d' -e 's/#.*//' -e 's/  */=/' -e 's/^/export /' <$paramfile >$rundir/params.psims
source $rundir/params.psims
echo Run directory $rundir: site=$execsite paramfile=$paramfile

# Copy the Campaign.nc4 file in the rundir so we can append it to the data
cp $campaign/*.nc4 $rundir/

# Report an error if configuration files are missing
if [ ! -f "conf/$execsite.xml" ] && [ ! -f "conf/$execsite.conf" ]; then
   echo Unable to find requested configuration file for site $execsite
   exit 1
fi

# Use start-coaster-service if site is a .conf file
if [ -f "conf/$execsite.conf" ]; then
   USE_SCS=1
fi

# Check for missing .cf files
if [ -f "conf/$execsite.xml" ] && [ ! -f "conf/$execsite.cf" ]; then
   echo Missing configuration file $execsite.cf
fi

cp $gridlist $rundir/gridList.txt
cp RunpSIMS-nodebug.swift $rundir
cp bin/RunpSIMS.sh $rundir

cat << END > $rundir/restart.sh
#!/bin/bash
export SWIFT_HEAP_MAX=5120M
../bin/regen_gridlist.pl
start-coaster-service
time swift -tc.file tc.data -sites.file $site.xml -config cf RunpSIMS.swift -model=$model -scenarios=$scenarios -weather=$weather -refdata=$refdata -bindata=$bindata -bintransfer=$bintransfer -executable=$executable -outtypes=$outtypes -postprocess=$postprocess
stop-coaster-service
echo Running CSV extractions.. please wait
time ../bin/extract_csv.pl
END
chmod +x $rundir/restart.sh

# Echo parameters
echo Parameters:
echo
cat $rundir/params.psims
echo

# Do the run
cd $rundir
export WORK=$PWD/swiftwork
export SWIFT_USERHOME=$PWD
mkdir -p $PWD/swiftwork/workers

# Use start-coaster-service if the site uses a .conf file
if [ "$USE_SCS" == "1" ]; then
   cp ../conf/$execsite.conf coaster-service.conf
   cp ../conf/$execsite.cf cf
   sed -i -e "s@_RUNDIR_@$rundir@" coaster-service.conf
   start-coaster-service
fi

# Run gensites
if [ ! "$USE_SCS" == 1 ]; then
   cp ../conf/$execsite.cf cf
   gensites -p ../conf/$execsite.cf ../conf/$execsite.xml > $execsite.xml
fi

echo "Running with paramter file $paramfile" >> ABOUT
echo "Running on site $execsite" >> ABOUT

if [ "$USE_SCS" == "1" ]; then
   runswift "sites.xml"
   stop-coaster-service
else
   runswift "$execsite.xml"
fi

# Post Post processing
echo
echo "Running post-post-processing . . ."

# calculate number of variables
OLD_IFS=$IFS
IFS=',' # change file separator
var_names_arr=($variables)
num_vars=${#var_names_arr[@]}
IFS=$OLD_IFS # reset file separator

# calculate number of latitude bands
num_lat_bands=$(ls parts | wc -l)

# write runtask_append in run directory
cat << END > runtask_append
#!/bin/bash
../bin/append_ascii_par.sh \$1 128 "$variables" $num_lons $delta $num_years $scens $lon_zero ./parts
END
chmod +x runtask_append

# write parallel_append in run directory
cat << END > parallel_append.sbatch
#!/bin/sh
#SBATCH --time=01:00:00
#SBATCH --ntasks=128
#SBATCH --exclusive
#SBATCH --mem-per-cpu=2048
#SBATCH --partition=westmere
module load parallel
srun="srun -N1 -n1"
parallel="parallel -j \$SLURM_NTASKS --joblog runtask_append.log --resume"
\$parallel "\$srun ./runtask_append {1} > runtask_append.{1}" ::: {1..128}
END
chmod +x parallel_append.sbatch

# make var_files directory to store files
mkdir var_files

# run first job
echo "Creating variable files . . ."
sbatch parallel_append.sbatch

tot_files=$(($num_vars*$num_lat_bands))
while true; do
   sleep 60 # wait 60 seconds
   num_files=$(ls var_files | wc -l) # number of files in var_files directory
   if [ $num_files -eq $tot_files ]; then
      # all files created
      sleep 60 # wait 60 seconds before proceeding
      break
   fi
done

# clean up after run
rm runtask_append.*
rm slurm*

# write runtask_merge in run directory
cat << END > runtask_merge
#!/bin/bash
../bin/merge_ascii.sh \$1 "$variables" "$long_names" "$var_units" $num_lons $num_lats $delta $num_years $scens $ref_year $lat_zero $lon_zero 1 ./var_files $out_file
END
chmod +x runtask_merge

# write parallel_merge in run directory
cat << END > parallel_merge.sbatch
#!/bin/sh
#SBATCH --time=01:00:00
#SBATCH --ntasks=$num_vars
#SBATCH --exclusive
#SBATCH --mem-per-cpu=20480
#SBATCH --partition=westmere
module load parallel
srun="srun -N1 -n1"
parallel="parallel -j \$SLURM_NTASKS --joblog runtask_merge.log --resume"
\$parallel "\$srun ./runtask_merge {1} > runtask_merge.{1}" ::: {1..$num_vars}
END
chmod +x parallel_merge.sbatch

# run second job
echo "Merging variables . . ."
sbatch parallel_merge.sbatch

while true; do
   sleep 60 # wait 60 seconds
   psims_files=(`ls $out_file*`) # array of psims files
   if [ ${#psims_files[@]} -eq $num_vars ]; then
      # all files created
      byte_count=0
      while true; do
         # get total byte count
         new_byte_count=0
         for f in $psims_files; do
            bcf=`stat -c%s $f`
            new_byte_count=$(($new_byte_count+$bcf))
         done
         if [ $new_byte_count -eq $byte_count ]; then
            break
         fi
         sleep 15 # wait 15 seconds
         byte_count=$new_byte_count
      done
      break
   fi
done

# clean up after run
rm runtask_merge.*
rm slurm*

echo "Appending variables into one file . . ."
first_file=${psims_files[0]}
final_file=$out_file".nc4"
echo "  Appending "${var_names_arr[0]}" . . ."
cp $first_file $final_file
for ((i = 1; i < $num_vars; i++)); do
   echo "  Appending "${var_names_arr[$i]}" . . ." 
   ncks -A -h ${psims_files[$i]} $final_file
done
echo "  Appending campaign file . . ."
ncks -A -h Campaign.nc4 $final_file

echo
echo "Finished!"

exit
