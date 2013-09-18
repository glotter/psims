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
   script=$1
   export SWIFT_HEAP_MAX=$ram
   command="swift -tc.file tc.data -sites.file $site.xml -config cf "

   # Set command line options based on param file   
   command="$command $script "
   for param in $( awk '{print $1}' paramfile )
   do
      command="$command -$param=${!param}"
   done

   echo "Running Swift command $command" >> ABOUT
   $command 2>&1 | tee swift.out
}

# Default settings
execsite=local
paramfile=local
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
export rundir=$( echo run??? | sed -e 's/^.*run//' | awk '{ printf("run%03d\n", $1+1)}' )

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

# Report an error if configuration files are missing
if [ ! -f "conf/$execsite.xml" ] && [ ! -f "conf/$execsite.conf" ]; then
   echo Unable to find requested configuration file for site $execsite
   exit 1
fi

# Check for missing .cf files
if [ -f "conf/$execsite.xml" ] && [ ! -f "conf/$execsite.cf" ]; then
   echo Missing configuration file $execsite.cf
fi

# Copy CDM files
if [ -f "conf/$execsite.fs" ]; then
   cp conf/$execsite.fs $rundir
fi

cp $gridlist $rundir/gridList.txt
cp RunpSIMS.swift $rundir
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

cp ../conf/$execsite.cf cf
gensites -p ../conf/$execsite.cf ../conf/$execsite.xml > $execsite.xml

echo "Running with paramter file $paramfile" >> ABOUT
echo "Running on site $execsite" >> ABOUT
echo "Running with gridlist $gridlist" >> ABOUT 

runswift "RunpSIMS.swift"

# Post Post processing
echo
echo Running extractions.. please wait
NCconcat=$( echo $NCconcat | sed s/:/' '/g )
time ./$NCconcat

exit
