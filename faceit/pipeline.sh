#! /bin/bash
set -x
set -e

quaduijar="./quadui/quadui-1.2.1-SNAPSHOT-Beta12.jar"
acmouijar="./acmo/acmoui-1.2-SNAPSHOT-beta4.jar"
#surveydata="./quadui/Survey_data_import.aceb"
#fielddata="./quadui/Field_Overlay.zip"
#surveydata="./EMBU/Survey_data_import.aceb"
#fielddata="./EMBU/Field_Overlay.zip"
surveydata="./ISHIARA/Survey_data_import.aceb"
fielddata="./ISHIARA/Field_Overlay.zip"
quadoutdir="quadoutdir"

echo "Starting ..."
sleep 1

echo " Run quadui ... "
java -Xms256m -Xmx768m -jar $quaduijar -cli -zip -clean -f -JD $surveydata ' '  $fielddata $quadoutdir
RC=$?
if [ $RC -ne 0 ]
then
	exit $RC
fi

# Copy DSSAT Auxiliary file bundle to quadoutdir/DSSAT
if [ -d $quadoutdir/DSSAT ]
then
    cp dssat_aux.tgz $quadoutdir/DSSAT
else
    echo "DSSAT directory does not exist in $quadoutdir"
    exit 1
fi

#unzip quadoutdir/DSSAT
cd $quadoutdir/DSSAT
unzip DSSAT_Input.zip
RC=$?
if [ $RC -ne 0 ]
then
	exit $RC
fi

# untar the auxiliary package here and move all files in DSSAT dir
tar zxf dssat_aux.tgz && mv dssat_aux/* .
RC=$?
if [ $RC -ne 0 ]
then
	exit $RC
fi
echo "quadui done,  run DSSAT ... "
sleep 1

for i in *.MZX
do
   ./DSCSM045.EXE A $i
   RC=$?
   mkdir -p ${i}_dir
   cp *.OUT ${i}_dir
   cp LUN.LST ${i}_dir
   cp DSSAT45.INP ${i}_dir
   cp DSSAT45.INH ${i}_dir
   if [ $RC -ne 0 ]
   then
	exit $RC
   fi
done > dssat_output.txt

#./DSCSM045.EXE A MACH0012.MZX #test

#cd back to current dir
cd -

echo " DSSAT done, Run acmoui ..."
for i in ${quadoutdir}/DSSAT/*.MZX
do
 cp ${i}_dir/* ${quadoutdir}/DSSAT
 java -jar $acmouijar -cli -dssat ${quadoutdir}/DSSAT
 RC=$?
 if [ $RC -ne 0 ]
 then
	exit $RC
 fi
done

echo "Done. Check out results in $quadoutdir/DSSAT"
exit $RC

