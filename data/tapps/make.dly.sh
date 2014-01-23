dir=/project/joshuaelliott/psims/data/clim/agmerra
metafile=/project/joshuaelliott/psims/data/tapps/WDLSTCOM.dat
csvfile=/project/joshuaelliott/data/NASA.CO2.Annual.1850-2013.csv
outdir=/project/joshuaelliott/data/agmerra.epic.ggcmi
app=/project/joshuaelliott/psims/data/tapps/psims2dly.py
for i in `ls $dir`; do
  for j in `ls $dir/$i`; do
    file=$dir/$i/$j/$i"_"$j.psims.nc
    lat=$(echo `ncks -v latitude $file` | sed -s 's/.*latitude\[0\]=\(.*\) degrees_north/\1/')
    lon=$(echo `ncks -v longitude $file` | sed -s 's/.*longitude\[0\]=\(.*\) degrees_east/\1/')
    OLD_IFS=$IFS
    IFS=","
    src=$(echo "`printf "%9.2f" $lon``printf "%9.2f" $lat`")
    s=`cat $metafile | grep "$src"`
    IFS=$OLD_IFS
    s=($s) # break along spaces
    if [ "${s[1]}" ]; then
      echo "Processing lat = "$lat", lon = "$lon
      outfile=${s[1]}
      `$app -i $file -v rsds,tasmax,tasmin,pr,hur,wind -o $outdir/$outfile -c $csvfile`
    else
      echo "Skipping . . ."
    fi
  done
done
