#!/bin/bash

: '
This script aggregates multiple single-gridpoint netcdf files into one big spatial
raster netcdf file. The usage is:

  ./merge_ascii.sh [batch] [num_vars_per_file] [file_dir] [output_dir] [params]

where the input arguments are as follows:

batch: Batch number to run
num_vars_per_file: Number of variables to store in each file
file_dir: Directory from which to search for netcdf files, up to depth of two
output_dir: Directory where the output will be stored
params: psims params file (runNNN/params.psims)

The params file should contain:

var_names: List of variable names extracted from netcdf files
long_names: List of long descriptive names corresponding to var_names
units: List of units corresponding to var_names
num_lons: Number of longitude points in spatial raster
num_lats: Number of latitude points in spatial raster
delta: Distance(s) between each latitude/longitude grid cell in arcminutes
num_years: Number of years in netcdf files
ref_year: Reference year for times in netcdf files
lat_zero: Latitude of grid origin
lon_zero: Longitude of grid origin
out_file: Name of output file

Example:
  ./merge_ascii.sh 1 1 /path/to/input /path/to/output params.psims
'

# ================
# CREATE BLANK CDL
# ================
create_blank_cdl() {
  local sub_var_arr=(${!1})
  local sub_long_arr=(${!2})
  local sub_units_arr=(${!3})
  local cdl_file=$4

  # latitutde
  local num_lats=${#lat[@]}
  lat_str=$(printf ",%s" "${lat[@]}")
  lat_str=${lat_str:1}

  # longitude
  local num_lons=${#lon[@]}
  lon_str=$(printf ",%s" "${lon[@]}")
  lon_str=${lon_str:1}

  # time
  local time=($(seq 1 $num_years))
  time=$(printf ",%s" "${time[@]}")
  time=${time:1}

  # scenario
  local scenario=($(seq 1 $scens))
  scenario=$(printf ",%s" "${scenario[@]}")
  scenario=${scenario:1}

  # variables
  local vars=""
  local i
  for ((i = 0; i < ${#sub_var_arr[@]}; i++)); do
    vars=$vars"\tfloat "${sub_var_arr[i]}"(lat, lon, time, scen) ;\n"
    vars=$vars"\t\t"${sub_var_arr[i]}":units = "\"${sub_units_arr[i]}\"" ;\n"
    vars=$vars"\t\t"${sub_var_arr[i]}":long_name = "\"${sub_long_arr[i]}\"" ;\n"
    vars=$vars"\t\t"${sub_var_arr[i]}":_FillValue = 1.e+20f ;\n"
    vars=$vars"\t\t"${sub_var_arr[i]}":_DeflateLevel = 5 ;\n"
  done

  # data
  local data="data:\n"
  data=$data" lat = $lat_str ;\n"
  data=$data" lon = $lon_str ;\n"
  data=$data" time = $time ;\n"
  data=$data" scen = $scenario ;"

  # write file
  echo -e "netcdf blank {\ndimensions:" > $cdl_file
  echo -e "\tlat = UNLIMITED ;" >> $cdl_file
  echo -e "\tlon = $num_lons ;" >> $cdl_file
  echo -e "\ttime = $num_years ;" >> $cdl_file
  echo -e "\tscen = $scens ;" >> $cdl_file
  echo -e "variables:" >> $cdl_file
  echo -e "\tfloat lat(lat) ;" >> $cdl_file
  echo -e "\t\tlat:units = \"degrees_north\" ;" >> $cdl_file
  echo -e "\t\tlat:long_name = \"latitude\" ;" >> $cdl_file
  echo -e "\tfloat lon(lon) ;" >> $cdl_file
  echo -e "\t\tlon:units = \"degrees_east\" ;" >> $cdl_file
  echo -e "\t\tlon:long_name = \"longitude\" ;" >> $cdl_file
  echo -e "\tint time(time) ;" >> $cdl_file
  echo -e "\t\ttime:units = \"growing seasons since "$ref_year"-01-01 00:00:00\" ;" >> $cdl_file
  echo -e "\t\ttime:long_name = \"time\" ;" >> $cdl_file
  echo -e "\tint scen(scen) ;" >> $cdl_file
  echo -e "\t\tscen:units = \"no\" ;" >> $cdl_file
  echo -e "\t\tscen:long_name = \"scenario\" ;" >> $cdl_file
  echo -e "$vars" >> $cdl_file
  echo -e "$data" >> $cdl_file
}

# ==============
# APPEND MISSING
# ==============
append_missing() {
  local lat1=$1
  local lat2=$2
  for ((k = $lat1; k <= $lat2; k++)); do
    cat $blank_lat_file >> $temp_cdl_file
    if [ $k -ne $num_lats ]; then
      echo ", " >> $temp_cdl_file # add comma and newline
    fi
  done
}

# read inputs from command line
batch=$1
num_vars_per_file=$2
file_dir=$3
output_dir=$4
params=$5

# load parameter file options
source $params

# replace colons with underscores in the variable names
variables=$(echo $variables | sed s/:/'_'/g)

if [ ! -d "$output_dir" ]; then
   mkdir -p $output_dir
fi

# parse variables into array
OLD_IFS=$IFS
IFS=',' # change file separator
var_names_arr=($variables)
delta_arr=($delta)
long_names_arr=($long_names)
units_arr=($var_units)
num_vars=${#var_names_arr[@]}
IFS=$OLD_IFS # reset file separator

# split into multiple files, if necessary
nf=$(printf %.0f $(echo "scale=10;$num_vars/$num_vars_per_file"| bc))
if [ $(($num_vars/$nf)) -lt 1 ]; then
  echo "Two few variables to break into multiple files. Exiting . . ."
  exit 0
fi
si=$(echo "$num_vars_per_file*($batch-1)" | bc)
if [ $batch -eq $nf ]; then
  ei=$num_vars
else
  ei=$(($si+$num_vars_per_file))
  # check for end index out of bounds
  if [ $ei -gt $num_vars ]; then
    ei=$num_vars
  fi
fi

# no work for processor to do
if [ $si -ge $num_vars ]; then
  echo "No jobs for processor to perform. Exiting . . ."
  exit 0
fi

# get latitude and longitude deltas
if [[ ${#delta_arr[@]} -lt 1 || ${#delta_arr[@]} -gt 2 ]]; then
  echo "Wrong number of delta values. Exiting . . ."
  exit 0
fi
latdelta=${delta_arr[0]}
if [ ${#delta_arr[@]} -eq 1 ]; then
  londelta=$latdelta
else
  londelta=${delta_arr[1]}
fi

# calculate longitudes
for ((i = 1; i <= $num_lons; i++)); do
  lon[$(($i-1))]=$(echo "scale=15;$lon_zero+$londelta/60*($i-0.5)" | bc)
done

# calculate latitudes
for ((i = 1; i <= $num_lats; i++)); do
  lat[$(($i-1))]=$(echo "scale=15;$lat_zero-$latdelta/60*($i-0.5)" | bc)
done

# calculate lat0 offset of grid into global grid
lat0_off=$(echo "60*(90-$lat_zero)/$latdelta" | bc)

# calculate number of scenarios from first file
first_file=$(ls $file_dir/* | sort -n | head -1)
size_block=$(head -1 $first_file | grep -o , | wc -l)
scens=$(($size_block/$num_years))

# create blank point (time, scenario) grid
blank_pt=""
for ((i = 0; i < $(($num_years*$scens)); i++)); do
  blank_pt=$blank_pt"_, "
done
blank_pt=${blank_pt%??} # remove extra comma and space

# create blank latitude band file
blank_lat_file="blank_lat_file_"$batch".txt"
touch $blank_lat_file
echo from 1 to $num_lons
for ((i = 1; i <= $num_lons; i++)); do
  if [ $i -eq $num_lons ]; then
    echo -n $blank_pt >> $blank_lat_file # no comma, no newline
  else
    echo $blank_pt", " >> $blank_lat_file # comma, newline
  fi
done

# select subset of variables
nv=$(($ei-$si))
sub_vars=(${var_names_arr[@]:$si:$nv})
sub_long_names=(${long_names_arr[@]:$si:$nv})
sub_units=(${units_arr[@]:$si:$nv})
echo ${sub_vars[@]}
echo ${sub_long_names[@]}
echo ${sub_units[@]}

# temporary cdf filename
temp_cdl_file="temp_file_"$batch".cdl"

# create temporary file
create_blank_cdl sub_vars[@] sub_long_names[@] sub_units[@] $temp_cdl_file

# iterate through all variables
for var in ${sub_vars[@]}; do
  echo "Appending variable "$var" . . ."
  echo -n " "$var" = " >> $temp_cdl_file

  # find all files belonging to variable
  files=(`ls $file_dir/* | egrep "$var"_[0-9]`) # match variable followed by _number

  # iterate over files, filling in gaps
  next_lat=1
  for f in ${files[@]}; do
    # get latitude index
    grid1=`echo $f | sed "s/.*_\(.*\).txt/\1/"`
    grid1=`echo $grid1 | sed 's/^0*//'` # remove leading zeros
    grid1=$(($grid1-$lat0_off))
    echo Grid1 is $grid1

    # insert missing latitudes, if necessary
    append_missing $next_lat $((grid1-1))

    # append file
    cat $f >> $temp_cdl_file
    if [ $grid1 -ne $num_lats ]; then
      echo ", " >> $temp_cdl_file # add comma and newline
    fi

    # increment latitude index
    next_lat=$(($grid1+1))
  done

  # insert missing latitudes, if necessary
  append_missing $next_lat $num_lats

  # add semicolon
  echo " ;" >> $temp_cdl_file
done

# add ending bracket
echo "}" >> $temp_cdl_file

# convert to netcdf
echo "Running ncgen . . ."
if [ ${#sub_vars[@]} -eq 1 ]; then
  fn=$output_dir/$out_file"."${sub_vars[@]}".nc4"
else
  fn=$output_dir/$out_file"."$batch".nc4"
fi
echo Writing output to $fn
time ncgen -k4 -o $fn $temp_cdl_file

# remove temporary files
# rm $blank_lat_file
# rm $temp_cdl_file

echo "Done!"
