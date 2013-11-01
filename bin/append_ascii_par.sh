#!/bin/bash

: '
The usage is:

  ./append_ascii_par.sh [batch] [num_batches] [var_names] [num_lons] [delta] [num_years]
                        [num_scenarios] [lon0] [file_dir]

where the input arguments are as follows:

batch: Batch number to run
num_batches: Number of total batches
var_names: List of variable names extracted from netcdf files
num_lons: Number of longitude points in spatial raster
delta: Distance(s) between each latitude/longitude grid cell in arcminutes
num_years: Number of years in netcdf files
num_scenarios: Number of scenarios in netcdf files
lon0: Longitude of grid origin
file_dir: Directory from which to search for netcdf files, up to depth of two

Example:
  ./append_ascii_par.sh 1 1 PDAT 2 30 44 6 -180 .
'

# ==============
# APPEND MISSING
# ==============
append_missing() {
  local lon1=$1
  local lon2=$2
  local lat=$3

  for ((k = $lon1; k <= $lon2; k++)); do
    for var in ${var_names_arr[@]}; do
      if [ $k -eq $num_lons ]; then
        echo -n $blank_pt >> var_files/$var"_"$lat".txt" # no comma, no newline
      else
        echo $blank_pt", " >> var_files/$var"_"$lat".txt" # comma, newline
      fi
    done
  done
}

# read inputs from command line
batch=$1
num_batches=$2
var_names=$3
num_lons=$4
delta=$5
num_years=$6
num_scenarios=$7
lon0=$8
file_dir=$9

# parse variables into array
OLD_IFS=$IFS
IFS=',' # change file separator
var_names_arr=($var_names)
delta_arr=($delta)
IFS=$OLD_IFS # reset file separator

# blank point
blank_pt=""
for ((i = 0; i < $(($num_years*$num_scenarios)); i++)); do
  blank_pt=$blank_pt"1e20, "
done
blank_pt=${blank_pt%??} # remove extra comma and space

# create temp directory
if [ ! -d "var_files" ]; then
  mkdir var_files
fi

# find all directories
dcts=(`ls -d $file_dir/*/ | grep '[0-9]'`)
num_lats=${#dcts[@]}

# find out start and end indices for batch
bz=$(echo "($num_lats + $num_batches - 1)/$num_batches" | bc) # batch size (use ceiling)
si=$(echo "$bz*($batch-1)" | bc)
if [ $batch -eq $num_batches ]; then
  ei=$num_lats
else
  ei=$(($si+$bz))
  # check for end index out of bounds
  if [ $ei -gt $num_lats ]; then
    ei=$num_lats
  fi
fi

# no work for processor to do
if [ $si -ge $num_lats ]; then
  echo "No jobs for processor to perform. Exiting . . ."
  exit 0
fi

# calculate lon0 offset of grid into global grid
if [ ${#delta_arr[@]} -eq 1 ]; then
  londelta=${delta_arr[0]}
elif [ ${#delta_arr[@]} -eq 2 ]; then
  londelta=${delta_arr[1]}
else
  echo "Wrong number of delta values. Exiting . . ."
  exit 0
fi
lon0_off=$(echo "60*($lon0+180)/$londelta" | bc)

# iterate over directories, filling in gaps
for ((i = $si; i < $ei; i++)); do
  # get latitude index
  grid1=(`echo ${dcts[$i]} | egrep -o [0-9]+`)
  echo "Processing latitude band "$grid1" . . ."

  # create files for each variable
  for var in ${var_names_arr[@]}; do
    touch var_files/$var"_"$grid1".txt"
  done

  # find all files in directory
  files=(`find $file_dir/$grid1 -name \*.psims.nc | grep '[0-9]/[0-9]' | sort`)

  # iterate over files, filling in gaps
  next_lon=1
  for ((j = 0; j < ${#files[@]}; j++)); do
    # get longitude index
    grid2=(`echo ${files[$j]} | egrep -o [0-9]+`)
    grid2=`echo ${grid2[1]} | sed 's/^0*//'` # remove leading zeros
    grid2=$(($grid2-$lon0_off))

    # insert missing longitudes, if necessary
    append_missing $next_lon $((grid2-1)) $grid1

    # iterate through variables, adding to files
    for var in ${var_names_arr[@]}; do
      # dump variable
      var_dump=`ncdump -v $var ${files[$j]}`

      # strip header and footer
      v=`echo $var_dump | sed "s/.*$var = \(.*\); }/\1/"`
      v=${v%?} # remove extra space

      # add to file
      if [ $grid2 -eq $num_lons ]; then
        echo -n $v >> var_files/$var"_"$grid1".txt" # no comma, no newline
      else
        echo $v", " >> var_files/$var"_"$grid1".txt" # comma, newline
      fi
    done

    # increment longitude index
    next_lon=$(($grid2+1))
  done

  # insert missing longitudes, if necessary
  append_missing $next_lon $num_lons $grid1
done
