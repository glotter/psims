#!/bin/bash

: '
This script aggregates multiple single-gridpoint netcdf files into one big spatial
raster netcdf file. The usage is:

  ./append.sh [var_names] [long_names] [units] [num_lons] [num_lats] [delta] [num_years]
              [num_scenarios] [ref_year] [file_dir] [out_file]

where the input arguments are as follows:

var_names: List of variable names extracted from netcdf files
long_names: List of long descriptive names corresponding to var_names
units: List of units corresponding to var_names
num_lons: Number of longitude points in spatial raster
num_lats: Number of latitude points in spatial raster
delta: Distance between grid points in arcminutes
num_years: Number of years in netcdf files
num_scenarios: Number of scenarios in netcdf files
ref_year: Reference year for times in netcdf files
lat0: Latitude of grid origin
lon0: Longitude of grid origin
file_dir: Directory from which to search for netcdf files, up to depth of two
out_file: Name of output file

Example:
  ./append.sh PDAT "Planting date" YrDoy 2 2 30 44 6 1958 90 -180 . out.psims.nc4

NOTE:
  The final dimensions of the output file are: latitude, scenario, longitude, time, in that order. To make "time" the lead dimension perform the following operation on the output file:

  ncpdq -O -h -a time,latitude out.psims.nc4 out.psims.nc4
'

# ============
# CREATE BLANK
# ============
create_blank() {
  local lon=(${!1})
  local lat=(${!2})
  local permute=$3
  local out_file=$4

  # latitutde
  local num_lats=${#lat[@]}
  lat=$(printf ",%s" "${lat[@]}")
  lat=${lat:1}

  # longitude
  local num_lons=${#lon[@]}
  lon=$(printf ",%s" "${lon[@]}")
  lon=${lon:1}

  # time
  local time=($(seq 1 $num_years))
  time=$(printf ",%s" "${time[@]}")
  time=${time:1}

  # scenario
  local scenario=($(seq 1 $num_scenarios))
  scenario=$(printf ",%s" "${scenario[@]}")
  scenario=${scenario:1}

  # variables
  local IFS=',' # change file separator
  local var_names_arr=($var_names)
  local long_names_arr=($long_names)
  local units_arr=($units)
  local permute_arr=($permute)
  local num_vars=${#var_names_arr[@]}
  local vars=""
  local i
  for ((i = 0; i < $num_vars; i++)); do
    vars=$vars"\tfloat "${var_names_arr[i]}"("
    vars=$vars${permute_arr[0]}", "
    vars=$vars${permute_arr[1]}", "
    vars=$vars${permute_arr[2]}", "
    vars=$vars${permute_arr[3]}") ;\n"
    vars=$vars"\t\t"${var_names_arr[i]}":units = "\"${units_arr[i]}\"" ;\n"
    vars=$vars"\t\t"${var_names_arr[i]}":long_name = "\"${long_names_arr[i]}\"" ;\n"
    vars=$vars"\t\t"${var_names_arr[i]}":_FillValue = 1.e+20f ;\n"
  done

  # data
  local data="data:\n"
  data=$data" latitude = $lat ;\n"
  data=$data" longitude = $lon ;\n"
  data=$data" time = $time ;\n"
  data=$data" scenario = $scenario ;\n}"

  # write file
  echo -e "netcdf blank {\ndimensions:" > blank.cdl

  first_dim=${permute_arr[0]}  
  if [ $first_dim == "longitude" ]; then
    echo -e "\tlongitude = UNLIMITED ;" >> blank.cdl
  else
    echo -e "\tlongitude = $num_lons ;" >> blank.cdl
  fi
  if [ $first_dim == "latitude" ]; then
    echo -e "\tlatitude = UNLIMITED ;" >> blank.cdl
  else
    echo -e "\tlatitude = $num_lats ;" >> blank.cdl
  fi
  if [ $first_dim == "time" ]; then
    echo -e "\ttime = UNLIMITED ;" >> blank.cdl
  else
    echo -e "\ttime = $num_years ;" >> blank.cdl
  fi
  if [ $first_dim == "scenario" ]; then
    echo -e "\tscenario = UNLIMITED ;" >> blank.cdl
  else
    echo -e "\tscenario = $num_scenarios ;" >> blank.cdl
  fi

  echo -e "variables:" >> blank.cdl
  echo -e "\tfloat longitude(longitude) ;" >> blank.cdl
  echo -e "\t\tlongitude:units = \"degrees_east\" ;" >> blank.cdl
  echo -e "\t\tlongitude:long_name = \"longitude\" ;" >> blank.cdl
  echo -e "\tfloat latitude(latitude) ;" >> blank.cdl
  echo -e "\t\tlatitude:units = \"degrees_north\" ;" >> blank.cdl
  echo -e "\t\tlatitude:long_name = \"latitude\" ;" >> blank.cdl
  echo -e "\tint time(time) ;" >> blank.cdl
  echo -e "\t\ttime:units = \"growing seasons since "$ref_year"-01-01 00:00:00\" ;" >> blank.cdl
  echo -e "\t\ttime:long_name = \"time\" ;" >> blank.cdl
  echo -e "\tint scenario(scenario) ;" >> blank.cdl
  echo -e "\t\tscenario:units = \"no\" ;" >> blank.cdl
  echo -e "\t\tscenario:long_name = \"scenario\" ;" >> blank.cdl
  echo -e "$vars" >> blank.cdl
  echo -e "$data" >> blank.cdl

  ncgen -k1 -o $out_file blank.cdl
  rm blank.cdl # remove temporary cdl file
}

# ==============
# INSERT MISSING
# ==============
insert_missing() {
  local lat_idx1=$1
  local lat_idx2=$2
  local lon_idx1=$3
  local lon_idx2=$4
  local permute=$5
  local save=$6
  local in_file=$7

  # boundary conditions
  if [ $lat_idx2 == "end" ]; then
    lat_idx2=$num_lats
  fi
  if [ $lon_idx2 == "end" ]; then
    lon_idx2=$num_lons
  fi

  # missing latitudes
  local msg_lat
  if [ $lat_idx1 == $lat_idx2 ]; then
    # single point
    msg_lat[0]=${lat[$lat_idx1]}
  else
    local i
    for ((i = $lat_idx1; i < $lat_idx2; i++)); do
      msg_lat[$(($i-$lat_idx1))]=${lat[$i]}
    done
  fi

  # missing longitudes
  local msg_lon
  if [ $lon_idx1 == $lon_idx2 ]; then
    # single point
    msg_lon[0]=${lon[$lon_idx1]}
  else
    local i
    for ((i = $lon_idx1; i < $lon_idx2; i++)); do
      msg_lon[$(($i-$lon_idx1))]=${lon[$i]}
    done
  fi

  # create blank file for missing data
  create_blank msg_lon[@] msg_lat[@] "$permute" blank.nc
  
  if [ $save == "create" ]; then
    mv -f blank.nc $in_file
  elif [ $save == "append" ]; then
    ncrcat --no_tmp_fl -h $in_file blank.nc $in_file.tmp &> /dev/null
    mv -f $in_file.tmp $in_file
    rm blank.nc
  fi
}

# ===========
# APPEND FILE
# ===========
append_file() {
  local file=$1
  local file_to_append=$2
  ncrcat --no_tmp_fl -h $file $file_to_append $file.tmp &> /dev/null
  mv -f $file.tmp $file
}

# ===========
# PERMUTE DIM
# ===========
permute_dim() {
  local dim1=$1
  local dim2=$2
  local file=$3
  ncpdq --no_tmp_fl -h -a $dim1,$dim2 $file $file.tmp &> /dev/null
  mv -f $file.tmp $file
}

# read inputs from command line
var_names=$1
long_names=$2
units=$3
num_lons=$4
num_lats=$5
delta=$6
num_years=$7
num_scenarios=$8
ref_year=$9
lat0=${10}
lon0=${11}
file_dir=${12}
out_file=${13}

# longitude
for ((i = 1; i <= $num_lons; i++)); do
  lon[$(($i-1))]=$(echo "scale=15;$lon0+$delta/60*($i-0.5)" | bc)
done

# latitude
for ((i = 1; i <= $num_lats; i++)); do
  lat[$(($i-1))]=$(echo "scale=15;$lat0-$delta/60*($i-0.5)" | bc)
done

# calculate lat0 and lon0 offsets of grid into global grid
lat0_off=`printf "%.0f" $(echo "scale=15;60*(90-$lat0)/$delta+1" | bc)`
lon0_off=`printf "%.0f" $(echo "scale=15;60*($lon0+180)/$delta+1" | bc)`

# find all directories
dcts=(`ls -d $file_dir/*/ | grep '[0-9]'`)

# iterate over directories, filling in gaps
for ((i = 0; i < ${#dcts[@]}; i++)); do
  # get latitude index
  grid1_orig=(`echo ${dcts[$i]} | egrep -o [0-9]+`)
  grid1=`echo $grid1_orig | sed 's/^0*//'` # remove leading zeros
  lat_idx=$(($grid1-$lat0_off))

  # temporary file
  sum_lon_file=$grid1.nc

  echo "Processing latitude band "$grid1" . . ."

  # find all files in directory
  files=(`find $file_dir/$grid1_orig -name \*.psims.nc | grep '[0-9]/[0-9]' | sort`)

  # iterate over files, filling in gaps
  for ((j = 0; j < ${#files[@]}; j++)); do
    # get longitude index
    grid2=(`echo ${files[$j]} | egrep -o [0-9]+`)
    grid2=`echo ${grid2[1]} | sed 's/^0*//'` # remove leading zeros
    lon_idx=$(($grid2-$lon0_off))

    # temporary file
    lat_lon_file=$grid1"_"$grid2.nc

    # make longitude unlimited
    ncpdq --no_tmp_fl -h -a longitude,time ${files[$j]} $lat_lon_file &> /dev/null

    if [ $j == 0 ]; then
      # first file
      if [ $lon_idx -gt 0 ]; then
        # insert missing longitudes at beginning
        insert_missing $lat_idx $lat_idx 0 $lon_idx "longitude,scenario,latitude,time" "create" $sum_lon_file
        # append
        append_file $sum_lon_file $lat_lon_file
      else
        # copy file
        cp $lat_lon_file $sum_lon_file
      fi
    else
      # subsequent files
      if [ $next_lon_idx -lt $lon_idx ]; then
        # insert missing longitudes
        insert_missing $lat_idx $lat_idx $next_lon_idx $lon_idx "longitude,scenario,latitude,time" "append" $sum_lon_file
      fi
      # append
      append_file $sum_lon_file $lat_lon_file
    fi

    # increment longitude index
    next_lon_idx=$(($lon_idx+1))

    # remove temporary file
    rm $lat_lon_file
  done

  if [ $next_lon_idx -lt $num_lons ]; then
    # insert missing longitudes at end
    insert_missing $lat_idx $lat_idx $next_lon_idx end "longitude,scenario,latitude,time" "append" $sum_lon_file
  fi

  # make latitude unlimited
  permute_dim "latitude" "longitude" $sum_lon_file

  if [ $i == 0 ]; then
    # first directory
    if [ $lat_idx -gt 0 ]; then
      # insert missing latitudes at beginning
      insert_missing 0 $lat_idx 0 end "latitude,scenario,longitude,time" "create" $out_file
      # append
      append_file $out_file $sum_lon_file
    else
      # copy file
      cp $sum_lon_file $out_file
    fi
  else
    # subsequent directories
    if [ $next_lat_idx -lt $lat_idx ]; then
      # insert missing latitudes
      insert_missing $next_lat_idx $lat_idx 0 end "latitude,scenario,longitude,time" "append" $out_file
    fi
    # append
    append_file $out_file $sum_lon_file
  fi

  # increment latitude index
  next_lat_idx=$(($lat_idx+1))

  # remove temporary file
  rm $sum_lon_file
done

if [ $next_lat_idx -lt $num_lats ]; then
  # insert missing longitudes at end
  insert_missing $next_lat_idx end 0 end "latitude,scenario,longitude,time" "append" $out_file
fi

# add global attribute note
ncatted -O -h -a note,global,c,c,"To make time lead dimension run: ncpdq -O -h -a time,latitude <out_file> <out_file>" $out_file

# THIS TAKES A LONG TIME!
# echo "Resetting . . ."
# permute_dim "time" "latitude" $out_file

# upconvert and compress
nccopy -k4 -d9 $out_file $out_file.tmp
mv -f $out_file.tmp $out_file
