#!/bin/bash

# Usage: combine_ascii.sh input_dir params
input_dir=$1
params=$2
echo input_dir: $input_dir
echo params: $params

source $params

OLD_IFS=$IFS
IFS=',' 
var_names_arr=($variables)
num_vars=${#var_names_arr[@]}
IFS=$OLD_IFS 

find $PWD -mindepth 2 -type f -exec ln -s {} \;
find $PWD -mindepth 2 -type l -exec ln -s {} \;

psims_files=($( ls $input_dir/$out_file* ))
first_file=${psims_files[0]}
final_file=$out_file".nc4"
cp $first_file $final_file

for ((i = 1; i < $num_vars; i++)); do
   echo "Appending "${var_names_arr[$i]}" ..." 
   ncks -A -h ${psims_files[$i]} $final_file
done

echo "Appending campaign file..."
ncks -A -h Campaign.nc4 $final_file
