type file;

app (file outfile) append (file inputArray[], file param) {
   append 1 1 @param "./parts" @outfile;
}

app (file outfile) merge (int var_num, file inputArray[], file param) {
   merge var_num "1" "./var_files" @param;
}

app (file outfile) combine (file inputArray[], file scenarioInput[], file param) {
   combine @param;
}

file params <single_file_mapper; file=@strcat(@arg("workdir"), "/params.psims")>;
file scenario_input[] <filesys_mapper; location=@arg("campaign"), pattern="*">;
file part_outputs[][] <ext; exec="../bin/findParts.sh" >;

# Append
file lat_outs[];
foreach latitude,latval in part_outputs {
   file latout <single_file_mapper; file=@strcat("lat", latval, ".tar.gz")>;
   latout = append(part_outputs[latval], params);
   lat_outs[latval] = latout;
}

# Merge
string variable_array[];
variable_array = @strsplit(@arg("variables"), ",");
file var_ncs[];
foreach v,i in variable_array {
   file var_nc <single_file_mapper; file=@strcat(@arg("out_file"), ".", v, ".nc4")>;
   var_nc = merge(i+1, lat_outs, params);
   var_ncs[i] = var_nc;
}

# Combine 
file nc <single_file_mapper; file=@strcat(@arg("out_file"), ".nc4")>;
nc = combine(var_ncs, scenario_input, params);
