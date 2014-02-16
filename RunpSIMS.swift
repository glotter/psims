type file;

app (file tar_out, file part_out) RunpSIMS (file scenario[], file weather[], file soils[], file common[], file binary[], 
                                            string latidx, string lonidx, file param)
{
   RunpSIMS latidx lonidx @param @tar_out;
}

app (file outfile) append (file inputArray[], file param) {
   append 1 1 @param "./parts" @outfile;
}

app (file outfile) merge (int var_num, file inputArray[], file param) {
   merge var_num "1" "./var_files" @param;
}

app (file outfile) combine (file inputArray[], file scenarioInput[], file param) {
   combine @param;
}

string gridLists[] = readData("gridList.txt");
file part_outputs[][];
file scenario_input[] <filesys_mapper; location=@arg("campaign"), pattern="*">;
file common_input[] <filesys_mapper; location=@arg("refdata"), pattern="*">;	
file binary_input[] <fixed_array_mapper; files=@arg("bintransfer")>; 
file params <"params.psims">;

foreach g,i in gridLists {

   string gridNames[] = @strsplit(g, "/");
   int lat = @toInt(gridNames[0]);
   int lon = @toInt(gridNames[1]);

   // Input files
   file weather_input[] <filesys_mapper; location=@strcat(@arg("weather"), "/", gridLists[i]), pattern="*">; 
   file soils_input[] <filesys_mapper; location=@strcat(@arg("soils"), "/", gridLists[i]), pattern="*">; 

   // Output files
   file tar_output <single_file_mapper; file=@strcat("output/", gridLists[i], "output.tar.gz")>;
   file part_output <single_file_mapper; file=@strcat("parts/", gridLists[i], ".psims.nc")>;

   // Run pSIMS
   (tar_output, part_output) = RunpSIMS(scenario_input, weather_input, soils_input, common_input, binary_input, gridNames[0], gridNames[1], params);
   part_outputs[lat][lon] = part_output;

}

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
