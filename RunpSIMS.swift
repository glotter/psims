type file;

app (file tar_out, file part_out) RunpSIMS (file scenario_in[], file weather_in[], file soils_in[], file common_in[], file binary_in[], file wrapper_file, string latidx, string lonidx)
{
   bash "-c" @strcat("chmod +x ./RunpSIMS.sh ; ./RunpSIMS.sh ", @tar_out, " ",
                     @arg("executable"), " ", @arg("outtypes"), " ", @arg("postprocess"), " ", 
                     @arg("tappwth"), " ", @arg("tappinp"), " ", @arg("tappcamp"), " ", 
                     latidx, " ", lonidx, " ", @arg("model"), " ", @arg("ref_year"), " ", 
                     @arg("delta"), " ", @arg("num_years"), " ", @arg("scens"), " ", 
                     @arg("variables"), " ", @arg("long_names"), " ", @arg("var_units"), " ", 
                     @arg("num_lats"), " ", @arg("num_lons"), " ",
                     @arg("lat_zero"), " ", @arg("lon_zero"), " ", @arg("out_file"), " ",
                     @scenario_in, " ", @weather_in, " ", @soils_in, " ", @common_in, " ", @binary_in);
}

string gridLists[] = readData("gridList.txt");

foreach g,i in gridLists {

string gridNames[] = @strsplit(g, "/");

   // Input files
   file scenario_input[] <filesys_mapper; location=@arg("campaign"), pattern="*">;
   file weather_input[] <filesys_mapper; location=@strcat(@arg("weather"), "/", gridLists[i]), pattern="*">; 
   file soils_input[] <filesys_mapper; location=@strcat(@arg("soils"), "/", gridLists[i]), pattern="*">; 
   file common_input[] <filesys_mapper; location=@arg("refdata"), pattern="*">;	
   file binary_input[] <fixed_array_mapper; files=@arg("bintransfer")>; 
   file wrapper_input <single_file_mapper; file="RunpSIMS.sh">; 

   // Output files
   file tar_output <single_file_mapper; file=@strcat("output/", gridLists[i], "output.tar.gz")>;
   file part_output <single_file_mapper; file=@strcat("parts/", gridLists[i], ".psims.nc")>;

   (tar_output, part_output) = RunpSIMS(scenario_input, weather_input, soils_input, common_input, binary_input, wrapper_input, gridNames[0], gridNames[1]);
}
