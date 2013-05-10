type file;

app (file tar_out, file part_out, file outlog, file errlog) RunpSIMS (file scenario_in[], file weather_in[], file common_in[], file binary_in[], file wrapper_file)
{
   bash "-c" @strcat("chmod +x ./RunpSIMS.sh ; ./RunpSIMS.sh ", @tar_out, " ",
                     @arg("executable"), " ", @arg("outtypes"), " ", @arg("postprocess"), " ", @arg("tapp"), " ",
                     @arg("model"), " ", @scenario_in, " ", @weather_in, " ", @common_in, " ", @binary_in) 
                     stdout=@outlog stderr=@errlog;
}

string gridLists[] = readData("gridList.txt");

foreach g,i in gridLists {

   // Input files
   file scenario_input[] <filesys_mapper; location=@strcat(@arg("scenarios"), "/", gridLists[i]), pattern="*">; 
   file weather_input[] <filesys_mapper; location=@strcat(@arg("weather"), "/", gridLists[i]), pattern="*">; 
   file common_input[] <filesys_mapper; location=@arg("refdata"), pattern="*">;	
   file binary_input[] <fixed_array_mapper; files=@arg("bintransfer")>; 
   file wrapper_input <single_file_mapper; file="RunpSIMS.sh">; 

   // Output files
   file stdout_output <single_file_mapper; file=@strcat("logs/", gridLists[i], "o.log")>;
   file stderr_output <single_file_mapper; file=@strcat("logs/", gridLists[i], "e.log")>;
   file tar_output <single_file_mapper; file=@strcat("output/", gridLists[i], "output.tar.gz")>;
   file part_output <single_file_mapper; file=@strcat("parts/", gridLists[i], ".part")>;

   (tar_output, part_output, stdout_output, stderr_output) = RunpSIMS(scenario_input, weather_input, common_input, binary_input, wrapper_input);
}
