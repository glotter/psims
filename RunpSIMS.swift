type file;

app (file tar_out, file part_out, file outlog, file errlog) RunpSIMS (file input1[], file input2[], file input3[], file input4[], file wrapper_file)
{
   bash "-c" @strcat("chmod +x ./RunpSIMS.sh ; ./RunpSIMS.sh ", @tar_out, " ",
                     @arg("executable"), " ", @arg("outtypes"), " ", @arg("postprocess"), " ", @arg("model"), " ",
                     @input1, " ", @input2, " ", @input3, " ", @input4) stdout=@outlog stderr=@errlog;
}

string gridLists[] = readData("gridList.txt");

foreach g,i in gridLists {
   file tar_output <single_file_mapper; file=@strcat("output/", gridLists[i], "output.tar.gz")>;
   file part_output <single_file_mapper; file=@strcat("parts/", gridLists[i], ".part")>;

   file in1[] <filesys_mapper; location=@strcat(@arg("scenarios"), "/", gridLists[i]), pattern="*">; // Scenario files
   file in2[] <filesys_mapper; location=@strcat(@arg("weather"), "/", gridLists[i]), pattern="*">;    // Weather files
   file in3[] <filesys_mapper; location=@arg("refdata"), pattern="*">;				      // Common data
   file in4[] <fixed_array_mapper; files=@arg("bintransfer")>;                                        // Binaries
   file wrapper <single_file_mapper; file="RunpSIMS.sh">;                                             // RunpSIMS wrapper
   file o <single_file_mapper; file=@strcat("logs/", gridLists[i], "o.log")>;
   file e <single_file_mapper; file=@strcat("logs/", gridLists[i], "e.log")>;

   (tar_output, part_output, o, e) = RunpSIMS(in1, in2, in3, in4, wrapper);
}
