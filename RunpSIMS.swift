type file;

app RunpSIMS (string weather_in, string soils_in, string common_in, string latidx, string lonidx, string tar_out, string part_out)
{
   bash "-c" @strcat("dd if=", @arg("run_directory"), "/RunpSIMS.sh of=RunpSIMS.sh bs=16M ; chmod +x ./RunpSIMS.sh ; ./RunpSIMS.sh ", 
                     tar_out, " ", 	  	 // $1
                     @arg("executable"), " ",    // $2
                     @arg("outtypes"), " ",      // $3
                     @arg("postprocess"), " ",   // $4
                     @arg("tappwth"), " ",       // $5
                     @arg("tappinp"), " ",       // $6
                     @arg("tappcamp"), " ",      // $7
                     latidx, " ",                // $8
                     lonidx, " ",                // $9
                     @arg("model"), " ",         // $10
                     @arg("ref_year"), " ",      // $11
                     @arg("delta"), " ",         // $12
                     @arg("num_years"), " ",     // $13
                     @arg("scens"), " ",         // $14
                     @arg("variables"), " ",     // $15
                     @arg("long_names"), " ",    // $16
                     @arg("var_units"), " ",     // $17
                     @arg("num_lats"), " ",      // $18
                     @arg("num_lons"), " ",      // $19
                     @arg("lat_zero"), " ",      // $20
                     @arg("lon_zero"), " ",      // $21
                     part_out, " ",              // $22
                     common_in, " ",             // $23
                     @arg("pid"), " ",           // $24
                     @weather_in, " ", 
                     @soils_in);
}

string gridLists[] = readData("gridList.txt");
   
// Common input files for all grids
file scenario_input[] <filesys_mapper; location=@arg("campaign"), pattern="*">;
file common_input[] <filesys_mapper; location=@arg("refdata"), pattern="*">;	
file binary_input[] <fixed_array_mapper; files=@arg("bintransfer")>; 
string common_files = @strcat("\"", @scenario_input, " ", @common_input, " ", @binary_input, "\"");

foreach g,i in gridLists {

   string gridNames[] = @strsplit(g, "/");

   // Unique input files for each grid
   file weather_input[] <filesys_mapper; location=@strcat(@arg("weather"), "/", gridLists[i]), pattern="*">; 
   file soils_input[] <filesys_mapper; location=@strcat(@arg("soils"), "/", gridLists[i]), pattern="*">; 
 
   // Output files
   string tar_output = @strcat(@arg("run_directory"), "/output/", gridLists[i], "output.tar.gz");
   string part_output = @strcat(@arg("run_directory"), "/parts/", gridLists[i], ".psims.nc");
   RunpSIMS(@weather_input, @soils_input, common_files, gridNames[0], gridNames[1], tar_output, part_output);

}
