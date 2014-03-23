type file;

app (external e) append (string lat, string var, string inputDir, string outputDir, string param) {
   append lat var inputDir outputDir param;
}

app (external e) merge (external[string][string] varsDone, int varNum, string fileDir, string outDir, string param) {
   merge varNum "1" fileDir outDir param;
}

app (file outfile) combine (external inputs[], file scenarioInput[], string varDirectory, string param) {
   combine varDirectory param;
}

app (file output) findParts (string workDir) {
   findParts workDir stdout=@output;
}

file scenarios[] <filesys_mapper; location=@arg("campaign"), pattern="*">;

string params = @strcat(@arg("workdir"), "/params.psims");
string variables[] = @strsplit(@arg("variables"), ",");
string lats[] = readData(findParts(@arg("workdir"))); 
string partDir = @strcat(@arg("workdir"), "/parts");
string varDir = @strcat(@arg("workdir"), "/var_files");

external varsCompleted[string][string];
external varNcs[];

# Append
tracef("\nRunning appends... %k\n", lats);
foreach lat in lats {
   foreach var in variables {
      varsCompleted[lat][var] = append(lat, var, partDir, varDir, params);
   }
}

# Merge
tracef("\nRunning merge...%k\n", varsCompleted);
foreach v,i in variables {
   varNcs[i] = merge(varsCompleted, i+1, varDir, @arg("workdir"), params);
}

# Combine 
tracef("\nRunning combine...%k\n", varNcs);
file finalNC4 <single_file_mapper; file=@strcat(@arg("out_file"), ".nc4")>;
finalNC4 = combine(varNcs, scenarios, @arg("workdir"), params);

