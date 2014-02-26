Prerequisites
=============
The pSIMS application requires that you have several prerequisite packages needed
before running.

Package                  | Location | Type 
-------                  | -------- | ----
APSIM                    | http://www.apsim.info/ | Crop model
Boost                    | http://www.boost.org | Required to run APSIM
CenW                     | http://www.kirschbaum.id.au/Welcome_Page.htm | Generic forestry model
DSSAT                    | http://dssat.net | Crop model
Mono                     | http://www.mono-project.com | REquired to run APSIM
nco                      | http://nco.sourceforge.net | Required for postprocessing 
netcdf4                  | http://www.unidata.ucar.edu/software/netcdf | Required 
netcdf4 python libraries | http://code.google.com/p/netcdf4-python | Required
Oracle Java 7            | http://www.oracle.com/us/downloads/index.html | Required
Swift 0.95               | http://swiftlang.org | Required

How to Run
==========
The "psims" script is used to start a pSIMS run. The options you pass to 
this script will determine which pSIMS runs will be done (including which models) 
and where they will run.

Usage: psims [-s sitename] [-p paramfile] [-g gridlist]

The sitename option determines where a run will take place. Currently, valid 
options are "midway" and "local".

The params file defines the path to inputs, outputs, the type of model to run, and
what post processing steps need to happen.

The gridlist is a set of latitudes and longitudes that should be processed.


Params File Format
==================
The params file contains a set of keys and values defining the parameters of a psims
run. It defines things like the number of years to look at, the path name to climate
input files, and how to name the ouputs. Below is a list of all valid parameters and
a description of what it does.

Parameter    | Description                                                                                 | Example
---------    |-----------                                                                                  |-------
bintransfer  | Stage in the following binaries with each task, so RunpSIMS.sh has them available           | bintransfer bin/DSCSM045.EXE,pdssat/psims2WTH.py
campaign     | Directory containing campaign data                                                          | campaign /Users/davidk/psims/data/whe.demo
delta        | Gridcell spacing in arcminutes                                                              | delta 30
executable   | Name of executable and arguments to run for each grid                                       | executable DSCSM045.EXE:A:X1234567.WHX
lat\_zero     | Top edge of the North most grid cell in the campaign                                       | lat\_zero 90
lon\_zero     | Left edge of the West most grid cell in the campaign                                      | lon\_zero -180
long\_names   | Long names for variables, in same order that variables are listed                          | long\_names "PlantDate,AnthesisDate"
model        | Defines the type of model to run. Valid options are dssat45, apsim75, and cenw              | model dssat45
num\_lats     | Number of latitudes to be included in final nc4 file (starting with lat\_zero)             | num\_lats 360
num\_lons     | Number of longitudes to be included in final nc4 file (starting with lon\_zero)            | num\_lons 720
num\_years    | Number of years to simulate?                                                               | num\_years 31
out\_file     | Defines the prefix of the final nc4 filename (eg, $out\_file.nc4)                          | out\_file out.psims.dssat45.agmerra.wheat.demo
outtypes     | File extensions of files to include in output tar file                                      | outtypes .WTH,.WHX,.SOL,.OUT,.json,.txt
refdata      | Directory containing reference data. Will be copied to each simulation                      | refdata /Users/davidk/psims/data/common.isimip
ref\_year     | Reference year (the first year of the simulation)                                          | ref\_year 1980
scens        | Number of scenarios in the campaign                                                       | scens 8
soils        | Directory containing soils                                                                  | soils /Users/davidk/psims/data/soils/hwsd200.wrld.30min
tappcamp     | Campaign translator application and arguments (':' for space)                               | tappcamp camp2json.py:-c:Campaign.nc4
tappinp      | Input translator, goes from experiment.json and soil.json to model specific files      | tappinp jsons2dssat.py:-x:X1234567.WHX
tappwth      | Weather translater, converts .psims.nc format into model specfic weather files             | tappwth psims2WTH.py:-o:GENERIC1.WTH
postprocess  | Name of program and arguments to run after running executable                               | postprocess ./OUT2psims.py:-i:Summary.OUT
var\_units    | Units to use for each variable, in the same order that variables are listed               | var\_units "DOY,Days,Days,kg/ha,kg/ha,mm,mm,mm"
variables    | Define the variables to extract and format                                                  | variables PDAT,ADAT,MDAT,CWAM
weather      | Defines the directory where weather data is stored                                          | weather /Users/davidk/psims/data/agmerra

Gridlist Format
===============
A gridlist file contains a list of latitudes and longitudes to be processed, in the format of "lat/lon". Here is an example:

104/114
104/115
104/116

The latitude/longitude format is also appended to the weather and soils variables to determine the pathname to input 
files for a specific grid point. For example, suppose weather is set to /Users/davidk/psims/data/agmerra. For grid 
104/114, psims will include all files in the path: /Users/davidk/psims/data/agmerra/104/114/*.

It is important then, that for data exists in the soils and weather directory for each grid point. Missing data will 
result in errors.

Output Files
============
The output/ directory contains a directory for each latitude being processed. Within each latitude directory,
a tar.gz file exists for each longitude. For example, if your gridList contained a grid 100/546, you would see an
output file called runNNN/output/100/546output.tar.gz. This file is generated from within the Swift work directory.
Which files get included in the file is determined by how you set "outtypes" in your parameter file.

The parts/ directory contains the output nc files for each grid being processed. When grid 100/546 is done processing,
you will see a file called runNNN/parts/100/546.psims.nc.

The combined nc file is saved directly in the runNNN directory. Its name depends on the value of "out_file" in your
params file. If you set out_file to "out.psims.apsim75.cfsr.whea", the final combined nc file would be called "out.psims.apsim75.cfsr.whea.nc4".

How to Modify Swift Configuration
=================================
Determining how Swift runs is controlled by a file called conf/swift.properties. The swift.properties file defines the 
scheduler to use, the location of work and scratch directories, and controls parallelism. More information about 
swift.properties options can be found at http://swiftlang.org/guides/trunk/userguide/userguide.html#_configuration.

Debugging
=========
When problems occur, there are a few places to look to get answers about why the problems are occuring. The first is the standard output of
Swift. You will see this info on your screen as psims is running. Since there are many tasks running at once, it may scroll by your screen
too quickly. This output will also be recorded in runNNN/swift.out.

Another place to look is the runNNN/*.d directory. An info log file should exist in that directory for each failing task. The info file contains
the stdout and stderr output of RunpSIMS.sh. Each significant command should be logged with a timestamp so you can track the progress and get a
better idea of what's happening.

Midway
======
To run pSIMS on midway, the first thing you need to do is load the required modules.

$ module load java ant git mono/2.10 hdf5/1.8 nco/4.3 boost/1.50 netcdf/4.2 jasper python/2.7 cdo/1.6 tcllib/1.15 swift/0.95-RC5 

The conf/swift.properties file is configured to use the sandyb slurm partition. The sandyb partition has 16 cores per node. The default configuration
is to request nodes in chunks of 3, up to the Midway limit of 1536 total cores.

Running pSIMS jobs on Midway should be done from within the /scratch/midway filesystem.
Swift will use the /scratch/local filesystem on each node to perform the work, then transfer results back to your current working directory on /scratch/midway.
