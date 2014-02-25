Prerequisites
=============
The pSIMS application requires that you have several prerequisite packages needed
before running.

Package                  | Location
-------                  | --------
APSIM                    | http://www.apsim.info/
Boost                    | http://www.boost.org
CenW                     | http://www.kirschbaum.id.au/Welcome_Page.htm
DSSAT                    | http://dssat.net
Mono                     | http://www.mono-project.com
nco                      | http://nco.sourceforge.net
netcdf4                  | http://www.unidata.ucar.edu/software/netcdf
netcdf4 python libraries | http://code.google.com/p/netcdf4-python
Oracle Java 7            | http://www.oracle.com/us/downloads/index.html
Swift 0.95               | http://swiftlang.org

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

Output Files
============
Describe output files here.

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
delta        | Delta?                                                                                      | delta 30
executable   | Name of executable and arguments to run for each grid                                       | executable DSCSM045.EXE:A:X1234567.WHX
lat_zero     | Lat zero?                                                                                   | lat_zero 90
lon_zero     | Lon zero?                                                                                   | lon_zero -180
long_names   | Long names for variables, in same order that variables are listed                           | long_names "PlantDate,AnthesisDate"
model        | Defines the type of model to run. Valid options are dssat45, apsim75, and cenw              | model dssat45
num_lats     | Number of latitudes to be included in final nc4 file                                        | num_lats 360
num_lons     | Number of longitudes to be included in final nc4 file                                       | num_lons 720
num_years    | Number of years to simulate?                                                                | num_years 31
out_file     | Defines the prefix of the final nc4 filename (eg, $out_file.nc4)                            | out_file out.psims.dssat45.agmerra.wheat.demo
outtypes     | File extensions of files to include in output tar file                                      | outtypes .WTH,.WHX,.SOL,.OUT,.json,.txt
refdata      | Directory containing reference data. Will be copied to each simulation                      | refdata /Users/davidk/psims/data/common.isimip
ref_year     | Reference year                                                                              | ref_year 1980
scens        | Number of scenarios                                                                         | scens 8
soils        | Directory containing soils                                                                  | soils /Users/davidk/psims/data/soils/hwsd200.wrld.30min
tappcamp     | Campaign translator application and arguments (':' for space)                               | tappcamp forcamp2json.py:-c:Campaign.nc4
tappinp      | Translator app for for soils?                                                               | tappinp jsons2dssat.py:-x:X1234567.WHX
tappwth      | Defines the weather translater application and arguments to use (use : instead of spaces)   | tappwth psims2WTH.py:-o:GENERIC1.WTH
postprocess  | Name of program and arguments to run after running executable                               | postprocess ./OUT2psims.py:-i:Summary.OUT
var_units    | Units to use for each variable, in the same order that variables are listed                 | var_units "DOY,Days,Days,kg/ha,kg/ha,mm,mm,mm"
variables    | Define the variables to analyze                                                             | variables PDAT,ADAT,MDAT,CWAM
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
