Prerequisites
=============
The pSIMS application requires that you have several prerequisite packages needed
before running.

Package                  | Location
-------                    --------
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

Params File Format
==================
Params file format and options here.

Gridlist Format
===============
Gridlist info here.

How to Modify Swift Configuration
=================================
Info about swift.properties here.

Debugging
=========
Information about how to debug when things go wrong.

Midway
======
Midway specific setup (location of APSIM tar, DSCM75 tar, etc). Information about slurm partitions, QoS limits, scratch filesystems.

