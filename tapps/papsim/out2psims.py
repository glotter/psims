#!/usr/bin/env python

# import modules
import os, datetime
from netCDF4 import Dataset as nc
from optparse import OptionParser
from numpy import array, empty, asarray, double

# parse inputs
parser = OptionParser()
parser.add_option("-i", "--input", dest = "inputfile", default = "data/Generic.out", type = "string",
                  help = "Name of (first) APSIM out file to parse", metavar = "FILE")
parser.add_option("-o", "--output", dest = "outputfile", default = "Generic.psims.nc", type = "string",
                  help = "Output pSIMS netCDF3 file", metavar = "FILE")
parser.add_option("-s", "--num_scenarios", dest = "num_scenarios", default = 1, type = "int",
                  help = "Number of scenarios to process")
parser.add_option("-v", "--variables", dest = "variables", default = "", type = "string",
                  help = "Comma-separated list (with no spaces) of variables to process")
parser.add_option("-d", "--delta", dest = "delta", default = 1, type = "float",
                  help = "Distance between each grid cell in arcminutes")
parser.add_option("-r", "--ref_year", dest = "ref_year", default = 1958, type = "int",
                  help = "Reference year from which to record times")                          
parser.add_option("--grid1", dest = "grid1", default = 1, type = "string",
                  help = "Latitude coordinate")
parser.add_option("--grid2", dest = "grid2", default = 1, type = "string",
                  help = "Longitude coordinate")
(options, args) = parser.parse_args()

# get out files(s)
num_scenarios = options.num_scenarios
basename, fileext = os.path.splitext(options.inputfile)
outfiles = [''] * num_scenarios
for i in range(num_scenarios):
    outfiles[i] = options.inputfile if not i else basename + str(i) + fileext

# get variables
variables = array(options.variables.split(',')) # split variable names
grid1 = int(options.grid1)
grid2 = int(options.grid2)
delta = options.delta / 60. # convert from arcminutes to degrees

# get number of variables
num_vars = len(variables)

# compute latitude and longitude
lat = 90. - delta * (grid1 - 0.5)
lon = -180. + delta * (grid2 - 0.5)

# get reference time
ref_date = datetime.datetime(options.ref_year, 1, 1)

# load data and find number of times
data = [0] * num_scenarios
for i in range(num_scenarios):
    data[i] = [l.split() for l in tuple(open(outfiles[i]))]
    nt = len(data[i]) - 4 # minus 4 for header information
    if not i:
        num_times = nt
    else:
        if nt != num_times:
            raise Exception('Number of times not consistent between out files')

# create pSIMS NetCDF3 file
dirname = os.path.dirname(options.outputfile)
if dirname and not os.path.exists(dirname):
    raise Exception('Directory to output file does not exist')
root_grp = nc(options.outputfile, 'w', format = 'NETCDF3_CLASSIC')

# add latitude and longitude
root_grp.createDimension('longitude', 1)
root_grp.createDimension('latitude', 1)
lon_var = root_grp.createVariable('longitude', 'f8', ('longitude',))
lon_var[:] = lon
lon_var.units = 'degrees_east'
lon_var.long_name = 'longitude'
lat_var = root_grp.createVariable('latitude', 'f8', ('latitude',))
lat_var[:] = lat
lat_var.units = 'degrees_north'
lat_var.long_name = 'latitude'

# create time and scenario dimensions
root_grp.createDimension('time', None)
root_grp.createDimension('scenario', num_scenarios)

# add time and scenario variables
time_var = root_grp.createVariable('time', 'i4', 'time')
time_var[:] = range(1, num_times + 1)
time_var.units = 'growing seasons since {:s}'.format(str(ref_date))
time_var.long_name = 'time'
scenario_var = root_grp.createVariable('scenario', 'i4', 'scenario')
scenario_var[:] = range(1, num_scenarios + 1)
scenario_var.units = 'no'
scenario_var.long_name = 'scenario'

# iterate through scenarios
var_data = empty((num_times, num_scenarios, num_vars))
for i in range(num_scenarios):
    # search for variables within list of all variables
    all_variables = data[i][2]
    variable_idx = []
    for v in variables:
        if not v in all_variables:
            raise Exception('Variable {:s} not in out file'.format(v))
        else:
            variable_idx.append(all_variables.index(v))

    # remove header, select variables, and convert to numpy array of doubles
    var_data[:, i, :] = asarray(data[i][4 :])[:, variable_idx].astype(double)

    all_units = array([v.strip('()') for v in data[i][3]]) # remove parentheses
    if not i:
        # pull units from first out file
        units = all_units[variable_idx]

# add data
for i in range(num_vars):
    var = root_grp.createVariable(variables[i], 'f4', ('time', 'scenario', 'latitude', 'longitude',))
    var[:] = var_data[:, :, i]
    var.units = units[i]
    var.long_name = variables[i]

# close file
root_grp.close()