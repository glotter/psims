#!/usr/bin/env python

# import modules
import re
import csv
import datetime
from netCDF4 import Dataset as nc
from optparse import OptionParser
from numpy import array, double, zeros

# parse inputs
parser = OptionParser()
parser.add_option("-i", "--input", dest = "inputfile", default = "Generic.csv", type = "string",
                  help = "Weather CSV file to parse", metavar = "FILE")
parser.add_option("-o", "--output", dest = "outputfile", default = "Generic.psims.nc", type = "string",
                  help = "Output pSIMS netCDF3 file", metavar = "FILE")
(options, args) = parser.parse_args()

# open and parse csv file
data = []
with open(options.inputfile) as f:
    for row in csv.reader(f, delimiter = '\t'):
        data.append(row)

# get variable indices
psims_vars = ['tmin', 'tmax', 'precip', 'solar', 'wind']
csv_vars = ['TempMin', 'TempMax', 'Precipitation', 'Radiation', 'Windspeed']
units = ['C', 'C', 'mm', 'MJ/m^2/day', 'km day-1']
long_names = ['Daily minimum temperature', 'Daily maximum temperature', \
              'Daily total precipitation', 'Daily average downward short-wave radiation flux', \
              'Windspeed at 10 meter']

# strip header and put in array
header = data[0]
varmat = array(data[1 :])

# get time
time_idx = header.index('Date')
time_units = 'days since ' + varmat[0, time_idx] + ' 00:00:00'
fst_time = varmat[0, time_idx].split('-')
fst_time = datetime.date(int(fst_time[0]), int(fst_time[1]), int(fst_time[2]))
time = zeros((len(varmat),))
for i in range(len(time)):
    ith_time = varmat[i, time_idx].split('-')
    ith_time = datetime.date(int(ith_time[0]), int(ith_time[1]), int(ith_time[2]))
    time[i] = (ith_time - fst_time).days

# get grid cell
grd_idx = header.index('Gridcell')
p = re.compile('\d+')
grd = p.findall(varmat[0, grd_idx])
lat_idx = int(grd[1]) + 4216
lon_idx = int(grd[0]) + 22305
lat = 90 - 30. / 3600 * (lat_idx - 0.5)
lon = -180 + 30. / 3600 * (lon_idx - 0.5)

# create pSIMS NetCDF3 file
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

# add time
root_grp.createDimension('time', None)
time_var = root_grp.createVariable('time', 'i4', 'time')
time_var[:] = time
time_var.units = time_units
time_var.long_name = 'time'

# add other variables
for i in range(len(psims_vars)):
    var = root_grp.createVariable(psims_vars[i], 'f4', ('time', 'latitude', 'longitude',))
    val = varmat[:, header.index(csv_vars[i])].astype(double)
    if csv_vars[i] == 'Radiation':
        # convert solar
       val *= 0.001 # kJ/m^2/day -> MJ/m^2/day
    elif csv_vars[i] == 'Windspeed':
       val *= 86.4 # m/s -> km/day
    var[:] = val
    var.units = units[i]
    var.long_name = long_names[i]

# close file
root_grp.close()