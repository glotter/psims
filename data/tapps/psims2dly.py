#!/usr/bin/env python

# import modules
from netCDF4 import Dataset as nc
from optparse import OptionParser
import os, stat, datetime, csv, re
from collections import OrderedDict as od
from numpy import zeros, array, concatenate, savetxt, double, logical_and

# search for patterns in variable list
def isin(var, varlist):
    vararr = array(varlist)
    patt = re.compile(var + '_*')
    matches = array([bool(patt.match(v)) for v in vararr])
    return list(vararr[matches])

# parse inputs
parser = OptionParser()
parser.add_option("-i", "--input", dest = "inputfile", default = "Generic.psims.nc", type = "string", 
                  help = "NetCDF3 file to parse", metavar = "FILE")
parser.add_option("-v", "--variables", dest = "variables", default = "tmin,tmax,pr,rsds", type = "string",
                  help = "Comma-separated list of variables to parse", metavar = "FILE")
parser.add_option("-c", "--conc", dest = "conc", default = "NASA.CO2.Annual.1850-2013.csv", type = "string",
                  help = "CSV file containing annual CO2 concentration", metavar = "FILE")
parser.add_option("-o", "--output", dest = "outputfile", default = "Generic.met", type = "string",
                  help = "Output met file", metavar = "FILE")
(options, args) = parser.parse_args()

# open netcdf file for reading
infile = nc(options.inputfile, 'r', format = 'NETCDF3_CLASSIC')

# variable list
variables = options.variables.split(',')

# only CSV file
csvfile = csv.reader(open(options.conc))
csvdata = []
for row in csvfile:
    csvdata.append(row)
csvdata = array(csvdata[2 :])
csvyears = csvdata[:, 0].astype(int)
csvco2 = csvdata[:, 1].astype(double)

# get time
vlist = infile.variables.keys()
if 'time' in vlist: # make sure time is in file
    time = infile.variables['time'][:]
    time_units = infile.variables['time'].units
    nt = len(time)
else:
    raise Exception('Missing variable time')

# get all data
var_lists = od([('radn', ['solar', 'rad', 'rsds', 'srad']), \
                ('maxt', ['tmax', 'tasmax']), \
                ('mint', ['tmin', 'tasmin']), \
                ('rain', ['precip', 'pr', 'rain']), \
                ('hur',  ['hur', 'rh']), \
                ('wind', ['wind', 'windspeed'])])
var_names = array(var_lists.keys()); nv = len(var_names)
alldata = zeros((nt, nv)) # includes time
alldata[:, 1 : 4] = 999. # fill in missing data
for i in range(nv):
    var_name = var_names[i]
    var_list = var_lists[var_name]
    found_var = False

    for v in var_list:
        matchvar = isin(v, variables)
        if matchvar != []:
            matchvar = matchvar[0] # take first match
            if matchvar in vlist:
                alldata[:, i] = infile.variables[matchvar][:].squeeze()
                if 'units' in infile.variables[matchvar].ncattrs(): 
                    units = infile.variables[matchvar].units
                    # convert units, if necessary
                    if var_name == 'radn' and units == 'W m-2': # solar (MJ m-2)
                        alldata[:, i] *= 0.0864
                    elif (var_name == 'maxt' or var_name == 'mint') and units == 'K': # temperature (oC)
                        alldata[:, i] -= 273.15
                    elif var_name == 'rain' and units == 'kg m-2 s-1': # precip (mm)
                        alldata[:, i] *= 86400
                    elif var_name == 'hur' and units == '%': # hur (0-1)
                        alldata[:, i] /= 100.
                    elif var_name == 'wind': # wind (m s-1)
                        if units == 'km day-1':
                            alldata[:, i] *= 1000. / 86400
                        elif units == 'km h-1':
                            alldata[:, i] *= 1000. / 3600
                        elif units == 'miles h-1':
                            alldata[:, i] *= 1609.34 / 3600
                found_var = True
                break

    if not found_var and var_name in ['radn', 'maxt', 'mint', 'rain']:
        raise Exception('Missing necessary variable {:s}'.format(var_name))

# close input file
infile.close()

# compute day, month, year for every entry
yr0, mth0, day0 = time_units.split('days since ')[1].split(' ')[0].split('-')[0 : 3]
hr0, min0, sec0 = time_units.split('days since ')[1].split(' ')[1].split(':')[0 : 3]
ref = datetime.datetime(int(yr0), int(mth0), int(day0), int(hr0), int(min0), int(sec0))
datear = array([ref + datetime.timedelta(int(t)) for t in time])
days = array([d.day for d in datear]).reshape((nt, 1)) # convert to numpy array
months = array([d.month for d in datear]).reshape((nt, 1))
years = array([d.year for d in datear]).reshape((nt, 1))

# write output file
yj = array([int(d.strftime('%Y%j')) for d in datear]) # year + julian day
with open(options.outputfile, 'w') as f:
    for y in range(years[0], years[-1] + 1):
        # first day
        idx = yj == y * 1000 + 1
        ymd = array([y, 1, 1]).reshape(1, 3)
        co2 = csvco2[csvyears == y].reshape(1, 1)
        mat = concatenate((ymd, alldata[idx], co2), axis = 1)
        savetxt(f, mat, fmt = ['%6d', '%4d', '%4d'] + ['%6.1f'] * 4 + ['%6.2f', '%6.1f', ' %-.1f'], delimiter = '')
        # subsequent days
        idx = logical_and(yj > y * 1000 + 1, yj < y * 1000 + 1001)
        ymd = array(zip(years[idx], months[idx], days[idx])).reshape(sum(idx), 3) 
        mat = concatenate((ymd, alldata[idx]), axis = 1)
        savetxt(f, mat, fmt = ['%6d', '%4d', '%4d'] + ['%6.1f'] * 4 + ['%6.2f', '%6.1f'], delimiter = '')

# change permissions
f = os.open(options.outputfile, os.O_RDONLY)
os.fchmod(f, stat.S_IREAD | stat.S_IWRITE | stat.S_IRGRP | stat.S_IWGRP | stat.S_IROTH | stat.S_IWOTH)
os.close(f)