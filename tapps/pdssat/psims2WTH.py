#!/usr/bin/env python

# import modules
import os, stat, datetime
from netCDF4 import Dataset as nc
from optparse import OptionParser
from collections import OrderedDict as od
from numpy import empty, array, where, reshape, concatenate, savetxt

# parse inputs
parser = OptionParser()
parser.add_option("-i", "--input", dest = "inputfile", default = "Generic.psims.nc", type = "string",
                  help = "NetCDF3 file to parse", metavar = "FILE")
parser.add_option("-v", "--variables", dest = "variables", default = "time,tmin,tmax,precip,solar", type = "string",
                  help = "Comma-separated list of variables to parse", metavar = "FILE")
parser.add_option("-o", "--output", dest = "outputfile", default = "Generic.WTH", type = "string",
                  help = "Output WTH file", metavar = "FILE")
(options, args) = parser.parse_args()

# open netcdf file for reading
infile = nc(options.inputfile, 'r', format = 'NETCDF3_CLASSIC')

# variable list
variables = options.variables.split(',')

# get time
vlist = infile.variables.keys()
if 'time' in vlist: # make sure time is in file
    time = infile.variables['time'][:]
    time_units = infile.variables['time'].units
else:
    raise Exception('Missing variable time')

# get all data
var_lists = od([('SRAD', ['solar', 'rad', 'rsds', 'srad', 'rsds_USagcfsr', 'rsds_UScfsr', 'rsds_USsrb']), \
                ('TMAX', ['tmax', 'tasmax', 'tasmax_USagcfsr', 'tasmax_UScfsr']), \
                ('TMIN', ['tmin', 'tasmin', 'tasmin_USagcfsr', 'tasmin_UScfsr']), \
                ('RAIN', ['precip', 'pr', 'rain', 'pr_gpcc', 'pr_cru', 'pr_USagcfsr', 'pr_UScfsr', 'pr_UScpc']), \
                ('WIND', ['wind', 'windspeed', 'wind_USagcfsr', 'wind_UScfsr']), \
                ('DEWP', ['dew', 'dewp', 'dewpoint', 'tdew']), \
                ('SUNH', ['sun', 'sunh']), \
                ('RHUM', ['rhum', 'hur']), \
                ('VAPR', ['vap', 'vapr'])])
var_names = array(var_lists.keys())
nt = len(time)
nv = len(var_names)
alldata = empty((nt, nv)) # includes time
for i in range(nv):
    var_name = var_names[i]
    var_list = var_lists[var_name]
    found_var = False
    
    for v in var_list:
        if v in variables and v in vlist:
            alldata[:, i] = infile.variables[v][:].squeeze()
            
            if 'units' in infile.variables[v].ncattrs(): 
                units = infile.variables[v].units
                
                # convert units, if necessary
                if var_name == 'SRAD' and units == 'W m-2': # solar
                    alldata[:, i] *= 0.0864
                elif (var_name == 'TMAX' or var_name == 'TMIN') and units == 'K': # temperature
                    alldata[:, i] -= 273.15
                elif var_name == 'RAIN' and units == 'kg m-2 s-1': # precip
                    alldata[:, i] *= 86400
                elif var_name == 'WIND': # wind
                    if units == 'm s-1':
                        alldata[:, i] *= 86.4
                    elif units == 'km h-1':
                        alldata[:, i] *= 24
                    elif units == 'miles h-1':
                        alldata[:, i] *= 38.624256
                elif var_name == 'SUNH' and units == 'h': # sunh
                    alldata[:, i] *= 100. / 24
            
            found_var = True
            break

    if not found_var:
        if var_name == 'SRAD' or var_name == 'TMAX' or var_name == 'TMIN' or var_name == 'RAIN':
            raise Exception('Missing necessary variable {:s}'.format(var_name))
        else:
            var_names[i] = '' # indicates variable not available

# remove missing nonmandatory variables from array
not_missing = var_names != ''
nv = sum(not_missing)
var_names = var_names[not_missing]
alldata = reshape(alldata[:, not_missing], (nt, nv))

# compute day, month, year for every entry
yr0, mth0, day0 = time_units.split('days since ')[1].split(' ')[0].split('-')[0 : 3]
hr0, min0, sec0 = time_units.split('days since ')[1].split(' ')[1].split(':')[0 : 3]
ref = datetime.datetime(int(yr0), int(mth0), int(day0), int(hr0), int(min0), int(sec0))
datear = array([ref + datetime.timedelta(int(t)) for t in time])
days = array([d.timetuple().tm_yday for d in datear]) # convert to numpy array
months = array([d.month for d in datear])
years = array([d.year for d in datear])

# compute tav
tmin_idx = where(var_names == 'TMIN')[0][0]
tmax_idx = where(var_names == 'TMAX')[0][0]
tmin = alldata[:, tmin_idx]
tmax = alldata[:, tmax_idx]
tav = 0.5 * (sum(tmin) + sum(tmax)) / nt

# compute amp
monmax = -float("inf")
monmin = float("inf")
for i in range(1, 13):
    m = where(months == i)
    summ = sum(months == i)
    if summ != 0:
        t = 0.5 * (sum(tmin[m]) + sum(tmax[m])) / summ 
        if t > monmax:
            monmax = t
        if t < monmin:
            monmin = t
amp = monmax - monmin

# round data
date = (1000 * (years % 100) + days).reshape((nt, 1))
for i in range(nv): # round variables
    alldata[:, i] = [round(e, 1) for e in alldata[:, i]]

# ensure that after rounding tmax > tmin and solar > 0.0
bad_idx = alldata[:, tmax_idx] <= alldata[:, tmin_idx]
alldata[bad_idx, tmax_idx] = alldata[bad_idx, tmin_idx] + 0.1
alldata[alldata[:, 0] <= 0.0, 0] = 0.1

# write file
filler = -99 # used for ELEV, REFHT
filler2 = 10 # used for WNDHT
head = '*WEATHER DATA : ' + os.path.basename(options.inputfile) + '\n'
head += '@ INSI      LAT     LONG  ELEV   TAV   AMP REFHT WNDHT\n    CI'
head += '%9.3f' % infile.variables['latitude'][0]
head += '%9.3f' % infile.variables['longitude'][0]
head += '%6d' % filler + '%6.1f' % tav + '%6.1f' % amp
head += '%6d' % filler + '%6d' % filler2 + '\n'
head += '@DATE  ' + '  '.join(var_names) + '\n'

# close input file
infile.close()

# write output file
with open(options.outputfile, 'w') as f:
    f.write(head)
    savetxt(f, concatenate((date, alldata), axis = 1), fmt = ['%.5d'] + ['%6.1f'] * nv, delimiter = '')

# change permissions
f = os.open(options.outputfile, os.O_RDONLY)
os.fchmod(f, stat.S_IREAD | stat.S_IWRITE | stat.S_IRGRP | stat.S_IWGRP | stat.S_IROTH | stat.S_IWOTH)
os.close(f)
