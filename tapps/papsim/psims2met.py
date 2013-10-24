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
parser.add_option("-o", "--output", dest = "outputfile", default = "Generic.met", type = "string",
                  help = "Output met file", metavar = "FILE")
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
var_lists = od([('radn', ['solar', 'rad', 'rsds', 'srad', 'rsds_USagcfsr', 'rsds_agcfsr', 'rsds_UScfsr', 'rsds_USsrb']), \
                ('maxt', ['tmax', 'tasmax', 'tasmax_USagcfsr', 'tasmax_agcfsr', 'tasmax_UScfsr']), \
                ('mint', ['tmin', 'tasmin', 'tasmin_USagcfsr', 'tasmin_agcfsr', 'tasmin_UScfsr']), \
                ('rain', ['precip', 'pr', 'rain', 'pr_gpcc', 'pr_cru', 'pr_USagcfsr', 'pr_agcfsr', 'pr_UScfsr', 'pr_UScpc']), \
                ('wind', ['wind', 'windspeed', 'wind_USagcfsr', 'wind_agcfsr', 'wind_UScfsr']), \
                ('rhmax', ['rhmax', 'hurtmax']), \
                ('rhmin', ['rhmin', 'hurtmax']), \
                ('vp', ['vap', 'vapr'])])

var_names = array(var_lists.keys())
unit_names = array(['oC', 'oC', 'mm', 'MJ/m^2', 'm/s', '%', '%', 'Pa'])
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
                if var_name == 'radn' and units == 'W m-2': # solar
                    alldata[:, i] *= 0.0864
                elif (var_name == 'maxt' or var_name == 'mint') and units == 'K': # temperature
                    alldata[:, i] -= 273.15
                elif var_name == 'rain' and units == 'kg m-2 s-1': # precip
                    alldata[:, i] *= 86400
                elif var_name == 'wind': # wind
                    if units == 'km day-1':
                        alldata[:, i] *= 1000. / 86400
                    elif units == 'km h-1':
                        alldata[:, i] *= 1000. / 3600
                    elif units == 'miles h-1':
                        alldata[:, i] *= 1609.34 / 3600
                elif var_name == 'vp' and (units == 'hPa' or units == 'mbar'): # vapor pressure
                        alldata[:, i] *= 100.
            
            found_var = True
            break

    if not found_var:
        if var_name == 'maxt' or var_name == 'mint' or var_name == 'rain' or var_name == 'radn':
            raise Exception('Missing necessary variable {:s}'.format(var_name))
        else:
            var_names[i] = '' # indicates variable not available

# remove missing nonmandatory variables from array
not_missing = var_names != ''
nv = sum(not_missing)
var_names = var_names[not_missing]
unit_names = unit_names[not_missing]
alldata = reshape(alldata[:, not_missing], (nt, nv))

# compute day, month, year for every entry
yr0, mth0, day0 = time_units.split('days since ')[1].split(' ')[0].split('-')[0 : 3]
hr0, min0, sec0 = time_units.split('days since ')[1].split(' ')[1].split(':')[0 : 3]
ref = datetime.datetime(int(yr0), int(mth0), int(day0), int(hr0), int(min0), int(sec0))
datear = array([ref + datetime.timedelta(i) for i in [int(j) for j in time]])
days = array([d.timetuple().tm_yday for d in datear]).reshape((nt, 1)) # convert to numpy array
months = array([d.month for d in datear])
years = array([d.year for d in datear]).reshape((nt, 1))

# compute tav
tmin_idx = where(var_names == 'mint')[0][0]
tmax_idx = where(var_names == 'maxt')[0][0]
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

# write file
head = '[weather.met.weather]\nstationname = Generic\n'
head += 'latitude = ' + str(infile.variables['latitude'][0]) + ' (DECIMALDEGREES)\n'
head += 'longitude = ' + str(infile.variables['longitude'][0]) + ' (DECIMALDEGREES)\n'
head += 'tav = ' + str(tav) + ' (oC)\n'
head += 'amp = ' + str(amp) + ' (oC)\n\n'
head += 'year   day   ' + '   '.join(var_names) + '\n'
head += '()     ()    ' + '   '.join(['({:s})'.format(s) for s in unit_names]) + '\n'

# close input file
infile.close()

# write output file
with open(options.outputfile, 'w') as f:
    f.write(head)
    savetxt(f, concatenate((years, days, alldata), axis = 1), fmt = ['%d', '%d'] + ['%.3f'] * nv, delimiter = '   ')

# change permissions
f = os.open(options.outputfile, os.O_RDONLY)
os.fchmod(f, stat.S_IREAD | stat.S_IWRITE | stat.S_IRGRP | stat.S_IWGRP | stat.S_IROTH | stat.S_IWOTH)
os.close(f)
