#!/usr/bin/env python

# import modules
from os import remove
from shutil import copy
from optparse import OptionParser
from netCDF4 import Dataset as nc

def createnc(filename, lat, lon, time, time_units):
    # create file
    f = nc(filename, 'w', format = 'NETCDF4_CLASSIC')
    # create latitude
    f.createDimension('lat', None)
    lat_var = f.createVariable('lat', 'f8', ('lat',), zlib = True, shuffle = False, complevel = 9, chunksizes = [1])
    lat_var[:] = lat
    lat_var.units = 'degrees_north'
    lat_var.long_name = 'latitude'
    # create longitude
    f.createDimension('lon', len(lon))
    lon_var = f.createVariable('lon', 'f4', ('lon',), zlib = True, shuffle = False, complevel = 9, chunksizes = [len(lon)])
    lon_var[:] = lon
    lon_var.units = 'degrees_east'
    lon_var.long_name = 'longitude'
    # create time
    f.createDimension('time', len(time))
    time_var = f.createVariable('time', 'i4', ('time',), zlib = True, shuffle = False, complevel = 9, chunksizes = [len(time)], endian = 'little')
    time_var[:] = time
    time_var.units = time_units
    time_var.long_name = 'time'   
    # close file
    f.close()

# parse inputs
parser = OptionParser()
parser.add_option("-i", "--input", dest = "infile", default = "out.psims.nc4", type = "string",
                  help = "Input netCDF4 file", metavar = "FILE")
parser.add_option("-v", "--vars", dest = "vars", default = "yield", type = "string", 
                  help = "Comma-separated list of variables to create")
parser.add_option("-p", "--pre", dest = "prefix", default = "pdssat.pm_watch_hist", type = "string",
                  help = "File prefix of form model_climate_clim_scenario")
parser.add_option("-s", "--suf", dest = "suffix", default = "annual_1980_2012", type = "string",
                  help = "File suffix of form timestep_start-year_end-year")
parser.add_option("-c", "--crop", dest = "crop", default = "mai", type = "string",
                  help = "Crop name")
parser.add_option("--scens", dest = "scens", default = "1", type = "string",
                  help = "Comma-separated list of scenario numbers to process")
parser.add_option("--scen_names", dest = "scen_names", default = "fullharm_noirr", type = "string",
                  help = "Comma-separated list of scenario names")
options, args = parser.parse_args()

# get options
var_names = options.vars.split(',')
prefix = options.prefix
suffix = options.suffix
crop = options.crop
scens = [int(s) - 1 for s in options.scens.split(',')] # zero-based index
scen_names = options.scen_names.split(',')

# load netCDF4 file
f = nc(options.infile, 'r', format = 'NETCDF4_CLASSIC')
lat = f.variables['lat'][:] if 'lat' in f.variables else f.variables['latitude'][:]
lon = f.variables['lon'][:] if 'lon' in f.variables else f.variables['longitude'][:]
time = f.variables['time'][:]
time_units = f.variables['time'].units

# create base netcdf4 file
basenc = 'basenc.nc4'
createnc(basenc, lat, lon, time, time_units)

# file name template
var_file_names = prefix + '_{:s}_{:s}_' + crop + '_' + suffix + '.nc4'

# iterate over variables
for i in range(len(var_names)):
    v = var_names[i]
    
    if v in ['initr', 'leach', 'sco2', 'sn2o']:
        # skip variable
        print 'Skipping', v, '. . .'
        continue
    
    if 'dssat' in prefix:
        # map dssat variables, converting units as needed
        if v == 'yield':
            var = f.variables['HWAM']
            var_arr = var[:]
            if 'units' in var.ncattrs() and var.units == 'kg/ha': var_arr *= 0.001
            units = 't ha-1 yr-1'
        elif v == 'pirrww':
            var = f.variables['IRCM']
            var_arr = var[:]
            units = 'mm yr-1'
        elif v == 'biom':
            var = f.variables['CWAM']
            var_arr = var[:]
            if 'units' in var.ncattrs() and var.units == 'kg/ha': var_arr *= 0.001
            units = 't ha-1 yr-1'
        elif v == 'aet':
            var = f.variables['ETCP']
            var_arr = var[:]
            units = 'mm yr-1'
        elif v == 'plant-day':
            var = f.variables['PDAT']
            var_arr = var[:]
            units = 'day of year'
        elif v == 'anth-day':
            var = f.variables['ADAT']
            var_arr = var[:]
            units = 'days from planting'
        elif v == 'maty-day':
            var = f.variables['MDAT']
            var_arr = var[:]
            units = 'days from planting'
        elif v == 'gsprcp':
            var = f.variables['PRCP']
            var_arr = var[:]
            units = 'mm ha-1 yr-1'
        elif v == 'gsrsds':
            var = f.variables['SRADA']
            var_arr = var[:]
            units = 'w m-2 yr-1'
        elif v == 'sumt':
            var = f.variables['TMINA']
            var_arr = 0.5 * (var[:] + f.variables['TMAXA'][:]) # average temperatures
            units = 'deg C-days yr-1'
        else:
            raise Exception('Variable unrecognized. Exiting . . .')

    # get chunk sizes, long name, dimensions, and scenario index
    chunksizes = var.chunking()
    long_name = var.long_name if 'long_name' in var.ncattrs() else ''
    dims = var.dimensions
    sn = 'scenario' if 'scenario' in dims else 'scen'
    scen_idx = dims.index(sn)

    # subarray template 
    sub_arr = 'var_arr[' + ':,' * scen_idx + '{:d},' + ':,' * (len(dims) - 1 - scen_idx) + ']'

    # remove scenario from chunksizes and dimensions
    chunksizes.pop(scen_idx)
    dims = list(dims)
    dims.remove(sn)
    if 'latitude' in dims: # change name from latitude -> lat
        dims[dims.index('latitude')] = 'lat'
    if 'longitude' in dims: # change name from longitude -> lon
        dims[dims.index('longitude')] = 'lon'
    dims = tuple(dims)

    # iterate over scenarios
    for j in range(len(scens)):
        print 'Writing', scen_names[j], 'for variable', v, '. . .'
        
        # file name
        var_file_name = var_file_names.format(scen_names[j], v)
        copy(basenc, var_file_name)
        
        # write variable to file
        var_file = nc(var_file_name, 'a', format = 'NETCDF4_CLASSIC') # append
        vv = var_file.createVariable(v + '_' + crop, 'f4', dims, zlib = True, shuffle = False, complevel = 9, chunksizes = chunksizes, fill_value = 1e20)
        vv[:] = eval(sub_arr.format(scens[j]))
        vv.units = units
        vv.long_name = long_name
        
        # close file
        var_file.close()

# remove base netcdf4 file
remove(basenc)

# close file
f.close()