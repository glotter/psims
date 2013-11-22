#!/usr/bin/env python

# import modules
import os, re, time, sys
from optparse import OptionParser
from netCDF4 import Dataset as nc
from numpy import where, logical_not, logical_and, zeros, double

def create_blank(vars, units, longnames, time, timeunits):
    s = 'netcdf blank {\n'
    s += 'dimensions:\n\tlongitude = 1 ;\n\tlatitude = 1 ;\n\ttime = UNLIMITED ;\n'
    s += 'variables:\n'
    # longitude
    s += '\tdouble longitude(longitude) ;\n'
    s += '\t\tlongitude:units = \"degrees_east\" ;\n'
    s += '\t\tlongitude:long_name = \"longitude\" ;\n'
    # latitude
    s += '\tdouble latitude(latitude) ;\n'
    s += '\t\tlatitude:units = \"degrees_north\" ;\n'
    s += '\t\tlatitude:long_name = \"latitude\" ;\n'
    # time
    s += '\tint time(time) ;\n'
    s += '\t\ttime:units = \"' + timeunits + '\" ;\n'
    s += '\t\ttime:long_name = \"time\" ;\n'
    # variables
    for i in range(len(vars)):
        s += '\tfloat ' + vars[i] + '(time, latitude, longitude) ;\n'
        s += '\t\t' + vars[i] + ':units = \"' + units[i] + '\" ;\n'
        s += '\t\t' + vars[i] + ':long_name = \"' + longnames[i] + '\" ;\n'
    # data
    s += 'data:\n'
    s += 'time = ' + ', '.join([str(t) for t in time]) + ' ;\n'
    return s
    
def default_units(vars):
    nv = len(vars)
    units = [0] * nv
    for i in range(nv):
        v = vars[i]
        if v == 'hur' or v == 'hurtmax':
            units[i] = '%'
        elif re.compile('pr_*').match(v): # any precipitation variable
            units[i] = 'kg m-2 s-1'
        elif v == 'rsds':
            units[i] = 'W m-2'
        elif v == 'tas' or v == 'tasmin' or v == 'tasmax':
            units[i] = 'K'
        elif v == 'wind':
            units[i] = 'm s-1'
        elif v == 'ps':
            units[i] = 'Pa'
        elif v == 'hus':
            units[i] = 'kg kg-1'
        elif v == 'rlds':
            units[i] = 'W m-2'
        elif v == 'vap':
            units[i] = 'Pa'
        else:
            raise Exception('Unknown variable {:s} encountered'.format(v))
    return units

def default_long_names(vars):
    nv = len(vars)
    longnames = [0] * nv
    for i in range(nv):
        v = vars[i]
        if v == 'hur':
            longnames[i] = 'Relative Humidity Average approximated by Average Temperature'
        elif v == 'hurtmax':
            longnames[i] = 'Relative Humidity at time of Maximum Temperature'
        elif re.compile('pr_*').match(v): # any precipitation variable
            longnames[i] = 'Precipitation Flux'
        elif v == 'rsds':
            longnames[i] = 'Surface Downwelling Shortwave Radiation Flux'
        elif v == 'tas':
            longnames[i] = 'Surface Air Temperature at 2 Meter'
        elif v == 'tasmin':
            longnames[i] = 'Minimum Surface Air Temperature'
        elif v == 'tasmax':
            longnames[i] = 'Maximum Surface Air Temperature'
        elif v == 'wind':
            longnames[i] = 'Wind Speed at 10 meter'
        elif v == 'ps':
            longnames[i] = 'Surface pressure'
        elif v == 'hus':
            longnames[i] = 'Specific humidity'
        elif v == 'rlds':
            longnames[i] = 'Surface downwelling longwave radiation flux'
        else:
            raise Exception('Unknown variable {:s} encountered'.format(v))
    return longnames

def ind2process(batchno, numjobs, numbatches):
    bz = numjobs / numbatches
    si = bz * (batchno - 1)
    ei = numjobs if batchno == numbatches else min(si + bz, numjobs)
    return si, ei

parser = OptionParser()
parser.add_option("-s", "--suffix", dest = "filesuffix", default = "", type = "string",
                  help = "Files' suffix")
parser.add_option("-v", "--variables", dest = "variables", default = "", type = "string",
                  help = "Variable names")
parser.add_option("-b", "--batch", dest = "batch", default = 1, type = "int",
                  help = "Batch number")
parser.add_option("-n", "--numbatches", dest = "numbatches", default = 64, type = "int",
                  help = "Number of batches")
parser.add_option("-l", "--latlimits", dest = "latlimits", default = "", type = "string",
                  help = "Latitude limits within which to process data")
parser.add_option("-d", "--dir", dest = "dir", default = "", type = "string",
                  help = "Directory where variable files are located")
parser.add_option("-o", "--outputdir", dest = "outputdir", default = "", type = "string",
                  help = "Output directory")
(options, args) = parser.parse_args()

# get variables
variables = options.variables.split(',')
nv = len(variables)

# get batch information
batch_number = options.batch
num_batches = options.numbatches
if batch_number < 1 or batch_number > num_batches:
    raise Exception('Invalid batch number')

# get latitude limits
latlimits = options.latlimits.split(',')
latlim1 = double(latlimits[0])
latlim2 = double(latlimits[1])

units = default_units(variables)
long_names = default_long_names(variables)
got_mask = False
for i in range(nv):
    v = variables[i]
    f = nc(options.dir + os.sep + v + '_' + options.filesuffix + '.nc4', 'r')
    if re.compile('pr_*').match(v): v = 'pr'
    var = f.variables[v]
    if 'units' in var.ncattrs():
        units[i] = var.units
    if 'long_name' in var.ncattrs():
        long_names[i] = var.long_name
    if v == 'pr' and not got_mask: # get mask, etc., from pr file
        # unmasked 1D and 2D indices
        grid_idx = where(logical_not(var[0].mask))
        fgrid_idx = where(logical_not(var[0].mask.flatten()))[0]
        lat_idx = grid_idx[0]
        lon_idx = grid_idx[1]
        # latitudes and longitudes
        lats = f.variables['lat'][:]
        lons = f.variables['lon'][:]
        # time
        times = f.variables['time'][:].astype(int) # round to integers
        time_units = f.variables['time'].units
        time_long_name = f.variables['time'].standard_name
        got_mask = True
    f.close()

# restrict latitude range
sel_lat = logical_and(lats[lat_idx] >= latlim1, lats[lat_idx] <= latlim2)
lat_idx = lat_idx[sel_lat]
lon_idx = lon_idx[sel_lat]
fgrid_idx = fgrid_idx[sel_lat]

# get range of points to process
si, ei = ind2process(batch_number, len(fgrid_idx), num_batches)
if si >= len(fgrid_idx):
    print 'No jobs to execute. Exiting . . .'
    sys.exit(0)
    
# restrict range of points
lat_idx = lat_idx[si : ei]
lon_idx = lon_idx[si : ei]
fgrid_idx = fgrid_idx[si : ei]

nf = len(lat_idx) # number of points to process
nt = len(times)   # number of times

# create header string
s = create_blank(variables, units, long_names, times, time_units)

# standard out file
outfile = 'stdout_' + str(si) + '_' + str(ei) + '.txt'
    
hstr = 'Processing start_idx = ' + str(si) + ', end_idx = ' + str(ei)
open(outfile, 'w').write(hstr + '\n')

print 'Writing cdl files . . .'
open(outfile, 'a').write('Writing cdl files . . .\n')
file_names = [0] * nf
for i in range(nf):
    lati = lat_idx[i]
    loni = lon_idx[i]
    grid1 = str(lati + 1).zfill(3)
    grid2 = str(loni + 1).zfill(3)
    
    # make directory, if necessary
    drt = options.outputdir + os.sep + grid1 + os.sep + grid2
    if not os.path.exists(drt):
        os.makedirs(drt)
    
    # make file
    file_names[i] = drt + os.sep + grid1 + '_' + grid2
    with open(file_names[i] + '.cdl', 'w') as f:
        f.write(s)
        f.write('latitude = ' + str(lats[lati]) + ' ;\n')
        f.write('longitude = ' + str(lons[loni]) + ' ;\n')
    
# iterate through all variables
varmat = zeros((nt, nf))
for i in range(nv):
    v = variables[i]
    print 'Adding variable', v, '. . .'
    open(outfile, 'a').write('Adding variable {:s} . . .\n'.format(v))
    
    # open file
    f = nc(options.dir + os.sep + v + '_' + options.filesuffix + '.nc4', 'r')
    var = f.variables['pr'] if re.compile('pr_*').match(v) else f.variables[v]
    var.set_auto_maskandscale(False)
    
    # fetch data
    print '  Fetching data . . .'
    open(outfile, 'a').write('  Fetching data . . .\n')
    t0 = time.clock()
    for j in range(nt):
        try:
            varmat[j] = var[j].flatten()[fgrid_idx]
        except:
            print j
            print fgrid_idx
            print len(fgrid_idx)
            print 'Failed on variable', v
            open(outfile, 'a').write(str(j) + '\n')
            open(outfile, 'a').write(str(len(fgrid_idx)) + '\n')
            open(outfile, 'a').write('Failed on variable {:s}'.format(v))
            f.close()
            sys.exit(0)
    dt = time.clock() - t0
    print 'Elapsed time =', dt
    open(outfile, 'a').write('Elapsed time = {:f}\n'.format(dt))
    
    # close file
    f.close()
    
    # append data
    print '  Appending variable . . .'
    open(outfile, 'a').write('  Appending variable . . .\n')
    t0 = time.clock()
    for j in range(nf):
        with open(file_names[j] + '.cdl', 'a') as f:
            f.write(variables[i] + ' = ' + ', '.join(varmat[:, j].astype('|S20')) + '; \n')
            if i == nv - 1: f.write('}') # end file
    dt = time.clock() - t0
    print 'Elapsed time =', dt
    open(outfile, 'a').write('Elapsed time = {:f}\n'.format(dt))
        
# iterate through all files
print 'Writing psims.nc files . . .'
open(outfile, 'a').write('Writing psims.nc files . . .\n')
t0 = time.clock()
for i in range(nf):
    os.system('ncgen -k1 -o ' + file_names[i] + '.psims.nc ' + file_names[i] + '.cdl')
    os.remove(file_names[i] + '.cdl') # delete cdl file
dt = time.clock() - t0
print 'Elapsed time =', dt
open(outfile, 'a').write('Elapsed time = {:f}\n'.format(dt))