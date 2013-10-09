#!/usr/bin/env python

# import modules
import os
import time
from optparse import OptionParser
from netCDF4 import Dataset as nc
from numpy import where, logical_not, zeros

def create_blank(variables, units, long_names, time_units, time):
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
    s += '\t\ttime:units = \"' + time_units + '\" ;\n'
    s += '\t\ttime:long_name = \"time\" ;\n'
    # variables
    for i in range(len(variables)):
        s += '\tfloat ' + variables[i] + '(time, latitude, longitude) ;\n'
        s += '\t\t' + variables[i] + ':units = \"' + units[i] + '\" ;\n'
        s += '\t\t' + variables[i] + ':long_name = \"' + long_names[i] + '\" ;\n'
    # data
    s += 'data:\n'
    s += 'time = ' + ', '.join([str(t) for t in time]) + ' ;\n'
    return s

parser = OptionParser()
parser.add_option("-s", "--suffix", dest = "filesuffix", default = "", type = "string",
                  help = "Files' suffix")
parser.add_option("-v", "--variables", dest = "variables", default = "", type = "string",
                  help = "Variable names")
parser.add_option("-o", "--outputdir", dest = "outputdir", default = "", type = "string",
                  help = "Output directory")         
(options, args) = parser.parse_args()

variables = options.variables.split(',')
nv = len(variables)

nf = 1000
outfile = 'stdout.txt'

units = [0] * nv
long_names = [0] * nv
for i in range(nv):
    f = nc(variables[i] + '_' + options.filesuffix + '.nc4', 'r')
    var = f.variables[variables[i]]
    units[i] = var.units
    long_names[i] = var.long_name
    if not i:
        grid_idx = where(logical_not(var[0].mask))
        fgrid_idx = logical_not(var[0].mask.flatten())
        lats = f.variables['lat'][:]
        lons = f.variables['lon'][:]
        times = f.variables['time'][:]
        time_units = f.variables['time'].units
        time_long_name = f.variables['time'].standard_name
    f.close()
    
s = create_blank(variables, units, long_names, time_units, times)

lat_idx = grid_idx[0]
lon_idx = grid_idx[1]
file_names = [0] * nf

print 'Writing cdl files . . .'
open(outfile, 'w').write('Writing cdl files . . .\n')
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
nt = len(times)
ng = sum(fgrid_idx)
varmat = zeros((nt, ng))
for i in range(nv):
    print 'Adding variable', variables[i], '. . .'
    open(outfile, 'a').write('Adding variable {:s} . . .\n'.format(variables[i]))
    f = nc(variables[i] + '_' + options.filesuffix + '.nc4', 'r')
    var = f.variables[variables[i]]
    var.set_auto_maskandscale(False)
    
    print '  Fetching data . . .'
    open(outfile, 'a').write('  Fetching data . . .\n')
    t0 = time.clock()
    for j in range(nt): varmat[j] = var[j].flatten()[fgrid_idx]
    dt = time.clock() - t0
    print 'Elapsed time =', dt
    open(outfile, 'a').write('Elapsed time = {:f}\n'.format(dt))
    
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