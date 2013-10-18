#!/usr/bin/env python

# import modules
from os.path import isdir
from os import listdir, sep
from optparse import OptionParser
from numpy import floor, array, argsort

def within_limits(x, lim1, lim2):
    xa = array(x)
    sel = array([int(e) >= lim1 and int(e) <= lim2 for e in xa])
    xsub = xa[sel]
    xsort = xsub[argsort([int(i) for i in xsub])] # sort
    return xsort

# parse inputs
parser = OptionParser()
parser.add_option("-w", "--weather", dest = "weatherdir", default = "psims/data/clim/agmerra", type = "string",
                  help = "Directory where weather data are located")
parser.add_option("-b", "--box", dest = "box", default = "90,50,-180,-120", type = "string",
                  help = "Comma-separated list of lat1,lat2,lon1,lon2 representing raster subset")
parser.add_option("-d", "--delta", dest = "delta", default = 1, type = "float",
                  help = "Distance between each grid cell in arcminutes")
parser.add_option("-s", "--soil", dest = "soildir", default = "", type = "string",
                  help = "Directory where soil data are located (optional)")
parser.add_option("-c", "--campaign", dest = "campfile", default = "", type = "string",
                  help = "Campaign file (optional)")
parser.add_option("-g", "--gridlist", dest = "gridlist", default = "gridList.txt", type = "string",
                  help = "Grid list file to save", metavar = "FILE")
(options, args) = parser.parse_args()

wd = options.weatherdir
sd = options.soildir
use_sd = sd != ''

box = [int(b) for b in options.box.split(',')]
lat1 = box[0]; lat2 = box[1]
lon1 = box[2]; lon2 = box[3]
delta = options.delta / 60.

latidx1 = floor((90. - lat1) / delta + 0.5)
latidx2 = floor((90. - lat2) / delta + 0.5)
lonidx1 = floor((lon1 + 180.) / delta + 0.5)
lonidx2 = floor((lon2 + 180.) / delta + 0.5)

grid_list = []
for lat in within_limits(listdir(wd), latidx1, latidx2):
    for lon in within_limits(listdir(wd + sep + lat), lonidx1, lonidx2):
        if not use_sd or (use_sd and isdir(sd + sep + lat + sep + lon)):
            grid_list.append(lat + lon)

with open(options.gridlist, 'w') as f:
    for g in grid_list:
        f.write(g[: 3] + '/' + g[3 :])
        f.write('\n')