#!/usr/bin/env python

# import modules
from os import listdir
import sys, re, time as tm
from os.path import sep, exists
from optparse import OptionParser
from netCDF4 import Dataset as nc
from numpy.ma import unique, masked_where
from numpy import pi, zeros, ones, cos, resize, where

def createnc(filename, time, tunits, scens, fpuidxs, basinidxs, regionidxs):
    f = nc(filename, 'w', format = 'NETCDF3_CLASSIC') # create file
    f.createDimension('time', len(time)) # time
    timevar = f.createVariable('time', 'i4', ('time',))
    timevar[:] = time
    timevar.units = tunits
    timevar.long_name = 'time'
    f.createDimension('scen', len(scens)) # scenario
    scenvar = f.createVariable('scen', 'i4', ('scen',))
    scenvar[:] = range(1, len(scens) + 1)
    scenvar.units = 'mapping'
    scenvar.long_name = ', '.join(scens)
    f.createDimension('irr', 2) # irr
    irrvar = f.createVariable('irr', 'i4', ('irr',))
    irrvar[:] = range(1, 3)
    irrvar.units = 'mapping'
    irrvar.long_name = 'ir, rf'
    f.createDimension('fpu_index', len(fpuidxs)) # fpu_index 
    fpuvar = f.createVariable('fpu_index', 'i4', ('fpu_index',))
    fpuvar[:] = fpuidxs
    fpuvar.units = 'FPU index'
    fpuvar.long_name = '309 food producing Units'
    f.createDimension('basin_index', len(basinidxs)) # basin_index 
    bsnvar = f.createVariable('basin_index', 'i4', ('basin_index',))
    bsnvar[:] = basinidxs
    bsnvar.units = 'Basin index'
    bsnvar.long_name = '129 major river basins' 
    f.createDimension('region_index', len(regionidxs)) # region_index 
    rgnvar = f.createVariable('region_index', 'i4', ('region_index',))
    rgnvar[:] = regionidxs
    rgnvar.units = 'Region index'
    rgnvar.long_name = '13 regions'       
    f.close() # close file

# parse inputs
parser = OptionParser()
parser.add_option("-b", "--batch", dest = "batch", default = "1", type = "int",
                  help = "Batch to process")
parser.add_option("-n", "--numbatches", dest = "num_batches", default = "64", type = "int",
                  help = "Total number of batches")
parser.add_option("-d", "--dir", dest = "dir", default = "", type = "string",
                  help = "Directory in which to perform aggregration")
parser.add_option("-m", "--mod", dest = "mod", default = "pDSSAT.pm,pDSSAT.pt,pAPSIM", type = "string",
                  help = "Comma-separated list of crop models")
parser.add_option("-w", "--weath", dest = "weath", default = "AgCFSR,AgMERRA,CFSR,ERAI,GRASP,Princeton,WATCH,WFDEI.CRU,WFDEI.GPCC", type = "string",
                  help = "Comma-separated list of weather datasets")
parser.add_option("-c", "--crop", dest = "crop", default = "maize", type = "string",
                  help = "Comma-separated list of crops")
parser.add_option("-l", "--landuseir", dest = "landuseir", default = "", type = "string",
                  help = "Landuse (weight) mask file for irrigation", metavar = "FILE")
parser.add_option("-r", "--landuserf", dest = "landuserf", default = "", type = "string",
                  help = "Landuse (weight) mask file for rainfed", metavar = "FILE")
parser.add_option("-a", "--aggr", dest = "aggr", default = "", type = "string",
                  help = "Aggregration mask file", metavar = "FILE")
parser.add_option("-o", "--outdir", dest = "outdir", default = "", type = "string",
                  help = "Output directory to save results")
options, args = parser.parse_args()

# constants
fillv = 1e20
yieldthr1 = 0.1 # t/ha
yieldthr2 = 50

# root directory
rootdir = options.dir
if rootdir[-1] == sep: rootdir = rootdir[: -1] # remove final separator

# names
models = options.mod.split(',')
climates = options.weath.split(',')
crops = options.crop.split(',')

# directories
dirs = []
for md in models:
    for cm in climates:
        for cp in crops:
            dirs.append(md + sep + cm + sep + cp)
ndirs = len(dirs)

# find out start and end indices for batch
batch = options.batch
numbatches = options.num_batches
bz = ndirs / numbatches
si = bz * (batch - 1)
ei = ndirs if batch == numbatches else min(si + bz, ndirs)

# no work for processor to do
if si >= ndirs:
    print 'No jobs for processor to perform. Exiting . . .'
    sys.exit()

# scenarios
dssat_pm_scens = ['fullharm', 'default', 'harmnon']
dssat_pt_scens = ['fullharm']
apsim_scens = dssat_pm_scens
dssat_pm_scens_full = ['fullharm_noirr', 'fullharm_firr', 'default_noirr', 'default_firr', 'harmnon_noirr', 'harmnon_firr']
dssat_pt_scens_full = ['fullharm_noirr', 'fullharm_firr']
apsim_scens_full = dssat_pm_scens_full

# variables
dssatvars = ['yield', 'pirrww', 'plant-day', 'maty-day', 'aet', 'gsprcp', 'anth-day', 'biom', 'gsrsds', 'sumt']
apsimvars = ['yield', 'pirrww', 'plant-day', 'maty-day', 'aet', 'gsprcp', 'anth-day', 'biom', 'gsrsds', 'sumt', 'initr', 'leach', 'sco2', 'sn2o']

# find unique crops
uqcrops = list(set([d.split(sep)[2].title() for d in dirs])) # capitalize first letters
ncrops = len(uqcrops)

# load weight masks
landmasksir = [0] * ncrops
landuseir = nc(options.landuseir) # land use IR mask
for i in range(ncrops):
    landmasksir[i] = landuseir.variables[uqcrops[i]][:]
landuseir.close()
landmasksrf = [0] * ncrops
landuserf = nc(options.landuserf) # land use RF mask
for i in range(ncrops):
    landmasksrf[i] = landuserf.variables[uqcrops[i]][:]
landuserf.close()

# load aggregration masks
aggr = nc(options.aggr)
fpu = aggr.variables['fpu'][:]
basin = aggr.variables['basin'][:]
region = aggr.variables['region'][:]
lat = aggr.variables['lat'][:] # latitude
aggr.close()

# find unique aggregration indices
fpuidxs = unique(fpu)
fpuidxs = fpuidxs[~fpuidxs.mask] # remove fill value from list 
nfpu = len(fpuidxs)
basinidxs = unique(basin)
basinidxs = basinidxs[~basinidxs.mask]
nbasin = len(basinidxs)
regionidxs = unique(region)
regionidxs = regionidxs[~regionidxs.mask]
nregion = len(regionidxs)

# number of latitudes and longitudes
nlats, nlons = fpu.shape

# compute area as function of latitude
area = 100 * (111.2 / 2)**2 * cos(pi * lat / 360)
area = resize(area, (nlons, nlats)).T

for i in range(si, ei):
    # iterate over subdirectories
    subdir = rootdir + sep + dirs[i]
    if not exists(subdir):
        print 'WARNING: Directory ' + subdir + ' does not exist. Skipping . . .'
        continue
        
    # get files in directory
    files = listdir(rootdir + sep + dirs[i])
    if not len(files):
        print 'WARNING: No files in directory ' + subdir + '. Skipping . . .'
        continue
        
    # variables and scenarios
    mod = dirs[i].split(sep)[0]
    if mod == 'pDSSAT.pm':
        vars = dssatvars
        scens = dssat_pm_scens
        scens_full = dssat_pm_scens_full
    elif mod == 'pDSSAT.pt':
        vars = dssatvars
        scens = dssat_pt_scens
        scens_full = dssat_pt_scens_full
    elif mod == 'pAPSIM':
        vars = apsimvars
        scens = apsim_scens
        scens_full = apsim_scens_full
    
    # crop
    cidx = crops.index(dirs[i].split(sep)[2])
    
    # use first file to get time and units    
    f = sep.join([rootdir, dirs[i], sep, files[0]])
    ncf = nc(f)
    time = ncf.variables['time'][:]
    tunits = ncf.variables['time'].units
    ncf.close()
    
    # use first file to get filename
    filename = files[0].split('_')
    filename.remove(filename[3])
    filename.remove(filename[3])
    filename.remove(filename[3])
    filename = options.outdir + sep + '_'.join(filename) # save in output directory
    
    # create nc file
    createnc(filename, time, tunits, scens, fpuidxs, basinidxs, regionidxs)
    
    nv = len(vars)  # number of variables
    nt = len(time)  # number of times
    ns = len(scens) # number of scenarios

    fput = resize(fpu, (nt, nlats, nlons)) # resize maps
    basint = resize(basin, (nt, nlats, nlons))
    regiont = resize(region, (nt, nlats, nlons))

    varfpu = fillv * ones((nv, nfpu, nt, ns, 2)) # preallocate averages and areas
    varbasin = fillv * ones((nv, nbasin, nt, ns, 2))
    varregion = fillv * ones((nv, nregion, nt, ns, 2))
    areafpu = zeros((nfpu, nt, ns, 2))
    areabasin = zeros((nbasin, nt, ns, 2))
    arearegion = zeros((nregion, nt, ns, 2))
    vunits = [''] * nv
    for j in range(len(scens_full)):
        # iterate over scenarios
        scen_irr = scens_full[j]
        
        # find scenario and irr
        scen_irr_split = scen_irr.split('_')
        sidx = scens.index(scen_irr_split[0])
        iidx = int(scen_irr_split[1] != 'firr')

        # pull mask from yield variable for corresponding scenario
        yieldfile = [f for f in files if re.search(scen_irr + '_yield', f)][0]
        yf = nc(sep.join([rootdir, dirs[i], yieldfile]))
        yvars = yf.variables.keys()
        yidx = ['yield' in v for v in yvars].index(True)
        yieldvar = yf.variables[yvars[yidx]][:]
        yieldvar = yieldvar.transpose((2, 0, 1))
        yieldvar = masked_where(yieldvar < yieldthr1, yieldvar) # mask yields based on thresholds
        yieldvar = masked_where(yieldvar > yieldthr2, yieldvar)
        yieldmask = yieldvar.mask
        yf.close()

        fpucommon = masked_where(yieldmask, fput) # common masks
        basincommon = masked_where(yieldmask, basint)
        regioncommon = masked_where(yieldmask, regiont)

        weight = landmasksir[cidx] if not iidx else landmasksrf[cidx] # weights
        
        fpus = zeros((nfpu, nt, nlats, nlons), dtype = bool) # fpu
        basins = zeros((nbasin, nt, nlats, nlons), dtype = bool) # basin
        regions = zeros((nregion, nt, nlats, nlons), dtype = bool) # region       
        for k in range(nfpu):
            fpus[k] = fpucommon == fpuidxs[k]
            areafpu[k, :, sidx, iidx] = (weight * area * fpus[k]).sum(axis = 2).sum(axis = 1)
        for k in range(nbasin):
            basins[k] = basincommon == basinidxs[k]
            areabasin[k, :, sidx, iidx] = (weight * area * basins[k]).sum(axis = 2).sum(axis = 1)
        for k in range(nregion):
            regions[k] = regioncommon == regionidxs[k]
            arearegion[k, :, sidx, iidx] = (weight * area * regions[k]).sum(axis = 2).sum(axis = 1)
        
        vtmpfpu = zeros((nfpu, nlats, nlons)) # for storing temporary variable
        vtmpbasin = zeros((nbasin, nlats, nlons))
        vtmpregion = zeros((nregion, nlats, nlons))
        for k in range(len(vars)):
            # iterate over variables
            t0 = tm.time()
            
            varfile = [f for f in files if re.search(scen_irr + '_' + vars[k], f)]
            if not len(varfile):
                continue
            else:
                varfile = varfile[0]
            print 'Processing', varfile, '. . .'
            
            # load file
            vf = nc(sep.join([rootdir, dirs[i], sep, varfile]))
            
            # get variable and units
            fvars = vf.variables.keys()
            vidx = [vars[k] in v for v in fvars].index(True)
            var = vf.variables[fvars[vidx]]
            if not j: vunits[k] = var.units if 'units' in var.ncattrs() else ''
            var = var[:].transpose((2, 0, 1))
            vf.close()
            
            print '  Processing aggregration masks . . .'
            for t in range(nt): 
                ridxfpu = areafpu[:, t, sidx, iidx] > 0. # fpu
                if ridxfpu.sum():
                    vtmpfpu[:] = 0.
                    idx1, idx2, idx3 = where(fpus[:, t, :, :])
                    vtmpfpu[idx1, idx2, idx3] = var[t, idx2, idx3] * weight[idx2, idx3] * area[idx2, idx3] * fpus[idx1, t, idx2, idx3]        
                    vsum = vtmpfpu.sum(axis = 2).sum(axis = 1)
                    varfpu[k, ridxfpu, t, sidx, iidx] = vsum[ridxfpu] / areafpu[ridxfpu, t, sidx, iidx]                        
                ridxbasin = areabasin[:, t, sidx, iidx] > 0. # basin
                if ridxbasin.sum():
                    vtmpbasin[:] = 0.
                    idx1, idx2, idx3 = where(basins[:, t, :, :])
                    vtmpbasin[idx1, idx2, idx3] = var[t, idx2, idx3] * weight[idx2, idx3] * area[idx2, idx3] * basins[idx1, t, idx2, idx3]        
                    vsum = vtmpbasin.sum(axis = 2).sum(axis = 1)
                    varbasin[k, ridxbasin, t, sidx, iidx] = vsum[ridxbasin] / areabasin[ridxbasin, t, sidx, iidx]                        
                ridxregion = arearegion[:, t, sidx, iidx] > 0. # region
                if ridxregion.sum():
                    vtmpregion[:] = 0.
                    idx1, idx2, idx3 = where(regions[:, t, :, :])
                    vtmpregion[idx1, idx2, idx3] = var[t, idx2, idx3] * weight[idx2, idx3] * area[idx2, idx3] * regions[idx1, t, idx2, idx3]        
                    vsum = vtmpregion.sum(axis = 2).sum(axis = 1)
                    varregion[k, ridxregion, t, sidx, iidx] = vsum[ridxregion] / arearegion[ridxregion, t, sidx, iidx]                                    
                                    
            print '  Time to process =', tm.time() - t0, 'seconds . . .'

    # append variables
    # areas
    f = nc(filename, 'a', format = 'NETCDF3_CLASSIC')
    afpuvar = f.createVariable('area_fpu', 'f4', ('fpu_index', 'time', 'scen', 'irr',)) # fpu
    afpuvar[:] = areafpu
    afpuvar.units = 'hectares'
    afpuvar.long_name = 'fpu harvested area'
    abasinvar = f.createVariable('area_basin', 'f4', ('basin_index', 'time', 'scen', 'irr',)) # basin
    abasinvar[:] = areabasin
    abasinvar.units = 'hectares'
    abasinvar.long_name = 'basin harvested area'
    aregionvar = f.createVariable('area_region', 'f4', ('region_index', 'time', 'scen', 'irr',)) # region
    aregionvar[:] = arearegion
    aregionvar.units = 'hectares'
    aregionvar.long_name = 'region harvested area'
    # averages
    for j in range(nv):
        v1 = f.createVariable(vars[j] + '_fpu', 'f4', ('fpu_index', 'time', 'scen', 'irr',), fill_value = fillv) # fpu
        v1[:] = varfpu[j]
        v1.units = vunits[j]
        v1.long_name = 'average fpu ' + vars[j]
        v2 = f.createVariable(vars[j] + '_basin', 'f4', ('basin_index', 'time', 'scen', 'irr',), fill_value = fillv) # basin
        v2[:] = varbasin[j]
        v2.units = vunits[j]
        v2.long_name = 'average basin ' + vars[j]
        v3 = f.createVariable(vars[j] + '_region', 'f4', ('region_index', 'time', 'scen', 'irr',), fill_value = fillv) # region
        v3[:] = varregion[j]
        v3.units = vunits[j]
        v3.long_name = 'average region ' + vars[j]
    f.close()