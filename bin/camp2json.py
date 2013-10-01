#!/usr/bin/env python

# import modules
from netCDF4 import Dataset as nc
from optparse import OptionParser
import re, json, copy, datetime as dt
from numpy import nan, isnan, double, resize, where

# UTILITY FUNCTIONS
def list_replace(arr, var, val, occ = nan, cnt = 1):
    for item in arr:
        if isinstance(item, list):
            cnt = list_replace(item, var, val, occ = occ, cnt = cnt)
        elif isinstance(item, dict):
            cnt = dict_replace(item, var, val, occ = occ, cnt = cnt)
    return cnt
def dict_replace(dic, var, val, occ = nan, cnt = 1):
    keys = dic.keys()
    for i in range(len(keys)):
        key = keys[i]
        item = dic[key]
        if isinstance(item, list):
            cnt = list_replace(item, var, val, occ = occ, cnt = cnt)
        elif isinstance(item, dict):
            cnt = dict_replace(item, var, val, occ = occ, cnt = cnt)
        elif key == var:
            if isnan(occ) or cnt == occ:
                dic[key] = val
            cnt += 1
    return cnt
def convert_var(var, val):
    if var == 'pdate':
        day = int(round(val))
        val = (dt.date(1900, 1, 1) + dt.timedelta(day - 1)).strftime('%e-%b')
    return val
def get_obj(dic, key, dft):
    return dic[key] if key in dic else dft

# parse inputs
parser = OptionParser()
parser.add_option("-c", "--campaign_file", dest = "campaignfile", default = "campaign.nc4", type = "string", 
                  help = "campaign netcdf4 file", metavar = "FILE")
parser.add_option("-e", "--exp_file", dest = "expfile", default = "expin.json", type = "string", 
                  help = "input experiment JSON file", metavar = "FILE")
parser.add_option("--latidx", dest = "latidx", default = 1, type = "string",
                  help = "Latitude coordinate")
parser.add_option("--lonidx", dest = "lonidx", default = 1, type = "string",
                  help = "Longitude coordinate")
parser.add_option("-d", "--delta", dest = "delta", default = 1, type = "float",
                  help = "Distance between each grid cell in arcminutes")
parser.add_option("-r", "--ref_year", dest = "ref_year", default = 1958, type = "int",
                  help = "Reference year from which to record times")
parser.add_option("-o", "--output", dest = "outputfile", default = "expout.json", type = "string",
                  help = "output experiment JSON file", metavar = "FILE")
(options, args) = parser.parse_args()

# open campaign netcdf4 file
campaign = nc(options.campaignfile, 'r',  format = 'NETCDF4')

# open experiment json file
template = json.load(open(options.expfile, 'r'))

# determine gridpoint with nearest latitude and longitude
delta = options.delta / 60. # convert from arcminutes to degrees
lat = campaign.variables['lat'][:]
lon = campaign.variables['lon'][:]
latd = resize(lat, (len(lon), len(lat))).T - 90. + delta * (int(options.latidx) - 0.5)
lond = resize(lon, (len(lat), len(lon))) + 180. - delta * (int(options.lonidx) - 0.5)
totd = latd**2 + lond**2
idx = where(totd == totd.min())
latidx = idx[0][0]
lonidx = idx[1][0]

# latitude and longitude
lat = lat[latidx]
lon = lon[lonidx]

# perform global replace
for attr in campaign.ncattrs():
    dict_replace(template, attr.lower(), campaign.getncattr(attr))
dict_replace(template, 'site_name', str(lat) + ', ' + str(lon))

# scenarios
scenarios = campaign.variables['scen'][:].astype(int)
num_scenarios = len(scenarios)

# duplicate experiment for each scenario
exp = {'experiments': []}
for i in range(num_scenarios):
    exp['experiments'].append(copy.deepcopy(template)) # need to deepcopy!

# get variables
variables = campaign.variables.keys()
variables.remove('lat') # remove lat, lon, scen
variables.remove('lon')
variables.remove('scen')

# replace trno
for i in range(num_scenarios): 
    dict_replace(exp['experiments'][i], 'trno', str(i + 1))

# iterate through variables
for var in variables:
    # get netCDF4 variable 
    v = campaign.variables[var]
    v.set_auto_maskandscale(False)

    # get dimensions
    dim = v.dimensions

    # get variable array
    if v.ndim == 1:
        if dim[0] == 'scen':
            var_array = v[:]
        else:
            raise Exception('Univariate data must have scenario as dimension')
    elif v.ndim == 2:
        if not 'lat' in dim:
            raise Exception('Latitude dimension is missing')
        if not 'lon' in dim:
            raise Exception('Longitude dimension is missing')
                
        if dim.index('lat') == 0:
            var_array = v[latidx, lonidx]
        else:
            var_array = v[lonidx, latidx]
        
        var_array = resize(var_array, (num_scenarios,)) # duplicate for all scenarios
    elif v.ndim == 3:
        if not 'scen' in dim:
            raise Exception('Scenario dimension is missing')
        if not 'lat' in dim:
            raise Exception('Latitude dimension is missing')
        if not 'lon' in dim:
            raise Exception('Longitude dimension is missing')
        
        scen_idx = dim.index('scen')
        lat_idx = dim.index('lat')
        lon_idx = dim.index('lon')
        
        if scen_idx == 0:
            if lat_idx == 1:
                var_array = v[:, latidx, lonidx] 
            else:
                var_array = v[:, lonidx, latidx]
        elif scen_idx == 1:
            if lat_idx == 0:
                var_array = v[latidx, :, lonidx]
            else:
                var_array = v[lonidx, :, latidx]
        else: # scen_idx == 2
            if lat_idx == 0:
                var_array = v[latidx, lonidx]
            else:
                var_array = v[lonidx, latidx]
    else:
         raise Exception('Data contain variables with improper dimensions')    

    # get attributes
    attrs = v.ncattrs()

    # get missing and fill values, if available
    fill_value = v._FillValue if '_FillValue' in attrs else nan
    missing_value = v.missing_value if 'missing_value' in attrs else nan
        
    # get mapping, if available
    is_mapping = False
    if 'units' in attrs and v.units == 'Mapping' and 'long_name' in attrs:
        mapping = v.long_name.split(',')
        is_mapping = True
    
    # get replacement number
    occ = nan # indicates to replace all instances of key
    nums = re.findall('\d+', var)
    if nums != []:
        var = re.sub('_\d+', '', var) # remove number
        occ = int(nums[0])

    # iterate over scenarios
    if len(var_array) != num_scenarios:
        raise Exception('Disagreement between variable length and number of scenarios!')
    for j in range(num_scenarios):
        val = var_array[j]
        
        if val == fill_value or val == missing_value:
            print 'Skipping fill/missing value', val, 'for variable', var, 'for occurrence =', occ
            continue
          
        if is_mapping:
            val = mapping[int(val - 1)]
        
        # convert variable to different representation, if necessary
        val_old = val
        val = convert_var(var, val)
 
        print 'Replacing variable', var, 'with', val, 'for occurrence =', occ
        dict_replace(exp['experiments'][j], var, str(val), occ = occ) # make sure val is str!

        # SPECIAL CASE FOR APSIM!
        # CHANGE EDATE TO PDATE - 60 days
        if var == 'pdate':
            eday = int(round(val_old)) - 60
            if eday < 1:
                eday = 365 + eday # non-leap year
            edate = (dt.date(1900, 1, 1) + dt.timedelta(eday - 1)).strftime('%e-%b')
            print 'Replacing variable edate with', edate, 'for occurrence =', occ
            dict_replace(exp['experiments'][j], 'edate', str(edate), occ = occ)

# change dates based on reference year
ref_year = options.ref_year
yer = ref_year % 100
list_replace(exp['experiments'], 'pfyer', str(yer))
list_replace(exp['experiments'], 'plyer', str(yer))
list_replace(exp['experiments'], 'hlyer', str(yer + 1))
list_replace(exp['experiments'], 'sdyer', str(yer))
list_replace(exp['experiments'], 'odyer', str(yer))
for e in exp['experiments']:
    man = get_obj(e, 'management', {})
    if man != {}:
        pdate = man['events'][0]['date']
        dict_replace(e, 'date', str(ref_year) + pdate[4 :], occ = 1) # planting date
        idate = man['events'][3]['date']
        dict_replace(e, 'date', str(ref_year) + idate[4 :], occ = 4) # irrigation date
        hdate = man['events'][4]['date']
        dict_replace(e, 'date', str(ref_year) + hdate[4 :], occ = 5) # harvest date
    soil = get_obj(e, 'soil', {})
    if soil != {}:
        sdate = soil['sadat']
        dict_replace(e, 'sadat', str(ref_year) + sdate[4 :])
    ic = get_obj(e, 'initial_conditions', {})
    if ic == {}:
        ic = get_obj(e, 'initial_condition', {})
    if ic != {}:
        icdate = ic['icdat']
        dict_replace(e, 'icdat', str(ref_year) + icdate[4 :])
    start_date = get_obj(e, 'start_date', '')
    if start_date != '':
        dict_replace(e, 'start_date', start_date[: 6] + str(ref_year))
    
# correct plyer, if available and necessary
for e in exp['experiments']:
    plt_dic = get_obj(e, 'dssat_simulation_control', {}) # only applies for DSSAT!
    if plt_dic != {}:
        planting = plt_dic['data'][0]['planting']
        pfday = planting['pfday']
        plday = planting['plday']
        if double(pfday) > double(plday):
            plyer = str(double(planting['pfyer']) + 1)
            print 'Setting plyer to', plyer
            dict_replace(e, 'plyer', plyer)

# close file
campaign.close()

# write experiment json file
json.dump(exp, open(options.outputfile, 'w'), indent = 2, separators = (',', ': '))