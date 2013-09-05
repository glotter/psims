#!/usr/bin/env python

# import modules
import json
import warnings
import datetime
from numpy import double
from optparse import OptionParser

# GENERAL PURPOSE FUNCTIONS
def for_str(vstr, nleading, vtype, vwidth, jtfy = 'r', ndec = 0, zero_pad = False):
    # vtype can be 'c' (character) or 'r' (real)
    # jfty can be 'r' (right) or 'l' (left)
    if vtype == 'c':
        if len(vstr) > vwidth:
            var = vstr[: vwidth]
        else:
            var = vstr
    elif vtype == 'r':
        var = ('{:.' + str(ndec) + 'f}').format(double(vstr))
        if len(var) > vwidth:
            warnings.warn('Real number is too long')
            var = var[: vwidth]
    else:
        raise Exception('Unknown variable type')
    if jtfy == 'r':
        var = var.zfill(vwidth) if zero_pad else var.rjust(vwidth)
    elif jtfy == 'l':
        var = var + '0' * (vwidth - len(var)) if zero_pad else var.ljust(vwidth) 
    else:
        raise Exception('Unknown jtfy method')   
    return nleading * ' ' + var
    
def for_field(dic, key, dft, nleading, vtype, vwidth, jtfy = 'r', ndec = 0):
    if key in dic:
        vstr = dic[key]
    else:
        vstr = dft
    return for_str(vstr, nleading, vtype, vwidth, jtfy, ndec)

class DSSATXFileOutput:
    # member variables
    def_val_R = '-99'
    def_val_C = '-99'
    def_val_I = '-99'
    def_val_D = '-99'
    def_val_blank = ''
    
    def __init__(self, json_file):
        data = json.load(open(json_file))
        self.exps = data['experiments']
        if not len(self.exps):
            raise Exception('No experiment data')
        
    def toXFile(self):
        if self.exps is None or self.exps == []:
            return
        
        # get defaults
        dR = self.def_val_R
        dC = self.def_val_C
        dI = self.def_val_I
        dD = self.def_val_D
        dblank = self.def_val_blank

        # pull meta data from first experiment
        exp0 = self.exps[0]
        exname = exp0['exname_o']
        locname = self.__get_obj(exp0, 'local_name', dblank)
        people = self.__get_obj(exp0, 'person_notes', dblank)
        address = self.__get_obj(exp0, 'institution', dblank)
        site = self.__get_obj(exp0, 'site_name', dblank)
        notes = self.__get_obj(exp0, 'tr_notes', dblank)

        # EXP.DETAILS section
        x_str = '*EXP.DETAILS: {:10s} {:60s}\n\n'.format(exname, locname)
        
        # GENERAL section
        x_str += '*GENERAL\n'
        # people
        if people != '':
            x_str += '@PEOPLE\n {:75s}\n'.format(people)
        # address
        if address != '':
            x_str += '@ADDRESS\n {:75s}\n'.format(address)
        # site
        if site != '':
            x_str += '@SITE\n {:75s}\n\n'.format(site)
        # plot information
        plot_ids = ['plta', 'pltr#', 'pltln', 'pldr', 'pltsp', 'pllay', \
                    'pltha', 'plth#', 'plthl', 'plthm']
        has_plot = False
        for p in plot_ids:
            if self.__get_obj(exp0, p, '') != '':
                has_plot = True
                break
        if has_plot:
            x_str += '@ PAREA  PRNO  PLEN  PLDR  PLSP  PLAY HAREA  HRNO  HLEN  HARM.........\n'
            x_str += for_field(exp0, 'plta', dR, 3, 'r', 6, ndec = 1) + \
                     for_field(exp0, 'pltr#', dI, 1, 'r', 5) + \
                     for_field(exp0, 'pltln', dR, 1, 'r', 5, ndec = 1) + \
                     for_field(exp0, 'pldr', dI, 1, 'r', 5) + \
                     for_field(exp0, 'pltsp', dI, 1, 'r', 5) + \
                     for_field(exp0, 'pllay', dC, 1, 'c', 5) + \
                     for_field(exp0, 'pltha', dR, 1, 'r', 5, ndec = 1) + \
                     for_field(exp0, 'plth#', dI, 1, 'r', 5) + \
                     for_field(exp0, 'plthl', dR, 1, 'r', 5, ndec = 1) + \
                     for_field(exp0, 'plthm', dC, 1, 'c', 15) + '\n'
        # notes
        if notes != '':
            x_str += '@NOTES\n {:75s}\n\n'.format(notes)
        
        # TREATMENTS section
        x_str += '*TREATMENTS                        -------------FACTOR LEVELS------------\n'
        x_str += '@N R O C TNAME.................... CU FL SA IC MP MI MF MR MC MT ME MH SM\n'
        nexp = len(self.exps)
        
        cu_arr = []; fl_arr = []; sa_arr = []
        ic_arr = []; mp_arr = []; mi_arr = []
        mf_arr = []; mr_arr = []; mc_arr = []
        mt_arr = []; me_arr = []; mh_arr = []
        sm_arr = []
        
        for i in range(nexp):            
            sq_arr = self.__get_list(self.exps[i], 'dssat_sequence', 'data')
            evt_arr = self.__get_list(self.exps[i], 'management', 'events')
            root_arr = self.__get_obj(self.exps[i], 'dssat_root', [])
            me_org_arr = self.__get_list(self.exps[i], 'dssat_environment_modification', 'data')
            sm_org_arr = self.__get_list(self.exps[i], 'dssat_simulation_control', 'data')
            soil_arr = self.__read_sw_data(self.exps[i], 'soil')
            
            for j in range(len(sq_arr)):
                sq_data = sq_arr[j]
            
                seq_id = self.__get_obj(sq_data, 'seqid', dblank)  
                em = self.__get_obj(sq_data, 'em', dblank)
                sm = self.__get_obj(sq_data, 'sm', dblank)
            
                cu_data = {}; fl_data = {}; mp_data = {}; sm_data = {}
                mi_sub_arr = []; mf_sub_arr = []; mr_sub_arr = []
                mc_sub_arr = []; mt_sub_arr = []; me_sub_arr = []
                mh_sub_arr = []
                
                if j < len(soil_arr):
                    soil_data = soil_arr[j]
                elif soil_arr == []:
                    soil_data = {}
                else:
                    soil_data = soil_arr[0]
                if soil_data is None:
                    soil_data = {}
                
                if j < len(root_arr):
                    root_data = root_arr[j]
                else:
                    root_data = self.exps[i]
                    
                # set field info
                self.__copy_item(fl_data, root_data, 'id_field')
                fl_data['wst_id'] = root_data['wst_id']
                self.__copy_item(fl_data, root_data, 'flsl')
                self.__copy_item(fl_data, root_data, 'flob')
                self.__copy_item(fl_data, root_data, 'fl_drntype')
                self.__copy_item(fl_data, root_data, 'fldrd')
                self.__copy_item(fl_data, root_data, 'fldrs')
                self.__copy_item(fl_data, root_data, 'flst')
                if 'sltx' in soil_data:
                    self.__copy_item(fl_data, soil_data, 'sltx')
                else:
                    self.__copy_item(fl_data, root_data, 'sltx')
                self.__copy_item(fl_data, soil_data, 'sldp')
                self.__copy_item(fl_data, root_data, 'soil_id')
                self.__copy_item(fl_data, root_data, 'fl_name')
                self.__copy_item(fl_data, root_data, 'fl_lat')
                self.__copy_item(fl_data, root_data, 'fl_long')
                self.__copy_item(fl_data, root_data, 'flele')
                self.__copy_item(fl_data, root_data, 'farea')
                self.__copy_item(fl_data, root_data, 'fllwr')
                self.__copy_item(fl_data, root_data, 'flsla')
                self.__copy_item(fl_data, self.__get_obj(root_data, 'dssat_info', {}), 'flhst')
                self.__copy_item(fl_data, self.__get_obj(root_data, 'dssat_info', {}), 'fhdur')
                soil_id = self.__get_obj(fl_data, 'soil_id', '')
                if len(soil_id) > 10 and soil_id.find('_') != -1:
                    idx = soil_id.find('_')
                    fl_data['soil_id'] = soil_id[: idx]
                fl_num = self.__set_sec_data(fl_data, fl_arr)
                
                # set initial condition info
                ic_data = self.__get_obj(root_data, 'initial_conditions', {})
                ic_num = self.__set_sec_data(ic_data, ic_arr)
                
                # set environment modification info
                for k in range(len(me_org_arr)):
                    if me_org_arr[k]['em'] == em:
                        tmp = {}
                        tmp.update(me_org_arr[k])
                        tmp.pop('em')
                        me_sub_arr.append(tmp)
                        
                # set soil analysis info
                soil_layers = self.__get_obj(soil_data, 'soilLayer', [])
                has_soil_analysis = False
                for k in range(len(soil_layers)):
                    if self.__get_obj(soil_layers[k], 'slsc', '') != '':
                        has_soil_analysis = True
                        break
                if has_soil_analysis:
                    sa_data = {}
                    sa_sub_arr = []
                    for k in range(len(soil_layers)):
                        sa_sub_data = {}
                        self.__copy_item(sa_sub_data, soil_layers[k], 'sabl', 'sllb')
                        self.__copy_item(sa_sub_data, soil_layers[k], 'sasc', 'slsc')
                        sa_sub_arr.append(sa_sub_data)
                    self.__copy_item(sa_data, soil_data, 'sadat')
                    sa_data['soilLayer'] = sa_sub_arr
                    sa_num = self.__set_sec_data(sa_data, sa_arr)
                else:
                    sa_num = 0
                    
                # set simulation control info
                for k in range(len(sm_org_arr)):
                    if sm_org_arr[k]['sm'] == sm:
                        sm_data.update(sm_org_arr[k])
                        sm_data.pop('sm')
                        break
                        
                self.__copy_item(sm_data, root_data, 'sdat')
                
                # loop through all events
                for k in range(len(evt_arr)):
                    evt_data = {}
                    evt_data.update(evt_arr[k])
                    
                    if self.__get_obj(evt_data, 'seqid', dblank) == seq_id:
                        evt_data.pop('seqid')
                        
                        if self.__get_obj(evt_data, 'event', dblank) == 'planting':
                            # planting
                            self.__copy_item(cu_data, evt_data, 'cul_name')
                            self.__copy_item(cu_data, evt_data, 'crid')
                            self.__copy_item(cu_data, evt_data, 'cul_id')
                            self.__copy_item(cu_data, evt_data, 'dssat_cul_id')
                            self.__copy_item(cu_data, evt_data, 'rm')
                            self.__copy_item(cu_data, evt_data, 'cul_notes')
                            # TODO: Translate CRID?
                            mp_data.update(evt_data)
                            mp_data.pop('cul_name')
                        elif self.__get_obj(evt_data, 'event', dblank) == 'irrigation':
                            # irrigation
                            mi_sub_arr.append(evt_data)
                        elif self.__get_obj(evt_data, 'event', dblank) == 'fertilizer':
                            # fertilizer
                            mf_sub_arr.append(evt_data)
                        elif self.__get_obj(evt_data, 'event', dblank) == 'organic_matter':
                           # organic matter
                            mr_sub_arr.append(evt_data)
                        elif self.__get_obj(evt_data, 'event', dblank) == 'chemical':
                            # chemical
                            mc_sub_arr.append(evt_data)
                        elif self.__get_obj(evt_data, 'event', dblank) == 'tillage':
                            # tillage
                            mt_sub_arr.append(evt_data)
                        elif self.__get_obj(evt_data, 'event', dblank) == 'harvest':
                            mh_sub_arr.append(evt_data)
                            if self.__get_obj(evt_data, 'date', '').strip() != '':
                                sm_data['hadat_valid'] = 'Y'
                                
                cu_num = self.__set_sec_data(cu_data, cu_arr)
                mp_num = self.__set_sec_data(mp_data, mp_arr)
                mi_num = self.__set_sec_data(mi_sub_arr, mi_arr)
                mf_num = self.__set_sec_data(mf_sub_arr, mf_arr)
                mr_num = self.__set_sec_data(mr_sub_arr, mr_arr)
                mc_num = self.__set_sec_data(mc_sub_arr, mc_arr)
                mt_num = self.__set_sec_data(mt_sub_arr, mt_arr)
                me_num = self.__set_sec_data(me_sub_arr, me_arr)
                mh_num = self.__set_sec_data(mh_sub_arr, mh_arr)
                sm_num = self.__set_sec_data(sm_data, sm_arr)
                if not sm_num:
                    sm_num = 1
                
                x_str += for_field(sq_data, 'trno', '1', 0, 'r', 2) + \
                         for_field(sq_data, 'sq', '1', 1, 'r', 1) + \
                         for_field(sq_data, 'op', '1', 1, 'r', 1) + \
                         for_field(sq_data, 'co', '0', 1, 'r', 1) + \
                         for_field(sq_data, 'trt_name', '', 1, 'c', 25, jtfy = 'l') + \
                         for_str(str(cu_num), 1, 'r', 2) + \
                         for_str(str(fl_num), 1, 'r', 2) + \
                         for_str(str(sa_num), 1, 'r', 2) + \
                         for_str(str(ic_num), 1, 'r', 2) + \
                         for_str(str(mp_num), 1, 'r', 2) + \
                         for_str(str(mi_num), 1, 'r', 2) + \
                         for_str(str(mf_num), 1, 'r', 2) + \
                         for_str(str(mr_num), 1, 'r', 2) + \
                         for_str(str(mc_num), 1, 'r', 2) + \
                         for_str(str(mt_num), 1, 'r', 2) + \
                         for_str(str(me_num), 1, 'r', 2) + \
                         for_str(str(mh_num), 1, 'r', 2) + \
                         for_str(str(sm_num), 1, 'r', 2) + '\n'
        
        x_str += '\n'
        
        # CULTIVARS section
        if cu_arr != []:
            x_str += '*CULTIVARS\n'
            x_str += '@C CR INGENO CNAME\n'
            for i in range(len(cu_arr)):
                sec_data = cu_arr[i]
                crid = self.__get_obj(sec_data, 'crid', '')
                if crid == '':
                    warnings.warn('Cultivar CRID is missing')
                # MAY NEED THIS LATER!
                # def_cul_id = self.__get_obj(sec_data, 'cul_id', dC)
                # for_field(sec_data, 'dssat_cul_id', def_cul_id, 1, 'c', 6)
                x_str += for_str(str(i + 1), 0, 'r', 2) + \
                         for_field(sec_data, 'crid', dblank, 1, 'c', 2) + \
                         for_field(sec_data, 'cul_id', dC, 1, 'c', 6) + \
                         for_field(sec_data, 'cul_name', dC, 1, 'c', 16, jtfy = 'l') + '\n'
                if self.__get_obj(sec_data, 'rm', '') != '' or self.__get_obj(sec_data, 'cul_notes', '') != '':
                    # SKIP CULTIVAR NOTES FOR NOW
                    pass

            x_str += '\n'  
        else:
            warnings.warn('Cultivar information is missing')
            
        # FIELDS section
        if fl_arr != []:
            x_str += '*FIELDS\n'
            x_str += '@L ID_FIELD WSTA....  FLSA  FLOB  FLDT  FLDD  FLDS  FLST SLTX  SLDP  ID_SOIL    FLNAME\n'
            event_part2 = '@L ...........XCRD ...........YCRD .....ELEV .............AREA .SLEN .FLWR .SLAS FLHST FHDUR\n'
        else:
            warnings.warn('Field information is missing')
        for i in range(len(fl_arr)):
            sec_data = fl_arr[i]
            if self.__get_obj(sec_data, 'wst_id', '') == '':
                warnings.warn('Field WST_ID is missing')
            soil_id = self.__get_obj(sec_data, 'soil_id', dC)
            if soil_id == '':
                warnings.warn('Field SOIL_ID is missing')
            elif len(soil_id) > 10:
                warnings.warn('Oversized field SOIL_ID')
            x_str += for_str(str(i + 1), 0, 'r', 2) + \
                     for_field(sec_data, 'id_field', dC, 1, 'c', 8, jtfy = 'l') + \
                     for_field(sec_data, 'wst_id', dC, 1, 'c', 8, jtfy = 'l') + \
                     for_field(sec_data, 'flsl', dD, 1, 'c', 5) + \
                     for_field(sec_data, 'flob', dR, 1, 'r', 5) + \
                     for_field(sec_data, 'fl_drntype', dC, 1, 'c', 5, jtfy = 'l') + \
                     for_field(sec_data, 'fldrd', dR, 1, 'r', 5) + \
                     for_field(sec_data, 'fldrs', dR, 1, 'r', 5) + \
                     for_field(sec_data, 'flst', dC, 1, 'c', 5, jtfy = 'l') + \
                     for_field(sec_data, 'sltx', dD, 1, 'c', 5, jtfy = 'l') + \
                     for_field(sec_data, 'sldp', dR, 1, 'r', 5) + \
                     for_str(str(soil_id), 1, 'c', 10, jtfy = 'l') + \
                     ' ' + self.__get_obj(sec_data, 'fl_name', dC) + '\n'
            event_part2 += for_str(str(i + 1), 0, 'r', 2) + \
                           for_field(sec_data, 'fl_long', dR, 1, 'r', 15, ndec = 2) + \
                           for_field(sec_data, 'fl_lat', dR, 1, 'r', 15, ndec = 2) + \
                           for_field(sec_data, 'flele', dR, 1, 'r', 9) + \
                           for_field(sec_data, 'farea', dR, 1, 'r', 17) + \
                           ' -99  ' + \
                           for_field(sec_data, 'fllwr', dD, 1, 'r', 5) + \
                           for_field(sec_data, 'flsla', dD, 1, 'r', 5) + \
                           for_field(sec_data, 'flhst', dD, 1, 'c', 5) + \
                           for_field(sec_data, 'fhdur', dD, 1, 'r', 5) + '\n'
        if fl_arr != []:
            x_str += event_part2 + '\n'
            
        # SOIL ANALYSIS section
        if sa_arr != []:
            x_str += '*SOIL ANALYSIS\n'
            for i in range(len(sa_arr)):
                sec_data = sa_arr[i]
                x_str += '@A SADAT  SMHB  SMPX  SMKE  SANAME\n'
                x_str += for_str(str(i + 1), 0, 'r', 2) + \
                         for_field(sec_data, 'sadat', dR, 1, 'r', 5) + \
                         for_field(sec_data, 'samhb', dC, 1, 'c', 5) + \
                         for_field(sec_data, 'sampx', dC, 1, 'c', 5) + \
                         for_field(sec_data, 'samke', dC, 1, 'c', 5) + \
                         ' ' + self.__get_obj(sec_data, 'sa_name', dC)
                sub_data_arr = self.__get_obj(sec_data, 'soilLayer', [])
                if sub_data_arr != []:
                    x_str += '@A  SABL  SADM  SAOC  SANI SAPHW SAPHB  SAPX  SAKE  SASC\n'
                for j in range(len(sub_data_arr)):
                    sub_data = sub_data_arr[j]
                    x_str += for_str(str(i + 1), 0, 'r', 2) + \
                             for_field(sub_data, 'sabl', dR, 1, 'r', 5) + \
                             for_field(sub_data, 'sabdm', dR, 1, 'r', 5, ndec = 1) + \
                             for_field(sub_data, 'saoc', dR, 1, 'r', 5, ndec = 2) + \
                             for_field(sub_data, 'sani', dR, 1, 'r', 5, ndec = 2) + \
                             for_field(sub_data, 'saphw', dR, 1, 'r', 5, ndec = 1) + \
                             for_field(sub_data, 'saphb', dR, 1, 'r', 5, ndec = 1) + \
                             for_field(sub_data, 'sapx', dR, 1, 'r', 5, ndec = 1) + \
                             for_field(sub_data, 'sake', dR, 1, 'r', 5, ndec = 1) + \
                             for_field(sub_data, 'sasc', dR, 1, 'r', 5) + '\n'
        
        # INITIAL CONDITIONS section
        if ic_arr != []:
            x_str += '*INITIAL CONDITIONS\n'
            for i in range(len(ic_arr)):
                sec_data = ic_arr[i]
                icdat = self.__translate_date_str(self.__get_obj(sec_data, 'icdat', dD))
                x_str += '@C   PCR ICDAT  ICRT  ICND  ICRN  ICRE  ICWD ICRES ICREN ICREP ICRIP ICRID ICNAME\n'
                x_str += for_str(str(i + 1), 0, 'r', 2) + \
                         for_field(sec_data, 'icpcr', dC, 1, 'c', 5) + \
                         for_str(icdat, 1, 'c', 5) + \
                         for_field(sec_data, 'icrt', dR, 1, 'r', 5) + \
                         for_field(sec_data, 'icnd', dR, 1, 'r', 5) + \
                         for_field(sec_data, 'icrz#', dR, 1, 'r', 5) + \
                         for_field(sec_data, 'icrze', dR, 1, 'r', 5) + \
                         for_field(sec_data, 'icwt', dR, 1, 'r', 5) + \
                         for_field(sec_data, 'icrag', dR, 1, 'r', 5) + \
                         for_field(sec_data, 'icrn', dR, 1, 'r', 5, ndec = 2) + \
                         for_field(sec_data, 'icrp', dR, 1, 'r', 5, ndec = 2) + \
                         for_field(sec_data, 'icrip', dR, 1, 'r', 5) + \
                         for_field(sec_data, 'icrdp', dR, 1, 'r', 5) + \
                         ' ' + self.__get_obj(sec_data, 'ic_name', dR) + '\n'
                sub_data_arr = self.__get_obj(sec_data, 'soilLayer', [])
                if sub_data_arr != []:
                    x_str += '@C  ICBL  SH2O  SNH4  SNO3\n'
                for j in range(len(sub_data_arr)):
                    sub_data = sub_data_arr[j]
                    x_str += for_str(str(i + 1), 0, 'r', 2) + \
                             for_field(sub_data, 'icbl', dR, 1, 'r', 5) + \
                             for_field(sub_data, 'ich2o', dR, 1, 'r', 5, ndec = 3) + \
                             for_field(sub_data, 'icnh4', dR, 1, 'c', 5) + \
                             for_field(sub_data, 'icno3', dR, 1, 'r', 5, ndec = 1) + '\n'
            x_str += '\n'
            
        # PLANTING DETAILS section
        if mp_arr != []:
            x_str += '*PLANTING DETAILS\n'
            x_str += '@P PDATE EDATE  PPOP  PPOE  PLME  PLDS  PLRS  PLRD  PLDP  PLWT  PAGE  PENV  PLPH  SPRL                        PLNAME\n'
            for i in range(len(mp_arr)):
                sec_data = mp_arr[i]
                pdate = self.__get_obj(sec_data, 'date', '')
                if pdate == '':
                    warnings.warn('Planting PDATE is missing')
                if self.__get_obj(sec_data, 'plpoe', '') == '':
                    warnings.warn('Planting PLPOE is missing')
                if self.__get_obj(sec_data, 'plrs', '') == '':
                    warnings.warn('Planting PLRS is missing')
                # convert from mm to cm
                pldp = self.__get_obj(sec_data, 'pldp', '')
                if pldp != '':
                    sec_data['pldp'] = str(double(pldp) / 10.)
                date = self.__translate_date_str(self.__get_obj(sec_data, 'date', dD))
                pldae = self.__translate_date_str(self.__get_obj(sec_data, 'pldae', dD))
                plpoe = self.__get_obj(sec_data, 'plpoe', dR)
                plpop = self.__get_obj(sec_data, 'plpop', dR)
                x_str += for_str(str(i + 1), 0, 'r', 2) + \
                         for_str(date, 1, 'c', 5) + \
                         for_str(pldae, 1, 'c', 5) + \
                         for_field(sec_data, 'plpop', plpoe, 1, 'r', 5, ndec = 1) + \
                         for_field(sec_data, 'plpoe', plpop, 1, 'r', 5, ndec = 1) + \
                         for_field(sec_data, 'plma', dC, 5, 'c', 1, jtfy = 'l') + \
                         for_field(sec_data, 'plds', dC, 5, 'c', 1, jtfy = 'l') + \
                         for_field(sec_data, 'plrs', dR, 1, 'r', 5) + \
                         for_field(sec_data, 'plrd', dR, 1, 'r', 5) + \
                         for_field(sec_data, 'pldp', dR, 1, 'r', 5, ndec = 1) + \
                         for_field(sec_data, 'plmwt', dR, 1, 'r', 5) + \
                         for_field(sec_data, 'page', dR, 1, 'r', 5) + \
                         for_field(sec_data, 'plenv', dR, 1, 'r', 5, ndec = 1) + \
                         for_field(sec_data, 'plph', dR, 1, 'r', 5, ndec = 1) + \
                         for_field(sec_data, 'plspl', dR, 1, 'r', 5) + \
                         ' ' * 24 + self.__get_obj(sec_data, 'pl_name', dC) + '\n'
            x_str += '\n'
        else:
            warnings.warn('Planting information is missing')
            
        # IRRIGATION AND WATER MANAGEMENT section
        if mi_arr != []:
            x_str += '*IRRIGATION AND WATER MANAGEMENT\n'
            for i in range(len(mi_arr)):
                sub_data_arr = mi_arr[i]
                if sub_data_arr != []:
                    sub_data = sub_data_arr[0]
                else:
                    sub_data = {}
                x_str += '@I  EFIR  IDEP  ITHR  IEPT  IOFF  IAME  IAMT IRNAME\n'
                x_str += for_str(str(i + 1), 0, 'r', 2) + \
                         for_field(sub_data, 'ireff', dR, 1, 'r', 5, ndec = 2) + \
                         for_field(sub_data, 'irmdp', dR, 1, 'r', 5) + \
                         for_field(sub_data, 'irthr', dR, 1, 'r', 5) + \
                         for_field(sub_data, 'irept', dR, 1, 'r', 5) + \
                         for_field(sub_data, 'irstg', dR, 1, 'c', 5) + \
                         for_field(sub_data, 'iame', dR, 1, 'c', 5) + \
                         for_field(sub_data, 'iamt', dR, 1, 'r', 5) + \
                         ' ' + self.__get_obj(sub_data, 'ir_name', dC) + '\n'
                if sub_data_arr != []:
                    x_str += '@I IDATE  IROP IRVAL\n'
                for j in range(len(sub_data_arr)):
                    sub_data = sub_data_arr[j]
                    date = self.__translate_date_str(self.__get_obj(sub_data, 'date', dC))
                    x_str += for_str(str(i + 1), 0, 'r', 2) + \
                             for_str(date, 1, 'c', 5) + \
                             for_field(sub_data, 'irop', dC, 1, 'c', 5) + \
                             for_field(sub_data, 'irval', dR, 1, 'r', 5) + '\n'
            x_str += '\n'
            
        # FERTILIZERS section
        if mf_arr != []:
            x_str += '*FERTILIZERS (INORGANIC)\n'
            x_str += '@F FDATE  FMCD  FACD  FDEP  FAMN  FAMP  FAMK  FAMC  FAMO  FOCD FERNAME\n'
            for i in range(len(mf_arr)):
                sec_data_arr = mf_arr[i]
                for j in range(len(sec_data_arr)):
                    sec_data = sec_data_arr[j]
                    date = self.__translate_date_str(self.__get_obj(sec_data, 'date', dD))
                    x_str += for_str(str(i + 1), 0, 'r', 2) + \
                             for_str(date, 1, 'c', 5) + \
                             for_field(sec_data, 'fecd', dC, 1, 'c', 5) + \
                             for_field(sec_data, 'feacd', dC, 1, 'c', 5) + \
                             for_field(sec_data, 'fedep', dR, 1, 'r', 5) + \
                             for_field(sec_data, 'feamn', dR, 1, 'r', 5) + \
                             for_field(sec_data, 'feamp', dR, 1, 'r', 5) + \
                             for_field(sec_data, 'feamk', dR, 1, 'r', 5) + \
                             for_field(sec_data, 'feamc', dR, 1, 'r', 5) + \
                             for_field(sec_data, 'feamo', dR, 1, 'r', 5) + \
                             for_field(sec_data, 'feocd', dR, 1, 'c', 5) + \
                             ' ' + self.__get_obj(sec_data, 'fe_name', dC) + '\n'
            x_str += '\n'
            
        # RESIDUES AND ORGANIC FERTILIZER section
        if mr_arr != []:
            x_str += '*RESIDUES AND ORGANIC FERTILIZER\n'
            x_str += '@R RDATE  RCOD  RAMT  RESN  RESP  RESK  RINP  RDEP  RMET RENAME\n'
            for i in range(len(mr_arr)):
                sec_data_arr = mr_arr[i]
                for j in range(len(sec_data_arr)):
                    sec_data = sec_data_arr[j]
                    date = self.__translate_date_str(self.__get_obj(sec_data, 'date', dD))
                    x_str += for_str(str(i + 1), 0, 'r', 2) + \
                             for_str(date, 1, 'c', 5) + \
                             for_field(sec_data, 'omcd', dC, 1, 'c', 5) + \
                             for_field(sec_data, 'omamt', dR, 1, 'r', 5) + \
                             for_field(sec_data, 'omn%', dR, 1, 'r', 5) + \
                             for_field(sec_data, 'omp%', dR, 1, 'r', 5) + \
                             for_field(sec_data, 'omk%', dR, 1, 'r', 5) + \
                             for_field(sec_data, 'ominp', dR, 1, 'r', 5) + \
                             for_field(sec_data, 'omdep', dR, 1, 'r', 5) + \
                             for_field(sec_data, 'omacd', dR, 1, 'r', 5) + \
                             ' ' + self.__get_obj(sec_data, 'on_name', dC) + '\n'
            x_str += '\n'
        
        # CHEMICAL APPLICATIONS section
        if mc_arr != []:
            x_str += '*CHEMICAL APPLICATIONS\n'
            x_str += '@C CDATE CHCOD CHAMT  CHME CHDEP   CHT..CHNAME\n'
            for i in range(len(mc_arr)):
                sec_data_arr = mc_arr[i]
                for j in range(len(sec_data_arr)):
                    sec_data = sec_data_arr[j]
                    date = self.__translate_date_str(self.__get_obj(sec_data, 'date', dD))
                    x_str += for_str(str(i + 1), 0, 'r', 2) + \
                             for_str(date, 1, 'c', 5) + \
                             for_field(sec_data, 'chcd', dC, 1, 'c', 5) + \
                             for_field(sec_data, 'chamt', dR, 1, 'r', 5) + \
                             for_field(sec_data, 'chacd', dC, 1, 'c', 5) + \
                             for_field(sec_data, 'chdep', dC, 1, 'c', 5) + \
                             for_field(sec_data, 'ch_targets', dC, 1, 'c', 5) + \
                             '  ' + self.__get_obj(sec_data, 'ch_name', dC) + '\n'
            x_str += '\n'
        
        # TILLAGE section
        if mt_arr != []:
            x_str += '*TILLAGE AND ROTATIONS\n'
            x_str += '@T TDATE TIMPL  TDEP TNAME\n'
            for i in range(len(mt_arr)):
                sec_data_arr = mt_arr[i]
                for j in range(len(sec_data_arr)):
                    sec_data = sec_data_arr[j]
                    date = self.__translate_date_str(self.__get_obj(sec_data, 'date', dD))
                    x_str += for_str(str(i + 1), 0, 'r', 2) + \
                             for_str(date, 1, 'c', 5) + \
                             for_field(sec_data, 'tiimp', dC, 1, 'c', 5) + \
                             for_field(sec_data, 'tidep', dR, 1, 'c', 5) + \
                             ' ' + self.__get_obj(sec_data, 'ti_name', dC) + '\n'
            x_str += '\n'
        
        # ENVIRONMENT MODIFICATIONS section
        if me_arr != []:
            x_str += '*ENVIRONMENT MODIFICATIONS\n'
            x_str += '@E ODATE EDAY  ERAD  EMAX  EMIN  ERAIN ECO2  EDEW  EWIND ENVNAME\n'
            for i in range(len(me_arr)):
                sec_data_arr = me_arr[i]
                for j in range(len(sec_data_arr)):
                    sec_data = sec_data_arr[j]['data']
                    odyer = self.__get_obj(sec_data, 'odyer', dC).split('.')[0] # remove decimal part
                    odyer = for_str(odyer, 1, 'c', 2, zero_pad = True)
                    odday = self.__get_obj(sec_data, 'odday', dC).split('.')[0]
                    odday = for_str(odday, 0, 'c', 3, zero_pad = True)
                    x_str += for_str(str(i + 1), 0, 'r', 2) + \
                             odyer + odday + \
                             for_field(sec_data, 'eday', dC, 1, 'c', 5) + \
                             for_field(sec_data, 'erad', dC, 1, 'c', 5) + \
                             for_field(sec_data, 'emax', dC, 1, 'c', 5) + \
                             for_field(sec_data, 'emin', dC, 1, 'c', 5) + \
                             for_field(sec_data, 'erain', dC, 1, 'c', 5) + \
                             for_field(sec_data, 'eco2', dC, 1, 'c', 5) + \
                             for_field(sec_data, 'edew', dC, 1, 'c', 5) + \
                             for_field(sec_data, 'ewind', dC, 1, 'c', 5) + \
                             ' ' + self.__get_obj(sec_data, 'envnam', dC) + '\n'                             
            x_str += '\n'
            
        # HARVEST DETAILS section
        if mh_arr != []:
            x_str += '*HARVEST DETAILS\n'
            x_str += '@H HDATE  HSTG  HCOM HSIZE   HPC  HBPC HNAME\n'
            for i in range(len(mh_arr)):
                sec_data_arr = mh_arr[i]
                for j in range(len(sec_data_arr)):
                    sec_data = sec_data_arr[j]
                    date = self.__translate_date_str(self.__get_obj(sec_data, 'date', dD))
                    x_str += for_str(str(i + 1), 0, 'r', 2) + \
                             for_str(date, 1, 'c', 5) + \
                             for_field(sec_data, 'hastg', dC, 1, 'c', 5, jtfy = 'l') + \
                             for_field(sec_data, 'hacom', dC, 1, 'c', 5, jtfy = 'l') + \
                             for_field(sec_data, 'hasiz', dC, 1, 'c', 5, jtfy = 'l') + \
                             for_field(sec_data, 'hap%', dR, 1, 'c', 5) + \
                             for_field(sec_data, 'hab%', dR, 1, 'c', 5) + \
                             ' ' + self.__get_obj(sec_data, 'ha_name', dC) + '\n'
            x_str += '\n'
            
        # SIMULATION CONTROLS and AUTOMATIC MANAGEMENT section
        if sm_arr != []:
            x_str += '*SIMULATION CONTROLS\n'
            for i in range(len(sm_arr)):
                sec_data = sm_arr[i]
                x_str += self.__create_SMMA_str(i + 1, sec_data)
                if i != len(sm_arr) - 1:
                    x_str += '\n\n'
        else:
            x_str += '*SIMULATION CONTROLS\n'
            x_str += self.__create_SMMA_str(1, {})
        
        return x_str
        
    def __read_sw_data(self, data, key):
        ret = []
        d = data[key]
        if not d is None:
            if isinstance(d, list):
                ret = d
            else:
                ret = []
                ret.append(d)
        else:
            ret = []
        return ret

    def __get_obj(self, dic, key, dft):
        # gets actual object, NOT copy
        if key in dic:
            return dic[key]
        else:
            return dft
    
    def __get_list(self, data, block_name, list_name):
        block = self.__get_obj(data, block_name, {})
        return self.__get_obj(block, list_name, [])
    
    def __copy_item(self, to, frm, to_key, frm_key = None, delete_flg = False):
        if frm_key is None:
            frm_key = to_key
        if frm_key in frm and not frm[frm_key] is None:
            if delete_flg:
                to[to_key] = frm.pop(frm_key)
            else:
                to[to_key] = frm[frm_key]
    
    def __set_sec_data(self, m, arr):
        if m != {} and m != []:
            for j in range(len(arr)):
                if arr[j] == m:
                    return j + 1
            arr.append(m)
            return len(arr)
        else:
            return 0
            
    def __translate_date_str(self, date_str):
        # convert from yyyymmdd to yyddd
        if len(date_str) < 8:
            return date_str
        year = int(date_str[: 4])
        month = int(date_str[4 : 6])
        day = int(date_str[6 : 8])
        return datetime.date(year, month, day).strftime('%y%j')
        
    def __create_SMMA_str(self, smid, tr_data):
        # get default
        dC = self.def_val_C
                        
        nitro = 'Y'
        water = 'Y'
        co2 = 'M'
        har_opt = 'M'
        sm = '{:2d}'.format(smid)
        
        co2y = self.__get_obj(tr_data, 'co2y', '').strip()
        if co2y != '' and not co2y.startswith('-'):
            co2 = 'W'
        
        sdate = self.__get_obj(tr_data, 'sdat', '')
        if sdate == '':
            sub_data = self.__get_obj(tr_data, 'planting', {})
            sdate = self.__get_obj(sub_data, 'date', self.def_val_D)
        sdate = self.__translate_date_str(sdate)
        sdate = '{:5s}'.format(sdate)
        
        if self.__get_obj(tr_data, 'hadat_valid', '').strip() != '':
            har_opt = 'R'
        
        # GENERAL
        sb = '@N GENERAL     NYERS NREPS START SDATE RSEED SNAME.................... SMODEL\n'
        sm_str = self.__get_obj(tr_data, 'general', {})
        if sm_str != {}:
            if sdate.strip() != '-99' and sdate.strip() != '':
                sm_str['sdyer'] = sdate[: 2]
                sm_str['sdday'] = sdate[2 : 5]
            sdyer = self.__get_obj(sm_str, 'sdyer', dC).split('.')[0] # remove decimal part
            sdyer = for_str(sdyer, 1, 'c', 2, zero_pad = True)
            sdday = self.__get_obj(sm_str, 'sdday', dC).split('.')[0]
            sdday = for_str(sdday, 0, 'c', 3, zero_pad = True)            
            sb += for_str(sm, 0, 'c', 2) + \
                  for_str('GE', 1, 'c', 11, jtfy = 'l') + \
                  for_field(sm_str, 'nyers', dC, 4, 'c', 2) + \
                  for_field(sm_str, 'nreps', dC, 4, 'c', 2) + \
                  for_field(sm_str, 'start', dC, 5, 'c', 1) + \
                  sdyer + sdday + \
                  for_field(sm_str, 'rseed', dC, 1, 'c', 5) + \
                  for_field(sm_str, 'sname', dC, 1, 'c', 25, jtfy = 'l') + \
                  ' ' + self.__get_obj(sm_str, 'smodel', dC) + '\n'
        else:
            sb += sm + ' GE              1     1     S ' + sdate + '  2150 DEFAULT SIMULATION CONTROL\n'
        
        # OPTIONS
        sb += '@N OPTIONS     WATER NITRO SYMBI PHOSP POTAS DISES  CHEM  TILL   CO2\n'
        sm_str = self.__get_obj(tr_data, 'options', {})
        if sm_str != {}:
            sm_str['co2'] = co2
            sb += for_str(sm, 0, 'c', 2) + \
                  for_str('OP', 1, 'c', 11, jtfy = 'l') + \
                  for_field(sm_str, 'water', dC, 5, 'c', 1) + \
                  for_field(sm_str, 'nitro', dC, 5, 'c', 1) + \
                  for_field(sm_str, 'symbi', dC, 5, 'c', 1) + \
                  for_field(sm_str, 'phosp', dC, 5, 'c', 1) + \
                  for_field(sm_str, 'potas', dC, 5, 'c', 1) + \
                  for_field(sm_str, 'dises', dC, 5, 'c', 1) + \
                  for_field(sm_str, 'chem', dC, 5, 'c', 1) + \
                  for_field(sm_str, 'till', dC, 5, 'c', 1) + \
                  for_field(sm_str, 'co2', dC, 5, 'c', 1) + '\n'
        else:
            sb += sm + ' OP              ' + water + '     ' + nitro + '     Y     N     N     N     N     Y     ' + co2 + '\n'
        
        # METHODS
        sb += '@N METHODS     WTHER INCON LIGHT EVAPO INFIL PHOTO HYDRO NSWIT MESOM MESEV MESOL\n'
        sm_str = self.__get_obj(tr_data, 'methods', {})
        if sm_str != {}:
            sb += for_str(sm, 0, 'c', 2) + \
                  for_str('ME', 1, 'c', 11, jtfy = 'l') + \
                  for_field(sm_str, 'wther', dC, 5, 'c', 1) + \
                  for_field(sm_str, 'incon', dC, 5, 'c', 1) + \
                  for_field(sm_str, 'light', dC, 5, 'c', 1) + \
                  for_field(sm_str, 'evapo', dC, 5, 'c', 1) + \
                  for_field(sm_str, 'infil', dC, 5, 'c', 1) + \
                  for_field(sm_str, 'photo', dC, 5, 'c', 1) + \
                  for_field(sm_str, 'hydro', dC, 5, 'c', 1) + \
                  for_field(sm_str, 'nswit', dC, 5, 'c', 1) + \
                  for_field(sm_str, 'mesom', dC, 5, 'c', 1) + \
                  for_field(sm_str, 'mesev', dC, 5, 'c', 1) + \
                  for_field(sm_str, 'mesol', dC, 5, 'c', 1) + '\n'
        else:
            sb += sm + ' ME              M     M     E     R     S     L     R     1     P     S     2\n'
        
        # MANAGEMENT
        sb += '@N MANAGEMENT  PLANT IRRIG FERTI RESID HARVS\n'
        sm_str = self.__get_obj(tr_data, 'management', {})
        if sm_str != {}:
            sm_str['harvs'] = har_opt
            sb += for_str(sm, 0, 'c', 2) + \
                  for_str('MA', 1, 'c', 11, jtfy = 'l') + \
                  for_field(sm_str, 'plant', dC, 5, 'c', 1) + \
                  for_field(sm_str, 'irrig', dC, 5, 'c', 1) + \
                  for_field(sm_str, 'ferti', dC, 5, 'c', 1) + \
                  for_field(sm_str, 'resid', dC, 5, 'c', 1) + \
                  for_field(sm_str, 'harvs', dC, 5, 'c', 1) + '\n'
        else:
            sb = sm + ' MA              R     R     R     R     ' + har_opt + '\n'
        
        # OUTPUTS
        sb += '@N OUTPUTS     FNAME OVVEW SUMRY FROPT GROUT CAOUT WAOUT NIOUT MIOUT DIOUT VBOSE CHOUT OPOUT\n'
        sm_str = self.__get_obj(tr_data, 'outputs', {})
        if sm_str != {}:
            sb += for_str(sm, 0, 'c', 2) + \
                  for_str('OU', 1, 'c', 11, jtfy = 'l') + \
                  for_field(sm_str, 'fname', dC, 5, 'c', 1) + \
                  for_field(sm_str, 'ovvew', dC, 5, 'c', 1) + \
                  for_field(sm_str, 'sumry', dC, 5, 'c', 1) + \
                  for_field(sm_str, 'fropt', dC, 4, 'c', 2) + \
                  for_field(sm_str, 'grout', dC, 5, 'c', 1) + \
                  for_field(sm_str, 'caout', dC, 5, 'c', 1) + \
                  for_field(sm_str, 'waout', dC, 5, 'c', 1) + \
                  for_field(sm_str, 'niout', dC, 5, 'c', 1) + \
                  for_field(sm_str, 'miout', dC, 5, 'c', 1) + \
                  for_field(sm_str, 'diout', dC, 5, 'c', 1) + \
                  for_field(sm_str, 'vbose', dC, 5, 'c', 1) + \
                  for_field(sm_str, 'chout', dC, 5, 'c', 1) + \
                  for_field(sm_str, 'opout', dC, 5, 'c', 1) + '\n\n'
        else:
            sb += sm + ' OU              N     Y     Y     1     Y     Y     N     N     N     N     N     N     N\n\n'
        
        # PLANTING
        sb += '@  AUTOMATIC MANAGEMENT\n'
        sb += '@N PLANTING    PFRST PLAST PH2OL PH2OU PH2OD PSTMX PSTMN\n'
        sm_str = self.__get_obj(tr_data, 'planting', {})
        if sm_str != {}:
            pfyer = self.__get_obj(sm_str, 'pfyer', dC).split('.')[0] # remove decimal part
            pfyer = for_str(pfyer, 1, 'c', 2, zero_pad = True)
            pfday = self.__get_obj(sm_str, 'pfday', dC).split('.')[0]
            pfday = for_str(pfday, 0, 'c', 3, zero_pad = True)
            plyer = self.__get_obj(sm_str, 'plyer', dC).split('.')[0]
            plyer = for_str(plyer, 1, 'c', 2, zero_pad = True)
            plday = self.__get_obj(sm_str, 'plday', dC).split('.')[0]
            plday = for_str(plday, 0, 'c', 3, zero_pad = True)
            sb += for_str(sm, 0, 'c', 2) + \
                  for_str('PL', 1, 'c', 11, jtfy = 'l') + \
                  pfyer + pfday + \
                  plyer + plday + \
                  for_field(sm_str, 'ph2ol', dC, 1, 'c', 5) + \
                  for_field(sm_str, 'ph2ou', dC, 1, 'c', 5) + \
                  for_field(sm_str, 'ph2od', dC, 1, 'c', 5) + \
                  for_field(sm_str, 'pstmx', dC, 1, 'c', 5) + \
                  for_field(sm_str, 'pstmn', dC, 1, 'c', 5) + '\n'
        else:
            sb += sm + ' PL          82050 82064    40   100    30    40    10\n'
        
        # IRRIGATION
        sb += '@N IRRIGATION  IMDEP ITHRL ITHRU IROFF IMETH IRAMT IREFF\n'
        sm_str = self.__get_obj(tr_data, 'irrigation', {})
        if sm_str != {}:
            sb += for_str(sm, 0, 'c', 2) + \
                  for_str('IR', 1, 'c', 11, jtfy = 'l') + \
                  for_field(sm_str, 'imdep', dC, 1, 'c', 5) + \
                  for_field(sm_str, 'ithrl', dC, 1, 'c', 5) + \
                  for_field(sm_str, 'ithru', dC, 1, 'c', 5) + \
                  for_field(sm_str, 'iroff', dC, 1, 'c', 5) + \
                  for_field(sm_str, 'imeth', dC, 1, 'c', 5) + \
                  for_field(sm_str, 'iramt', dC, 1, 'c', 5) + \
                  for_field(sm_str, 'ireff', dC, 1, 'r', 5, ndec = 2) + '\n'
        else:
            sb += sm + ' IR             30    50   100 GS000 IR001    10  1.00\n'
        
        # NITROGEN
        sb += '@N NITROGEN    NMDEP NMTHR NAMNT NCODE NAOFF\n'
        sm_str = self.__get_obj(tr_data, 'nitrogen', {})
        if sm_str != {}:
            sb += for_str(sm, 0, 'c', 2) + \
                  for_str('NI', 1, 'c', 11, jtfy = 'l') + \
                  for_field(sm_str, 'nmdep', dC, 1, 'c', 5) + \
                  for_field(sm_str, 'nmthr', dC, 1, 'c', 5) + \
                  for_field(sm_str, 'namnt', dC, 1, 'c', 5) + \
                  for_field(sm_str, 'ncode', dC, 1, 'c', 5) + \
                  for_field(sm_str, 'naoff', dC, 1, 'c', 5) + '\n'
        else:
            sb += sm + ' NI             30    50    25 FE001 GS000\n'
        
        # RESIDUES
        sb += '@N RESIDUES    RIPCN RTIME RIDEP\n'
        sm_str = self.__get_obj(tr_data, 'residues', {})
        if sm_str != {}:
            sb += for_str(sm, 0, 'c', 2) + \
                  for_str('RE', 1, 'c', 11, jtfy = 'l') + \
                  for_field(sm_str, 'ripcn', dC, 1, 'c', 5) + \
                  for_field(sm_str, 'rtime', dC, 1, 'c', 5) + \
                  for_field(sm_str, 'ridep', dC, 1, 'c', 5) + '\n'
        else:
            sb += sm + ' RE            100     1    20\n'
        
        # HARVEST
        sb += '@N HARVEST     HFRST HLAST HPCNP HPCNR\n'
        sm_str = self.__get_obj(tr_data, 'harvests', {})
        if sm_str != {}:
            hlyer = self.__get_obj(sm_str, 'hlyer', dC).split('.')[0] # remove decimal part
            hlyer = for_str(hlyer, 1, 'c', 2, zero_pad = True)
            hlday = self.__get_obj(sm_str, 'hlday', dC).split('.')[0]
            hlday = for_str(hlday, 0, 'c', 3, zero_pad = True)
            sb += for_str(sm, 0, 'c', 2) + \
                  for_str('HA', 1, 'c', 11, jtfy = 'l') + \
                  for_field(sm_str, 'hfrst', dC, 1, 'c', 5) + \
                  hlyer + hlday + \
                  for_field(sm_str, 'hpcnp', dC, 1, 'c', 5) + \
                  for_field(sm_str, 'hpcnr', dC, 1, 'c', 5)
        else:
            sb += sm + ' HA              0 83057   100     0'
        
        return sb

class SOLFileOutput:  
    # member variables
    def_val = '-99'
        
    def __init__(self, json_file):        
        data = json.load(open(json_file))
        self.soils = data['soils']
        if not len(self.soils):
            raise Exception('No soil data')
        
    def toSOLFile(self):
        soils = self.soils
        def_val = self.def_val
        
        # iterate over all profiles
        s_str = ''
        for i in range(len(soils)):
            # get and format variables
            soil_id = for_field(soils[i], 'soil_id', def_val, 0, 'c', 10, jtfy = 'l')
            sl_source = for_field(soils[i], 'sl_source', def_val, 2, 'c', 11, jtfy = 'l')
            sltx = for_field(soils[i], 'sltx', def_val, 1, 'c', 5, jtfy = 'l')
            sldp = for_field(soils[i], 'sldp', def_val, 1, 'r', 5, jtfy = 'r', ndec = 0)
            soil_name = for_field(soils[i], 'soil_name', def_val, 1, 'c', 50, jtfy = 'l')
            
            sl_loc_3 = for_field(soils[i], 'sl_loc_3', def_val, 1, 'c', 11, jtfy = 'l')
            sl_loc_1 = for_field(soils[i], 'sl_loc_1', def_val, 1, 'c', 11, jtfy = 'l')
            soil_lat = for_field(soils[i], 'soil_lat', def_val, 1, 'r', 8, jtfy = 'r', ndec = 3)
            soil_long = for_field(soils[i], 'soil_long', def_val, 1, 'r', 8, jtfy = 'r', ndec = 3)
            classification = for_field(soils[i], 'classification', def_val, 1, 'c', 50, jtfy = 'l')
            
            sscol = for_field(soils[i], 'sscol', def_val, 1, 'c', 5, jtfy = 'r')
            salb = for_field(soils[i], 'salb', def_val, 1, 'r', 5, jtfy = 'r', ndec = 2)
            slu1 = for_field(soils[i], 'slu1', def_val, 1, 'r', 5, jtfy = 'r', ndec = 0)
            sldr = for_field(soils[i], 'sldr', def_val, 1, 'r', 5, jtfy = 'r', ndec = 2)
            slro = for_field(soils[i], 'slro', def_val, 1, 'r', 5, jtfy = 'r', ndec = 0)
            slnf = for_field(soils[i], 'slnf', def_val, 1, 'r', 5, jtfy = 'r', ndec = 2)
            slpf = for_field(soils[i], 'slpf', def_val, 1, 'r', 5, jtfy = 'r', ndec = 2)
            smhb = for_field(soils[i], 'smhb', def_val, 1, 'c', 5, jtfy = 'l')
            smpx = for_field(soils[i], 'smpx', def_val, 1, 'c', 5, jtfy = 'l')
            smke = for_field(soils[i], 'smke', def_val, 1, 'c', 5, jtfy = 'l')
            
            # write header
            s_str += '*' + soil_id + sl_source + sltx + sldp + soil_name + '\n'
            s_str += '@SITE        COUNTRY          LAT     LONG SCS FAMILY\n'
            s_str += sl_loc_3 + sl_loc_1 + soil_lat + soil_long + classification + '\n'
            s_str += '@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE\n'
            s_str += sscol + salb + slu1 + sldr + slro + slnf + slpf + smhb + smpx + smke + '\n'
            
            soilLayer = soils[i]['soilLayer']
        
            # iterate over soil depths
            s_str += '@  SLB  SLMH  SLLL  SDUL  SSAT  SRGF  SSKS  SBDM  SLOC  SLCL  SLSI  SLCF  SLNI  SLHW  SLHB  SCEC  SADC\n'
            for j in range(len(soilLayer)):
                # get and format variables        
                sllb = for_field(soilLayer[j], 'sllb', def_val, 1, 'r', 5, jtfy = 'r', ndec = 0)
                slmh = for_field(soilLayer[j], 'slmh', def_val, 1, 'c', 5, jtfy = 'l')
                slll = for_field(soilLayer[j], 'slll',  def_val, 1, 'r', 5, jtfy = 'r', ndec = 3)
                sldul = for_field(soilLayer[j], 'sldul', def_val, 1, 'r', 5, jtfy = 'r', ndec = 3)
                slsat = for_field(soilLayer[j], 'slsat', def_val, 1, 'r', 5, jtfy = 'r', ndec = 3)
                slrgf = for_field(soilLayer[j], 'slrgf', def_val, 1, 'r', 5, jtfy = 'r', ndec = 2)
                sksat = for_field(soilLayer[j], 'sksat', def_val, 1, 'r', 5, jtfy = 'r', ndec = 1)
                slbdm = for_field(soilLayer[j], 'slbdm', def_val, 1, 'r', 5, jtfy = 'r', ndec = 2)
                sloc = for_field(soilLayer[j], 'sloc', def_val, 1, 'r', 5, jtfy = 'r', ndec = 2)
                slcly = for_field(soilLayer[j], 'slcly', def_val, 1, 'r', 5, jtfy = 'r', ndec = 1)
                slsil = for_field(soilLayer[j], 'slsil', def_val, 1, 'r', 5, jtfy = 'r', ndec = 1)
                slcf = for_field(soilLayer[j], 'slcf', def_val, 1, 'r', 5, jtfy = 'r', ndec = 1)
                slni = for_field(soilLayer[j], 'slni', def_val, 1, 'r', 5, jtfy = 'r', ndec = 2)
                slphw = for_field(soilLayer[j], 'slphw', def_val, 1, 'r', 5, jtfy = 'r', ndec = 1)
                slphb = for_field(soilLayer[j], 'slphb', def_val, 1, 'r', 5, jtfy = 'r', ndec = 1)
                slcec = for_field(soilLayer[j], 'slcec', def_val, 1, 'r', 5, jtfy = 'r', ndec = 1)
                sladc = for_field(soilLayer[j], 'sladc', def_val, 1, 'r', 5, jtfy = 'r', ndec = 1)
                
                # write first row of soil data
                s_str += sllb + slmh + slll + sldul + slsat + slrgf + sksat + slbdm + \
                    sloc + slcly + slsil + slcf + slni + slphw + slphb + slcec + sladc + '\n'
            
            s_str += '@  SLB  SLPX  SLPT  SLPO CACO3  SLAL  SLFE  SLMN  SLBS  SLPA  SLPB  SLKE  SLMG  SLNA  SLSU  SLEC  SLCA\n'
            for j in range(len(soilLayer)):
                # get and extract variables
                sllb = for_field(soilLayer[j], 'sllb', def_val, 1, 'r', 5, jtfy = 'r', ndec = 0)
                slpx = for_field(soilLayer[j], 'slpx', def_val, 1, 'r', 5, jtfy = 'r', ndec = 1)
                slpt = for_field(soilLayer[j], 'slpt', def_val, 1, 'r', 5, jtfy = 'r', ndec = 1)
                slpo = for_field(soilLayer[j], 'slpo', def_val, 1, 'r', 5, jtfy = 'r', ndec = 1)
                caco3 = for_field(soilLayer[j], 'caco3', def_val, 1, 'r', 5, jtfy = 'r', ndec = 1)
                slal = for_field(soilLayer[j], 'slal', def_val, 1, 'r', 5, jtfy = 'r', ndec = 1)
                slfe = for_field(soilLayer[j], 'slfe', def_val, 1, 'r', 5, jtfy = 'r', ndec = 1)       
                slmn = for_field(soilLayer[j], 'slmn', def_val, 1, 'r', 5, jtfy = 'r', ndec = 1)
                slbs = for_field(soilLayer[j], 'slbs', def_val, 1, 'r', 5, jtfy = 'r', ndec = 1)
                slpa = for_field(soilLayer[j], 'slpa', def_val, 1, 'r', 5, jtfy = 'r', ndec = 1)
                slpb = for_field(soilLayer[j], 'slpb', def_val, 1, 'r', 5, jtfy = 'r', ndec = 1)
                slke = for_field(soilLayer[j], 'slke', def_val, 1, 'r', 5, jtfy = 'r', ndec = 1)
                slmg = for_field(soilLayer[j], 'slmg', def_val, 1, 'r', 5, jtfy = 'r', ndec = 1)
                slna = for_field(soilLayer[j], 'slna', def_val, 1, 'r', 5, jtfy = 'r', ndec = 1)
                slsu = for_field(soilLayer[j], 'slsu', def_val, 1, 'r', 5, jtfy = 'r', ndec = 1)      
                slec = for_field(soilLayer[j], 'slec', def_val, 1, 'r', 5, jtfy = 'r', ndec = 1)
                slca = for_field(soilLayer[j], 'slca', def_val, 1, 'r', 5, jtfy = 'r', ndec = 1)
        
                # write second row of soil data
                s_str += sllb + slpx + slpt + slpo + caco3 + slal + slfe + slmn + slbs + \
                         slpa + slpb + slke + slmg + slna + slsu + slec + slca
                
                if j != len(soilLayer) - 1: s_str += '\n'
            
            if i != len(soils) - 1: s_str += '\n'
            
        return s_str

# parse inputs
parser = OptionParser()
parser.add_option("-e", "--exp", dest = "efile", default = "exp.json", type = "string", 
                  help = "input experiment json file", metavar = "FILE")
parser.add_option("-s", "--soil", dest = "sfile", default = "soil.json", type = "string", 
                  help = "input soil json file", metavar = "FILE") 
parser.add_option("-x", "--Xfile", dest = "Xfile", default = "exp.X", type = "string",
                  help = "output X file", metavar = "FILE")
parser.add_option("-S", "--SOLfile", dest = "SOLfile", default = "soil.SOL", type = "string",
                  help = "output SOL file", metavar = "FILE")
(options, args) = parser.parse_args()

# parse experiment JSON file and write out X file
xfileoutput = DSSATXFileOutput(options.efile)
open(options.Xfile, 'w').write(xfileoutput.toXFile())

# parse soil JSON file and write out SOL file
sfileoutput = SOLFileOutput(options.sfile)
open(options.SOLfile, 'w').write(sfileoutput.toSOLFile())