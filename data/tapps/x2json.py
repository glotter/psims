#!/usr/bin/env python

# import modules
import json
import copy
import os.path
import datetime
import numpy as np
from optparse import OptionParser
from collections import OrderedDict as od

class DSSATXFileInput:
    # member variables
    flg = [''] * 3
    event_key = 'data'
    ic_event_key = 'soilLayer'
    def_val_R = '-99.0'
    def_val_C = ''
    def_val_I = '-99'
    def_val_D = '20110101'
    
    def __init__(self, x_file):
        self.lines = [l.strip('\r\n') for l in tuple(open(x_file, 'r'))]
        fs = os.path.basename(x_file).split('.')
        self.exname = fs[0] + fs[1][: 2]
        
    def toJSON(self):
        ret = {}
        meta = {}
        exp_arr = []
        
        # get treatmenta data
        mgn_arr = self.__read_treatments(meta)
        
        for i in range(len(mgn_arr)):
            exp_data = self.__setup_meta_data(meta, i)
            self.__copy_item(exp_data, exp_data, 'soil', 'soil_analysis', True)
            soil_tmp = self.__get_obj(exp_data, 'soil', {})
            exp_data['soil_id'] += '_' + str(i + 1)
            self.__copy_item(soil_tmp, exp_data, 'soil_id', 'soil_id', False)
            self.__copy_item(soil_tmp, exp_data, 'sltx', 'sltx', False)
            self.__copy_item(soil_tmp, exp_data, 'sldp', 'sldp', False)
            soil_sub_arr = self.__get_obj(soil_tmp, self.ic_event_key, [])
            for sub in soil_sub_arr:
                if 'slsc' in sub:
                    sub.pop('slsc')
            exp_data['soil'] = soil_tmp
            self.__setup_trn_data(exp_data, mgn_arr[i]) # set up management data
            exp_arr.append(exp_data)
        
        ret['experiments'] = exp_arr
        return ret
        
    # =================
    # UTILITY FUNCTIONS
    # =================

    def __read_treatments(self, meta_data):
        tr = [] # treatment data
        meta = {} # metadata
        meta_data[self.exname] = meta
        
        # variable to be used later
        ireff = ''
        
        sq_arr = []; cu_arr = []; fl_arr = []
        sa_arr = []; sad_arr = []; ic_arr = []
        icd_arr = []; pl_arr = []; ir_arr = []
        ird_arr = []; fe_arr = []; om_arr = []
        ch_arr = []; ti_arr = []; em_arr = []
        ha_arr = []; sm_arr = []
        
        for i in range(len(self.lines)):
            line = self.lines[i]
            
            # retrieve line types
            self.__line_type(line)
            
            if self.flg[0].startswith('exp.details:') and self.flg[2] == '':
                # header
                fmt = od([('null', 25), ('local_name', 61)])
                meta.update(self.__read_line(line, fmt))
            elif self.flg[0].startswith('general'):
                # general
                if self.flg[1] == 'people' and self.flg[2] == 'data':
                    # people
                    meta['person_notes'] = line.strip()
                elif self.flg[1] == 'address' and self.flg[2] == 'data':
                    # address
                    meta['institution'] = line.strip()
                elif (self.flg[1] == 'site' or self.flg[1] == 'sites') and self.flg[2] == 'data':
                    # site
                    meta['site_name'] = line.strip()
                elif self.flg[1].startswith('parea') and self.flg[2] == 'data':
                    # plot
                    fmt = od([('plta', 7), ('pltr#', 6), ('pltln', 6), ('pldr', 6), \
                              ('pltsp', 6), ('pllay', 6), ('pltha', 6), ('plth#', 6), \
                              ('plthl', 6), ('plthm', 16)])
                    meta.update(self.__read_line(line, fmt))
                elif self.flg[1] == 'notes' and self.flg[2] == 'data':
                    # notes
                    if not 'tr_notes' in meta:
                        meta['tr_notes'] = line
                    else:
                        notes = meta['tr_notes']
                        notes += '\r\n' + line
                        meta['tr_notes'] = notes
            elif self.flg[0].startswith('treatments'):
                # treatments
                if self.flg[2] == 'data':
                    fmt = od([('trno', 3), ('sq', 1), ('op', 2), ('co', 2), \
                              ('trt_name', 26), ('ge', 3), ('fl', 3), ('sa', 3), \
                              ('ic', 3), ('pl', 3), ('ir', 3), ('fe', 3), ('om', 3), \
                              ('ch', 3), ('ti', 3), ('em', 3), ('ha', 3), ('sm', 3)])
                    sq_arr.append(self.__read_line(line, fmt))
            elif self.flg[0].startswith('cultivars'):
                # cultivars
                if self.flg[2] == 'data':
                    fmt = od([('ge', 2), ('crid', 3), ('cul_id', 7), ('cul_name', 17)])
                    tmp = self.__read_line(line, fmt)
                    # MAY NEED THIS LATER!
                    # cul_id = tmp['cul_id']
                    # if cul_id != '':
                    #     tmp['dssat_cul_id'] = cul_id
                    cu_arr.append(tmp)
            elif self.flg[0].startswith('fields'):
                # fields
                if self.flg[1].startswith('l id_') and self.flg[2] == 'data':
                    fmt = od([('fl', 2), ('id_field', 9), ('wst_id', 9), ('flsl', 6), \
                              ('flob', 6), ('fl_drntype', 6), ('fldrd', 6), \
                              ('fldrs', 6), ('flst', 6), ('sltx', 6), ('sldp', 6), \
                              ('soil_id', 11), ('fl_name', len(line))])
                    tmp = self.__read_line(line, fmt)
                    fl_arr = self.__add2array(fl_arr, tmp, 'fl')
                elif self.flg[1].startswith('l ...') and self.flg[2] == 'data':
                    fmt = od([('fl', 2), ('fl_long', 16), ('fl_lat', 16), \
                              ('flele', 10), ('farea', 18), ('', 6), ('fllwr', 6), \
                              ('flsla', 6), ('flhst', 6), ('fhdur', 6)])
                    tmp = self.__read_line(line, fmt)
                    fl_arr = self.__add2array(fl_arr, tmp, 'fl')
                    # ASSUMES LATITUDE AND LONGITUDE ARE VALID
            elif self.flg[0].startswith('soil'):
                # soil
                if self.flg[1].startswith('a sadat') and self.flg[2] == 'data':
                    fmt = od([('sa', 2), ('sadat', 6), ('smhb', 6), ('smpx', 6), \
                              ('smke', 6)])
                    tmp = self.__read_line(line, fmt)
                    tmp['sadat'] = self.__translate_date_str(tmp['sadat'])
                    sa_arr.append(tmp)
                    sad_arr = [] # new array
                    tmp[self.ic_event_key] = sad_arr
                elif self.flg[1].startswith('a  sabl') and self.flg[2] == 'data':
                    fmt = od([('', 2), ('sllb', 6), ('slbdm', 6), ('sloc', 6), \
                              ('slni', 6), ('slphw', 6), ('slphb', 6), ('slpx', 6), \
                              ('slke', 6), ('slsc', 6)])
                    sad_arr.append(self.__read_line(line, fmt))
            elif self.flg[0].startswith('initial'):
                # initial
                if self.flg[1].startswith('c   pcr') and self.flg[2] == 'data':
                    fmt = od([('ic', 2), ('icpcr', 6), ('icdat', 6), ('icrt', 6), \
                              ('icnd', 6), ('icrz#', 6), ('icrze', 6), ('icwt', 6), \
                              ('icrag', 6), ('icrn', 6), ('icrp', 6), ('icrip', 6), \
                              ('icrdp', 6), ('ic_name', len(line))])
                    tmp = self.__read_line(line, fmt)
                    tmp['icdat'] = self.__translate_date_str(tmp['icdat'])
                    ic_arr.append(tmp)
                    icd_arr = [] # new array
                    tmp[self.ic_event_key] = icd_arr
                elif self.flg[1].startswith('c  icbl') and self.flg[2] == 'data':
                    fmt = od([('', 2), ('icbl', 6), ('ich2o', 6), ('icnh4', 6), \
                              ('icno3', 6)])
                    icd_arr.append(self.__read_line(line, fmt))
            elif self.flg[0].startswith('planting'):
                # planting
                if self.flg[2] == 'data':
                    fmt = od([('pl', 2), ('pdate', 6), ('pldae', 6), ('plpop', 6), \
                              ('plpoe', 6), ('plma', 6), ('plds', 6), ('plrs', 6), \
                              ('plrd', 6), ('pldp', 6), ('plmwt', 6), ('page', 6), \
                              ('plenv', 6), ('plph', 6), ('plspl', 6), \
                              ('pl_name', len(line))])
                    tmp = self.__read_line(line, fmt)
                    if 'pdate' in tmp:
                        tmp['pdate'] = self.__translate_date_str(tmp['pdate'])
                    if 'pldae' in tmp:
                        tmp['pldae'] = self.__translate_date_str(tmp['pldae'])
                    pldp = self.__get_obj(tmp, 'pldp', '')
                    if pldp != '':
                        # convert from cm to mm
                        tmp['pldp'] = str(10 * np.double(pldp))
                    pl_arr.append(tmp)
            elif self.flg[0].startswith('irrigation'):
                # irrigation
                if self.flg[1].startswith('i  efir') and self.flg[2] == 'data':
                    fmt = od([('ir', 2), ('ireff', 6), ('irmdp', 6), ('irthr', 6), \
                              ('irept', 6), ('irstg', 6), ('iame', 6), ('iamt', 6), \
                              ('ir_name', len(line))])
                    tmp = self.__read_line(line, fmt)
                    ireff = tmp['ireff']
                    if ireff != '':
                        tmp.pop('ireff') # remove ireff field
                    ir_arr.append(tmp)
                    ird_arr = [] # new array
                    tmp[self.event_key] = ird_arr
                elif self.flg[1].startswith('i idate') and self.flg[2] == 'data':
                    fmt = od([('', 2), ('idate', 6), ('irop', 6), ('irval', 6)])
                    tmp = self.__read_line(line, fmt)
                    tmp['ireff'] = ireff
                    ird_arr.append(tmp)
            elif self.flg[0].startswith('fertilizers'):
                # fertilizer
                if self.flg[2] == 'data':
                    fmt = od([('fe', 2), ('fdate', 6), ('fecd', 6), ('feacd', 6), \
                              ('fedep', 6), ('feamn', 6), ('feamp', 6), ('feamk', 6), \
                              ('feamc', 6), ('feamo', 6), ('feocd', 6), \
                              ('fe_name', len(line))])
                    tmp = self.__read_line(line, fmt)
                    fe_arr.append(tmp)
            elif self.flg[0].startswith('residues'):
                # residues and other organic materials
                if self.flg[2] == 'data':
                    fmt = od([('om', 2), ('omdat', 6), ('omcd', 6), ('omamt', 6), \
                              ('omn%', 6), ('omnp%', 6), ('omk%', 6), ('ominp', 6), \
                              ('omdep', 6), ('omacd', 6), ('om_name', len(line))])
                    tmp = self.__read_line(line, fmt)
                    om_arr.append(tmp)
            elif self.flg[0].startswith('chemical'):
                # chemical applications
                if self.flg[2] == 'data':
                    fmt = od([('ch', 2), ('cdate', 6), ('chcd', 6), ('chamt', 6), \
                              ('chacd', 6), ('chdep', 6), ('ch_targets', 6), \
                              ('ch_name', len(line))])
                    tmp = self.__read_line(line, fmt)
                    tmp['cdate'] = self.__translate_date_str(tmp['cdate'])
                    ch_arr.append(tmp)
            elif self.flg[0].startswith('tillage'):
                # tillage
                if self.flg[2] == 'data':
                    fmt = od([('ti', 2), ('tdate', 6), ('tiimp', 6), ('tidep', 6), \
                              ('ti_name', len(line))])
                    tmp = self.__read_line(line, fmt)
                    tmp['tdate'] = self.__translate_date_str(tmp['tdate'])
                    ti_arr.append(tmp)
            elif self.flg[0].startswith('environment'):
                # environment modifications
                if self.flg[2] == 'data':
                    fmt = od([('em', 2)])
                    tmp = self.__read_line(line, fmt)
                    fmt2 = od([('odyer', 3), ('odday', 3), ('eday', 6), ('erad', 6), \
                               ('emax', 6), ('emin', 6), ('erain', 6), ('eco2', 6), \
                               ('edew', 6), ('ewind', 6), ('envnam', len(line[2 :]))])
                    tmp['data'] = self.__read_line(line[2 :], fmt2)
                    em_arr.append(tmp)
            elif self.flg[0].startswith('harvest'):
                # harvest details
                if self.flg[2] == 'data':
                    fmt = od([('ha', 2), ('hadat', 6), ('hastg', 6), ('hacom', 6), \
                              ('hasiz', 6), ('hap%', 6), ('hab%', 6), \
                              ('ha_name', len(line))])
                    tmp = self.__read_line(line, fmt)
                    if 'hadat' in tmp:
                        tmp['hadat'] = self.__translate_date_str(tmp['hadat'])
                    ha_arr.append(tmp)
            elif self.flg[0].startswith('simulation'):
                # simulation controls
                if self.flg[1].startswith('n general') and self.flg[2] == 'data':
                    # general
                    fmt = od([('sm', 2), ('general', len(line))])
                    tmp = self.__read_line(line, fmt)
                    fmt2 = od([('', 11), ('nyers', 6), ('nreps', 6), ('start', 6), \
                               ('sdyer', 3), ('sdday', 3), ('rseed', 6), ('sname', 26), \
                               ('smodel', len(tmp['general']))])
                    tmp['general'] = self.__read_line(tmp['general'], fmt2)
                    sm_arr = self.__add2array(sm_arr, tmp, 'sm')
                elif self.flg[1].startswith('n options') and self.flg[2] == 'data':
                    # options
                    fmt = od([('sm', 2), ('options', len(line))])
                    tmp = self.__read_line(line, fmt)
                    fmt2 = od([('', 11), ('water', 6), ('nitro', 6), ('symbi', 6), \
                               ('phosp', 6), ('potas', 6), ('dises', 6), ('chem', 6), \
                               ('till', 6), ('co2', 6)])
                    tmp['options'] = self.__read_line(tmp['options'], fmt2)
                    sm_arr = self.__add2array(sm_arr, tmp, 'sm')
                elif self.flg[1].startswith('n methods') and self.flg[2] == 'data':
                    # methods
                    fmt = od([('sm', 2), ('methods', len(line))])
                    tmp = self.__read_line(line, fmt)
                    fmt2 = od([('', 11), ('wther', 6), ('incon', 6), ('light', 6), \
                               ('evapo', 6), ('infil', 6), ('photo', 6), ('hydro', 6), \
                               ('nswit', 6), ('mesom', 6), ('mesev', 6), ('mesol', 6)])
                    tmp['methods'] = self.__read_line(tmp['methods'], fmt2)
                    sm_arr = self.__add2array(sm_arr, tmp, 'sm')
                elif self.flg[1].startswith('n management') and self.flg[2] == 'data':
                    # management
                    fmt = od([('sm', 2), ('management', len(line))])
                    tmp = self.__read_line(line, fmt)
                    fmt2 = od([('', 11), ('plant', 6), ('irrig', 6), ('ferti', 6), \
                               ('resid', 6), ('harvs', 6)])
                    tmp['management'] = self.__read_line(tmp['management'], fmt2)
                    sm_arr = self.__add2array(sm_arr, tmp, 'sm')
                elif self.flg[1].startswith('n outputs') and self.flg[2] == 'data':
                    # outputs
                    fmt = od([('sm', 2), ('outputs', len(line))])
                    tmp = self.__read_line(line, fmt)
                    fmt2 = od([('', 11), ('fname', 6), ('ovvew', 6), ('sumry', 6), \
                               ('fropt', 6), ('grout', 6), ('caout', 6), ('waout', 6), \
                               ('niout', 6), ('miout', 6), ('diout', 6), ('vbose', 6), \
                               ('chout', 6), ('opout', 6)])
                    tmp['outputs'] = self.__read_line(tmp['outputs'], fmt2)
                    sm_arr = self.__add2array(sm_arr, tmp, 'sm')
                elif self.flg[1].startswith('n planting') and self.flg[2] == 'data':
                    # planting
                    fmt = od([('sm', 2), ('planting', len(line))])
                    tmp = self.__read_line(line, fmt)
                    fmt2 = od([('', 11), ('pfyer', 3), ('pfday', 3), ('plyer', 3), \
                               ('plday', 3), ('ph2ol', 6), ('ph2ou', 6), ('ph2od', 6), \
                               ('pstmx', 6), ('pstmn', 6)])
                    tmp['planting'] = self.__read_line(tmp['planting'], fmt2)
                    sm_arr = self.__add2array(sm_arr, tmp, 'sm')
                elif self.flg[1].startswith('n irrigation') and self.flg[2] == 'data':
                    # irrigation
                    fmt = od([('sm', 2), ('irrigation', len(line))])
                    tmp = self.__read_line(line, fmt)
                    fmt2 = od([('', 11), ('imdep', 6), ('ithrl', 6), ('ithru', 6), \
                               ('iroff', 6), ('imeth', 6), ('iramt', 6), ('ireff', 6)])
                    tmp['irrigation'] = self.__read_line(tmp['irrigation'], fmt2)
                    sm_arr = self.__add2array(sm_arr, tmp, 'sm')
                elif self.flg[1].startswith('n nitrogen') and self.flg[2] == 'data':
                    # nitrogen
                    fmt = od([('sm', 2), ('nitrogen', len(line))])
                    tmp = self.__read_line(line, fmt)
                    fmt2 = od([('', 11), ('nmdep', 6), ('nmthr', 6), ('namnt', 6), \
                               ('ncode', 6), ('naoff', 6)])
                    tmp['nitrogen'] = self.__read_line(tmp['nitrogen'], fmt2)
                    sm_arr = self.__add2array(sm_arr, tmp, 'sm')
                elif self.flg[1].startswith('n residues') and self.flg[2] == 'data':
                    # residues
                    fmt = od([('sm', 2), ('residues', len(line))])
                    tmp = self.__read_line(line, fmt)
                    fmt2 = od([('', 11), ('ripcn', 6), ('rtime', 6), ('ridep', 6)])
                    tmp['residues'] = self.__read_line(tmp['residues'], fmt2)
                    sm_arr = self.__add2array(sm_arr, tmp, 'sm')
                elif self.flg[1].startswith('n harvest') and self.flg[2] == 'data':
                    # harvest
                    fmt = od([('sm', 2), ('harvests', len(line))])
                    tmp = self.__read_line(line, fmt)
                    fmt2 = od([('', 11), ('hfrst', 6), ('hlyer', 3), ('hlday', 3), \
                               ('hpcnp', 6), ('hpcnr', 6)]) 
                    tmp['harvests'] = self.__read_line(tmp['harvests'], fmt2)
                    sm_arr = self.__add2array(sm_arr, tmp, 'sm')

        tr_meta_arr = []
        meta_data['tr_meta'] = tr_meta_arr # treatment metadata

        # combine all sections into related treatment block
        trno = ''
        dssat_sq = {}
        sq_arr_new = []
        seqid = 1
        for i in range(len(sq_arr)):            
            sq_data = sq_arr[i]
            
            tr_meta = {}
            tr_meta_arr.append(tr_meta)
                  
            if sq_data['trno'] != trno:
                trno = sq_data['trno']
                
                tr_data = {}
                evt_arr = []
                tr.append(tr_data)
                tr_data['events'] = evt_arr
                
                dssat_sq = {}
                sq_arr_new = []
                dssat_sq[self.event_key] = sq_arr_new
                tr_data['dssat_sequence'] = dssat_sq
                
                tr_meta['trt_name'] = sq_data['trt_name']
                tr_meta['trno'] = trno
                tr_meta['exname'] = self.exname
                
                seqid = 1
            else:
                tr_meta.pop('trt_name')
            sq_data['seqid'] = str(seqid)
            sq_arr_new.append(sq_data)
            
            # cultivar
            cr_data = {}
            if self.__get_obj(sq_data, 'ge', '0') != '0':
                cr_data = self.__get_section_data_obj(cu_arr, 'ge', sq_data['ge'])
                
            # field
            if self.__get_obj(sq_data, 'fl', '0') != '0':
                fld = self.__get_section_data_obj(fl_arr, 'fl', sq_data['fl'])
                tr_meta.update(fld)
                tr_meta.pop('fl')
                
            # initial condition
            if self.__get_obj(sq_data, 'ic', '0') != '0':
                ic_tmp_arr = self.__get_section_data_obj(ic_arr, 'ic', sq_data['ic'])
                if ic_tmp_arr != []:
                    tr_data['initial_conditions'] = ic_tmp_arr
            
            # planting
            pdate = ''
            if self.__get_obj(sq_data, 'pl', '0') != '0':
                # add event to array
                m = self.__get_section_data_obj(pl_arr, 'pl', sq_data['pl'])
                self.__add_event(evt_arr, m, 'pdate', 'planting', seqid)
                
                # add cultivar
                if cr_data != {} and cr_data != []:
                    evt_arr[-1].update(cr_data)
                
                # get planting date
                pdate = self.__get_obj(evt_arr[-1], 'pdate', '')
                if len(pdate) > 5:
                    pdate = pdate[2 :]
                    
            # irrigation
            if self.__get_obj(sq_data, 'ir', '0') != '0':
                ir_tmp = self.__get_section_data_obj(ir_arr, 'ir', sq_data['ir'])
                ir_tmp_subs = self.__get_obj(ir_tmp, 'data', [])
                for j in range(len(ir_tmp_subs)):
                    ir_tmp_sub = ir_tmp_subs[j]
                    idate = self.__get_obj(ir_tmp_sub, 'idate', None)
                    ir_tmp_sub['idate'] = self.__translate_date_str_for_doy(idate, pdate)
                self.__add_event(evt_arr, ir_tmp, 'idate', 'irrigation', seqid)
                
            # fertilizer
            if self.__get_obj(sq_data, 'fe', '0') != '0':
                fe_tmp = self.__get_section_data_obj(fe_arr, 'fe', sq_data['fe'])
                for j in range(len(fe_tmp)):
                    fe_tmp_sub = fe_tmp[j]
                    fdate = self.__get_obj(fe_tmp_sub, 'fdate', None)
                    fe_tmp_sub['fdate'] = self.__translate_date_str_for_doy(fdate, pdate)
                    self.__add_event(evt_arr, fe_tmp_sub, 'fdate', 'fertilizer', seqid)
            
            # organic matter
            if self.__get_obj(sq_data, 'om', '0') != '0':
                om_tmp = self.__get_section_data_obj(om_arr, 'om', sq_data['om'])
                for j in range(len(om_tmp)):
                    om_tmp_sub = om_tmp[j]
                    omdate = self.__get_obj(om_tmp_sub, 'omdat', None)
                    om_tmp_sub['omdat'] = self.__translate_date_str_for_doy(omdate, pdate)
                    self.__add_event(evt_arr, om_tmp_sub, 'omdat', 'organic_matter', seqid)
            
            # chemical
            if self.__get_obj(sq_data, 'ch', '0') != '0':
                ch_tmp = self.__get_section_data_obj(ch_arr, 'ch', sq_data['ch'])
                for j in range(len(ch_tmp)):
                    self.__add_event(evt_arr, ch_tmp[j], 'cdate', 'chemical', seqid)
            
            # tillage
            if self.__get_obj(sq_data, 'ti', '0') != '0':
                ti_tmp = self.__get_section_data_obj(ti_arr, 'ti', sq_data['ti'])
                for j in range(len(ti_tmp)):
                    self.__add_event(evt_arr, ti_tmp[j], 'tdate', 'tillage', seqid)
            
            # environment
            if self.__get_obj(sq_data, 'em', '0') != '0':
                em = sq_data['em']
                em_data_arr = self.__get_section_data_obj(em_arr, 'em', em)
                tmp = self.__get_obj(tr_data, 'dssat_environment_modification', {})
                arr = self.__get_obj(tmp, self.event_key, [])
                is_exist = False
                for j in range(len(arr)):
                    if em == arr[j]['em']:
                        is_exist = True
                        break
                if not is_exist:
                    arr.append(em_data_arr)
                tmp[self.event_key] = arr
                tr_data['dssat_environment_modification'] = tmp
            
            # harvest
            if self.__get_obj(sq_data, 'ha', '0') != '0':
                m = self.__get_section_data_obj(ha_arr, 'ha', sq_data['ha'])
                self.__add_event(evt_arr, m, 'hadat', 'harvest', seqid)
            
            # simulation
            if self.__get_obj(sq_data, 'sm', '0') != '0':
                sm = sq_data['sm']
                sm_data = self.__get_section_data_obj(sm_arr, 'sm', sm)
                tmp = self.__get_obj(tr_data, 'dssat_simulation_control', {})
                arr = self.__get_obj(tmp, self.event_key, [])
                is_exist = False
                for j in range(len(arr)):
                    if sm == arr[j]['sm']:
                        is_exist = True
                        break
                if not is_exist:
                    arr.append(sm_data)
                tmp[self.event_key] = arr
                tr_data['dssat_simulation_control'] = tmp
                
            # soil analysis
            if self.__get_obj(sq_data, 'sa', '0') != '0':
                sa_tmp = self.__get_section_data_obj(sa_arr, 'sa', sq_data['sa'])
                tr_meta['soil_analysis'] = sa_tmp
            
            seqid += 1
        
        id_names = ['ge', 'fl', 'sa', 'ic', 'pl', 'ir', 'fe', 'om', 'ch', 'ti', 'ha']
        self.__remove_index(tr, id_names)
        self.__remove_index(meta_data, id_names)

        return tr
    
    def __read_line(self, l, f):
        dic = {}
        l2 = l
        for i in f.iteritems():
            key = i[0]
            val = i[1]
            length = min(val, len(l))
            if key != '' and not key.startswith('null'):
                tmp = l2[: length].strip()
                if self.__check_valid_value(tmp):
                    dic[key] = tmp
            l2 = l2[length :]
        return dic
        
    def __check_valid_value(self, val):
        return not (val is None or val.strip() == self.def_val_C or \
                    val.strip() == self.def_val_I or val.strip() == self.def_val_R)
    
    def __line_type(self, l):
        if l.startswith('*'):
            self.flg[0] = l[1 :].lower().strip()
            self.flg[1] = ''
            self.flg[2] = ''
        elif l.startswith('@'):
            self.flg[1] = l[1 :].lower().strip()
            self.flg[2] = 'title'
        elif l.startswith('!'):
            self.flg[2] = 'comment'
        elif l.strip() != '':
            self.flg[2] = 'data'
        # NOT NECESSARY
        # elif self.flg[2] == 'blank':
        #     self.flg[0] = ''
        #     self.flg[1] = ''
        #     self.flg[2] = 'blank'
        else:
            self.flg[1] = ''
            self.flg[2] = 'blank'
    
    def __add2array(self, arr, item, key):
        unmatched = True
        for i in range(len(arr)):
            if not type(key) is list and not type(key) is tuple:
                if arr[i][key] == item[key]:
                    arr[i].update(item)
                    unmatched = False
                    break
            else:
                equal_flag = True
                for j in range(len(key)):
                    if arr[i][key[j]] != item[key[j]]:
                        equal_flag = False
                        break
                if equal_flag:
                    arr[i].update(item)
                    unmatched = False
                    break
        if unmatched:
            arr.append(item)
        return arr
    
    def __translate_date_str(self, date_str):
        # convert from yyddd to yyyymmdd
        if date_str is None:
            return self.def_val_D # default date
        if len(date_str) > 5 or len(date_str) < 4:
            return date_str # includes relative dates
        date_str = date_str.strip() # remove spaces
        year = int(date_str[: 2])
        day = int(date_str[2 :])
        if year <= 15:
            year += 2000
        else:
            year += 1900
        date = datetime.date(year, 1, 1) + datetime.timedelta(day - 1)
        return date.strftime('%Y%m%d')
        
    def __translate_date_str_for_doy(self, date_str, pdate):
        # convert from yyddd or doy to yyyymmdd
        if not date_str is None and len(date_str) <= 3:
            if pdate != '' and len(pdate) >= 2:
                try:
                    date_str = pdate[: 2] + int(date_str)
                except:
                    return ''
        return self.__translate_date_str(date_str)
    
    def __get_obj(self, dic, key, dft):
        # gets actual object, NOT copy
        if key in dic:
            return dic[key]
        else:
            return dft
    
    def __get_section_data_obj(self, arr, key, val):
        # returns copy
        ret = []
        single_sub_rec = ['ge', 'fl', 'pl', 'ha', 'sm']
        if arr == [] or val is None:
            if key == 'icbl' or key == 'ir' or key == 'ic' or key == 'sa' or \
               key in single_sub_rec:
                return {}
            else:
                return ret
        fst_node = arr[0]
        if self.event_key in fst_node or self.ic_event_key in fst_node or \
           key in single_sub_rec:
            for i in range(len(arr)):
                if arr[i][key] == val:
                    return copy.deepcopy(arr[i])
        else:
            for i in range(len(arr)):
                if arr[i][key] == val:
                    ret.append(copy.deepcopy(arr[i]))
        return ret
        
    def __add_event(self, events, m, date_id, event_name, seqid):
        ret = {}
        mc = copy.deepcopy(m) # make copy
        if date_id in mc:
            ret['date'] = mc.pop(date_id)
        ret['event'] = event_name
        if self.event_key in mc:
            sub_arr = mc.pop(self.event_key)
            if sub_arr is None or sub_arr == []:
                ret.update(mc)
                events.append(ret)
            for i in range(len(sub_arr)):
                tmp = sub_arr[i]
                ret2 = copy.deepcopy(ret) # make copy
                ret2['date'] = tmp.pop(date_id)
                ret2.update(mc)
                ret2.update(tmp)
                ret2['seqid'] = str(seqid)
                events.append(ret2)
        else:
            ret.update(mc)
            ret['seqid'] = str(seqid)
            events.append(ret)
            
    def __remove_index(self, arr, id_names):
        for item in arr:
            if isinstance(item, list):
                self.__remove_index(item, id_names)
            elif isinstance(item, dict):
                self.__remove_index2(item, id_names)
    
    def __remove_index2(self, arr, id_names):
        keys = arr.keys()
        for i in range(len(keys)):
            key = keys[i]
            item = arr[key]
            if isinstance(item, list):
                self.__remove_index(item, id_names)
            elif isinstance(item, dict):
                self.__remove_index2(item, id_names)
            elif isinstance(item, str) and key in id_names:
                arr.pop(key)
                
    def __setup_meta_data(self, meta, tr_id):
        exp_data = {}
        tr_meta_arr = meta['tr_meta']
        exname = self.__get_obj(tr_meta_arr[tr_id], 'exname', '')
        exp_data.update(self.__get_obj(meta, exname, {}))
        exp_data.update(tr_meta_arr[tr_id])
        if exname != '':
            exp_data['exname'] = exname + '_' + exp_data['trno']
        exp_data['exname_o'] = exname
        return exp_data
        
    def __copy_item(self, to, frm, to_key, frm_key, delete_flg):
        if frm_key in frm and not frm[frm_key] is None:
            if delete_flg:
                to[to_key] = frm.pop(frm_key)
            else:
                to[to_key] = frm[frm_key]

    def __setup_trn_data(self, exp, mgn):
        if 'sltx' in exp:
            exp.pop('sltx')
        if 'sldp' in exp:
            exp.pop('sldp')
        exp['management'] = mgn
        
        # initial conditions
        self.__copy_item(exp, mgn, 'initial_conditions', 'initial_conditions', True)
        # dssat sequence
        self.__copy_item(exp, mgn, 'dssat_sequence', 'dssat_sequence', True)
        # dssat environment modification
        self.__copy_item(exp, mgn, 'dssat_environment_modification', 'dssat_environment_modification', True)
        # dssat simulation control
        self.__copy_item(exp, mgn, 'dssat_simulation_control', 'dssat_simulation_control', True)
        
        # create dssat info holder
        dssat_info = {}
        self.__copy_item(dssat_info, exp, 'flhst', 'flhst', True)
        self.__copy_item(dssat_info, exp, 'fhdur', 'fhdur', True)
        if dssat_info != {}:
            exp['dssat_info'] = dssat_info
        
        # remove index variables
        id_names = ['trno', 'trno_a', 'trno_t']
        self.__remove_index(exp, id_names)

# parse inputs
parser = OptionParser()
parser.add_option("-i", "--input", dest = "inputfile", default = "exp.X", type = "string", 
                  help = "input experiment X file", metavar = "FILE")
parser.add_option("-o", "--output", dest = "outputfile", default = "exp.json", type = "string",
                  help = "output experiment JSON file", metavar = "FILE")
(options, args) = parser.parse_args()

# parse X file and convert to JSON
res = DSSATXFileInput(options.inputfile)
j = res.toJSON()

# write out JSON
json.dump(j, open(options.outputfile, 'w'), indent = 2, separators=(',', ': '))