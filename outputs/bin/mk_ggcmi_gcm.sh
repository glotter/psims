#!/bin/bash

/project/joshuaelliott/psims/outputs/bin/out2GGCMI.py -i /project/joshuaelliott/psims/outputs/isi1/hadgem.maize.co2/out.psims.dssat45.isi1.maize.nc4 -v yield,pirrww,biom,aet,plant-day,anth-day,maty-day,gsprcp,gsrsds,sumt -p pdssat_hadgem2-es_rcp8p5 -s annual_1950_2099 -c mai --scens 1,2,3,4,5,6,7,8 --scen_names ssp2_co2_noirr,ssp2_co2_firr,ssp2_noco2_noirr,ssp2_noco2_firr,defnon_co2_noirr,defnon_co2_firr,defnon_noco2_noirr,defnon_noco2_firr

mv *.nc4 /project/joshuaelliott/psims/outputs/pDSSAT/isi1.co2/HadGEM2-ES/maize

/project/joshuaelliott/psims/outputs/bin/out2GGCMI.py -i /project/joshuaelliott/psims/outputs/isi1/hadgem.wheat.co2/out.psims.dssat45.isi1.wheat.nc4 -v yield,pirrww,biom,aet,plant-day,anth-day,maty-day,gsprcp,gsrsds,sumt -p pdssat_hadgem2-es_rcp8p5 -s annual_1950_2099 -c whe --scens 1,2,3,4,5,6,7,8 --scen_names ssp2_co2_noirr,ssp2_co2_firr,ssp2_noco2_noirr,ssp2_noco2_firr,defnon_co2_noirr,defnon_co2_firr,defnon_noco2_noirr,defnon_noco2_firr

mv *.nc4 /project/joshuaelliott/psims/outputs/pDSSAT/isi1.co2/HadGEM2-ES/wheat