#!/bin/bash

models=(gfdl hadgem ipsl miroc noresm)
models_full=(GFDL-ESM2M HadGEM2-ES IPSL-CM5A-LR MIROC-ESM-CHEM NorESM1-M)

crops=(maize rice soy wheat)
crops_short=(mai ric soy whe)

translator=/project/joshuaelliott/psims/outputs/bin/out2GGCMI.py

indir=/project/joshuaelliott/psims/outputs/isi1
outdir=/project/joshuaelliott/psims/outputs/pDSSAT/isi1.co2

vars=yield,pirrww,biom,aet,plant-day,anth-day,maty-day,gsprcp,gsrsds,sumt
scens=ssp2_co2_noirr,ssp2_co2_firr,ssp2_noco2_noirr,ssp2_noco2_firr,defnon_co2_noirr,defnon_co2_firr,defnon_noco2_noirr,defnon_noco2_firr
scen_nums=1,2,3,4,5,6,7,8

for ((m=0; m<${#models[@]}; m++)); do
  mod=${models[$m]}
  mod_full=${models_full[$m]}
  mod_lower=$(echo $mod_full | tr '[:upper:]' '[:lower:]')
  for ((c=0; c<${#crops[@]}; c++)); do
    crop=${crops[$c]}
    crop_short=${crops_short[$c]}
    file=$(ls $indir/$mod.$crop.co2/*.$crop.nc4)
    `$translator -i $file -v $vars -p "pdssat_"$mod_lower"_rcp8p5" -s annual_1950_2099 -c $crop_short --scens $scen_nums --scen_names $scens`
    mv *.nc4 $outdir/$mod_full/$crop
  done
done
