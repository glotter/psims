# Make and build the DSSAT Model
make

# Change path settings in DSSATPRO.FLE
sed -e 's=\/home\/jelliott\/dssat_build\/='"$PWD"'\/=g' DSSATPRO.FLE > temp
mv temp DSSATPRO.FLE

# Make a OUTPUT folder to store all final summaries
mkdir -p OUTPUT
mkdir -p WEATHER

