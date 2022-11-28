# BPPeeps

### Code for cleaning and analyzing ECCC peep data ('BPPeeps')

This repository primarily exists to ensure any data cleaning processes used to tidy up Roberts Bank/Brunswick Point shorebird data is both backed up and documented. The goal was to leave raw data untouched so that 
1) future users will not need to contact me for the most up-to-date 'clean' data - they can just run the cleaning script `peep_cleanr.R` instead and 
2) future users can see my data cleaning rationale and make changes to my cleaning scripts as they see fit.

Running this code will import raw shorebird survey data into R, clean it, and bundle it up into a SQLite database.

## Supporting files

Certain data cleaning processes were not done in R as it was not the most expedient means to do so. See the README in the [supporting_files](/supporting_files) directory for details.

## Covariates

In order to model the peeps data, several environmental covariates were collated for analysis:

1) Discharge (m^3/s) @ Fraser River Hope monitoring station (08MF005)
2) Tidal amplitude (m) @ Port Atkinson (49.3333°N, 123.2500°W)
3) Avg daily temp (C°) @ Vancouver Int'l Airport
4) Daily total precipitation (mm) @ Vancouver Int'l Airport
5) Avg daily wind speed, direction, and vectors (km/h, °) @ Vancouver Int'l Airport
6) IR (Solar radiation, W/m2) @ the University of British Columbia Totem Field climate station (49.2562°N, 123.2494°W) [Unavailable at the time of this writing]

At the time of this writing, collecting these datasets involved a hybrid of manually downloading data from relevant websites (e.g., [ECCC](https://wateroffice.ec.gc.ca/search/real_time_e.html)) and R packages that can programmatically download data (e.g., [{tidyhydat}](https://github.com/ropensci/tidyhydat)). Manually downloaded files are all stored within the `covariates/` directory.
