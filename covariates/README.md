# Covariates

In order to model the peeps data, several environmental covariates were collated for analysis:

1) Discharge (m^3/s) @ Fraser River Hope monitoring station (08MF005)
2) Tidal amplitude (m) @ Port Atkinson (49.3333°N, 123.2500°W)
3) Avg daily temp (C°) @ Vancouver Int'l Airport
4) Daily total precipitation (mm) @ Vancouver Int'l Airport
5) Avg daily wind speed, direction, and vectors (km/h, °) @ Vancouver Int'l Airport
6) IR (Solar radiation, W/m2) @ the University of British Columbia Totem Field climate station (49.2562°N, 123.2494°W) [Unavailable at the time of this writing]

At the time of this writing, collecting these datasets involved a hybrid of manually downloading data from relevant websites (e.g., [ECCC](https://wateroffice.ec.gc.ca/search/real_time_e.html)) and R packages that can programmatically download data (e.g., [{tidyhydat}](https://github.com/ropensci/tidyhydat)). Manually downloaded files are all stored within the `covariates/` directory.

Code to acquire and process all environmental covariate data is in `generate_covariates.R`. The output is saved as `environmental_covariates.csv`. The output is additionally imported into the BPPeeps SQLite database (created by the `peep_cleanr.R` script) as a table called 'environmental_covariates'.