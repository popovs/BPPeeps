## GENERATE COVARIATES

# This script connects to the bppeeps.db SQLite file,
# extracts the dates that will be used for later analysis,
# and pulls ECCC weather data for those dates.
# Finally, it saves the ECCC data as .Rda files for later use.

# All covariates:
# 01 Discharge (m^3/s) @ Fraser River Hope monitoring station (08MF005)
# 02 Tidal amplitude @ Port Atkinson (49.3333°N, 123.2500°W)
# 03 Avg daily temp (C°) @ Vancouver Int'l Airport
# 04 Daily total precipitation (mm) @ Vancouver Int'l Airport
# 05 Avg daily wind speed (km/h) @ Vancouver Int'l Airport
# 06 Avg daily wind direction (°) @ Vancouver Int'l Airport
# 07 u/v (westerly, southerly) wind vectors 
# 08 IR (Solar radiation, W/m2) @ the University of British Columbia Totem Field climate station (49.2562°N, 123.2494°W)

# SETUP ----

# First, install rclimateca and tidyhydat packages

# Note that at the time of writing this script, the packages
# are no longer maintained/buggy on CRAN. As such, the Github development
# versions are installed.

# install.packages("devtools")
devtools::install_github("paleolimbot/rclimateca")
remotes::install_github("ropensci/tidyhydat")

library(rclimateca)
library(tidyhydat)

# Download hydat data
# TODO: make this reproducible later. This just doesn't play well with a renv environment.
download_hydat(dl_hydat_here = "renv/local/hydat") # ~3 min - only needs to be run once
hy_set_default_db(hydat_path = "renv/local/hydat/Hydat.sqlite3") # doesn't work?
hydat_path <- "renv/local/hydat/Hydat.sqlite3"
hy_downloaded_db() # Check it's in renv path

# Get station names that we will use for data download
# Airport is split into two datasets, because one monitoring station
# recorded from 1937-2013 then closed. The second monitoring station
# recorded from 2013-present. We exclude the "HWOS" monitoring station,
# which has no data.
yvr <- ec_climate_search_locations("Vancouver int")[!grepl("HWOS", ec_climate_search_locations("Vancouver int"))] # Two locations (51442 & 889)
ubc <- ec_climate_search_locations(c(-123.2494, 49.2562))[1] # UBC Totem pole (903)
hope <- "08MF005" # At time of writing, Hope station number is 08MF005

# Pull peeps data ----
## TODO: change this later so that peep db is stored in better location..
db <- DBI::dbConnect(RSQLite::SQLite(), "Output/bppeeps.db")
dates <- DBI::dbGetQuery(db, "select distinct(date) from bp_counts;")
DBI::dbDisconnect(db)
rm(db)

dates <- as.character(unlist(dates))
dates <- dates[!is.na(dates)]
dates <- as.Date(dates)

# 01 Discharge (m^3/s) ----
# @ Fraser River Hope monitoring station (08MF005)

# First get historical daily data
hope_hist <- hy_daily_flows(station_number = hope,
                            start_date = min(dates),
                            end_date = max(dates),
                            hydat_path = hydat_path)

# Next get remaining data
# Because this is realtime data, it's every 10 mins.
# To get the daily flow, need to take the mean per day
hope_rt <- realtime_dd(hope) %>% realtime_daily_mean()


# Next get any realtime daily data for any dates not included in
# historical data

# 02 Tidal amplitude ----
# @ Port Atkinson (49.3333°N, 123.2500°W)



# 03 Avg daily temp (C°) ----
# @ Vancouver Int'l Airport



# 04 Daily total precipitation (mm) ----
# @ Vancouver Int'l Airport



# 05 Avg daily wind speed (km/h) ----
# @ Vancouver Int'l Airport



# 06 Avg daily wind direction (°) ----
# @ Vancouver Int'l Airport



# 07 u/v ----
# Westerly + southerly wind vectors 



# 08 IR (W/m2) ----
# @ the University of British Columbia Totem Field climate station (49.2562°N, 123.2494°W)

