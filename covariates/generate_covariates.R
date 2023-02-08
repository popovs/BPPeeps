## GENERATE COVARIATES

# This script connects to the bppeeps.db SQLite file,
# extracts the dates that will be used for later analysis,
# and pulls ECCC weather data for those dates.
# Finally, it saves the ECCC data as .Rda files for later use.

# All covariates:
# 01 Discharge (m^3/s) @ Fraser River Hope monitoring station (08MF005)
# 02 Tidal amplitude (m) @ Port Atkinson (49.3333°N, 123.2500°W)
# 03 Avg daily temp (C°) @ Vancouver Int'l Airport
# 04 Daily total precipitation (mm) @ Vancouver Int'l Airport
# 05 Avg daily wind speed, direction, and vectors (km/h, °) @ Vancouver Int'l Airport
# 06 IR (Solar radiation, W/m2) @ the University of British Columbia Totem Field climate station (49.2562°N, 123.2494°W)

# SETUP ----

# First, install rclimateca and tidyhydat packages

# Note that at the time of writing this script, these packages
# are no longer maintained/buggy on CRAN. As such, the Github development
# versions are installed. Two packages are defunct at the time of writing
# (rclimateca + weathercan) and one is under active development (tidyhydat),
# so function usage may change drastically in the future.

# install.packages("devtools")
#devtools::install_github("paleolimbot/rclimateca")
#devtools::install_github("ropensci/tidyhydat")
#devtools::install_github("ropensci/weathercan")

library(rclimateca)
library(tidyhydat)

# Download hydat data
# TODO: make this reproducible later. This just doesn't play well with a renv environment.
download_hydat() # 1st attempt - will it create dir in app support?
#download_hydat(dl_hydat_here = "renv/local/hydat") # ~3 min - only needs to be run once
#hydat_path <- "renv/local/hydat/Hydat.sqlite3"
#hy_set_default_db(hydat_path = hydat_path) # doesn't work?
hy_downloaded_db() # Check path.

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
db <- DBI::dbConnect(RSQLite::SQLite(), "temp/bppeeps.db")
dates <- DBI::dbGetQuery(db, "select distinct(date(date_time_pdt)) from bp_counts_all;")

dates <- as.character(unlist(dates))
dates <- dates[!is.na(dates)]
dates <- as.Date(dates)

# 01 Discharge (m^3/s) ----
# @ Fraser River Hope monitoring station (08MF005)

# First get historical daily data
hope_hist <- hy_daily_flows(station_number = hope,
                            start_date = min(dates),
                            end_date = max(dates))

# Next get remaining data
# Historical data only goes up to 2020, while realtime
# data only goes for the last 30 days. Currently no good
# web scraping API to get the intervening dates.
# So, for now, downloading the data manually from this
# URL, and loading it as a csv. 
# https://wateroffice.ec.gc.ca/report/real_time_e.html?stn=08MF005&mode=Graph&startDate=2021-01-01&endDate=2022-05-11&prm1=47&y1Max=&y1Min=&prm2=47&y2Max=&y2Min=
hope_recent <- read.csv("covariates/08MF005_discharge_20221125.csv", skip = 9)
hope_recent <- janitor::clean_names(hope_recent)
# Remove timestamp
hope_recent$date_pst <- sub(" .*", "", hope_recent$date_pst)
# Daily mean by date
hope_recent <- aggregate(value_m_s ~ date_pst, hope_recent, mean)
# Match columns and merge
hope_recent$date_pst <- as.Date(hope_recent$date_pst)
hope_recent$value_m_s <- as.numeric(hope_recent$value_m_s)
names(hope_recent) <- c("Date", "Value")
hope_recent$STATION_NUMBER <- hope

flow <- dplyr::bind_rows(hope_hist, hope_recent)

rm(hope_hist)
rm(hope_recent)
flow$Parameter <- "Flow (m3/s)"
names(flow)[4] <- "flow"

# 02 Tidal amplitude (m) ----
# @ Port Atkinson (49.3333°N, 123.2500°W)

amp <- read.csv("covariates/tidal_range_1991_to_2022.csv")

# YVR weather download ----

# Using weathercan

# Daily - for temp + precip (won't take long)
yvr_weather <- weather_dl(station_ids = c(yvr[[1]], yvr[[2]]), 
                          start = min(dates), 
                          end = max(dates), 
                          interval = "day", 
                          verbose = TRUE)

# Hourly - for wind (will take forever and if done all at once it
# can time out - so it is done year at a time. Kept verbose on to 
# keep track of things. Takes an hour or so.)
yvr_wind <- plyr::ldply(dates, function(x) {
  weather_dl(station_ids = c(yvr[[1]], yvr[[2]]), 
             start = x, 
             end = x, 
             interval = "hour", 
             verbose = TRUE)
})

# 03 Avg daily temp (C°) ----
# @ Vancouver Int'l Airport

temp <- aggregate(mean_temp ~ date, yvr_weather, mean)

# 04 Daily total precipitation (mm) ----
# @ Vancouver Int'l Airport

precip <- aggregate(total_precip ~ date, yvr_weather, sum)

# 05 Avg daily wind speed + direction (km/h, °) ----
# @ Vancouver Int'l Airport

# u/v calculation snippet authored by David D. Hope, 
# from 2021 Canham et al. paper
# u and v represent westerly and southerly wind vectors, 
# respectively

uv <- yvr_wind %>%
  # Filter out flagged data here
  dplyr::group_by(date) %>%
  dplyr::mutate( rad_dir = wind_dir* 10* pi/180 ,
          v = abs(wind_spd) * sin(rad_dir),
          u = abs(wind_spd)* cos(rad_dir)) %>%
  dplyr::summarize(u = median(u, na.rm=T),
                   v = median(v, na.rm=T),
                   Windspd = mean(wind_spd, na.rm=T),
                   s_sin = sum(sin(rad_dir), na.rm = T),
                   s_cos = sum(cos(rad_dir),na.rm=T),
                   .groups = 'drop') %>%
  dplyr::mutate(
    WindDir = atan2(s_sin, s_cos),
    WindDeg = WindDir * 180 / pi) %>%
  dplyr::mutate(Date = lubridate::ymd(date),
         Year = lubridate::year(Date),
         Month = lubridate::month(Date),
         DoY = lubridate::yday(Date))

# 06 IR (W/m2) ----
# @ the University of British Columbia Totem Field climate station (49.2562°N, 123.2494°W)

# As of this writing, IR is not an available column
ubc_weather <- weather_dl(station_ids = ubc[[1]], 
                          start = min(dates), 
                          end = max(dates), 
                          interval = "day", 
                          verbose = TRUE)

#MERGE & FILTER ----
# Merge together the 6 datasets, then filter down to only the dates
# and columns that are needed

# amp, flow, precip, temp, uv (no IR as of yet)

# Clean colnames of all datasets
amp <- janitor::clean_names(amp)
flow <- janitor::clean_names(flow)
precip <- janitor::clean_names(precip)
temp <- janitor::clean_names(temp)
uv <- janitor::clean_names(uv)

# Prepare dates for joins
dates <- as.data.frame(dates)
names(dates) <- "date"
dates$date <- as.character(dates$date)

flow$date <- as.character(flow$date)
precip$date <- as.character(precip$date)
temp$date <- as.character(temp$date)
uv$date <- as.character(uv$date)

# Join
cov <- dates %>% 
  dplyr::left_join(amp) %>%
  dplyr::left_join(flow) %>%
  dplyr::left_join(precip) %>%
  dplyr::left_join(temp) %>%
  dplyr::left_join(uv) %>%
  dplyr::select(date, elev_min, elev_max, elev_median, elev_mean,
                elev_range, flow, total_precip, mean_temp, u, v,
                windspd, wind_deg)

# UPDATE DB ----
# Add the newly generated cov file to the bppeeps db as a table
DBI::dbWriteTable(db, "environmental_covariates", cov, overwrite = TRUE)
DBI::dbDisconnect(db)
