## SETUP MODEL DATA
# Connect to db and pull relevant tables together

# Idea is to simply source this file if many different types
# of markdown/modeling R scripts are generated, in case the
# setup ever changes or pulls slightly different data together.

library(magrittr) # for %>% and %<>%

# Connect to DB and query tables
db <- DBI::dbConnect(RSQLite::SQLite(), "temp/bppeeps.db")
dat <- DBI::dbGetQuery(db, "select bcl.*, p_wesa, ec.* 
                       from bp_counts_loc bcl 
                       left join daily_percent_ratio dpr on bcl.survey_date = dpr.survey_date 
                       left join environmental_covariates ec on ec.date = bcl.survey_date ;")
dat <- dplyr::select(dat, -date)

# Set dates, factors etc.
dat$survey_date <- as.Date(dat$survey_date)
station_levels <- c("Canoe Pass", "Brunswick dike", "Brunswick Point", "View corner", "Pilings", "Bend", "34th St pullout", "Coal Port")
dat$station_n <- factor(dat$station_n, levels = station_levels)
dat$station_s <- factor(dat$station_s, levels = station_levels)
rm(station_levels)

dat %<>% dplyr::mutate_at(c("mumblies_yn", 
                            "mud_yn",
                            "marsh_yn",
                            "tide_edge_yn",
                            "flying_yn"),
                          as.logical)

# We want to filter out any records where birds span across >2 stations
dat$station_n_no <- as.numeric(dat$station_n)
dat$station_s_no <- as.numeric(dat$station_s)
dat$station_diff <- dat$station_s_no - dat$station_n_no

# Add columns as needed
dat$julian_day <- lubridate::yday(dat$survey_date)

# Filter to the appropriate data
dat <- dat[!(dat$station_n %in% c("Intercauseway")) & !is.na(dat$station_n), ]
dat <- dat[which(dat$station_diff < 3 | is.na(dat$station_diff)), ]

# Add updated data to shiny app
write.csv(dat, "shiny/peepr/bppeep_model_dat.csv", na = "", row.names = F)

# Disconnect from db
DBI::dbDisconnect(db)
rm(db)
