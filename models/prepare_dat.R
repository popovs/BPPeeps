## SETUP MODEL DATA
# Connect to db and pull relevant tables together

# Idea is to simply source this file if many different types
# of markdown/modeling R scripts are generated, in case the
# setup ever changes or pulls slightly different data together.

library(magrittr) # for %>% and %<>%

# Connect to DB
db <- DBI::dbConnect(RSQLite::SQLite(), "../temp/bppeeps.db")

# DAT ==============================================================
# Query table
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

# We want to later filter out any records where birds span 
# across >2 stations
dat$station_n_no <- as.numeric(dat$station_n)
dat$station_s_no <- as.numeric(dat$station_s)
dat$station_diff <- dat$station_s_no - dat$station_n_no

# Add columns as needed
dat$julian_day <- lubridate::yday(dat$survey_date)
dat$dos <- scale(dat$julian_day) # day of season variable

# Filter to the appropriate data
dat <- dat[!(dat$station_n %in% c("Intercauseway")) & !is.na(dat$station_n), ]
dat <- dat[which(dat$station_diff < 3 | is.na(dat$station_diff)), ]
# Mark also only includes data between April 15 & May 15?

# Add updated data to shiny app
#write.csv(dat, "shiny/peepr/bppeep_model_dat.csv", na = "", row.names = F)

# SR ===============================================================
# Daily species ratio
# Query table
sr <- DBI::dbGetQuery(db, "select * from daily_percent_ratio;")

# Set dates, factors etc
sr$survey_date <- as.Date(sr$survey_date)

# Add columns as needed
sr$year <- as.factor(lubridate::year(sr$survey_date))
sr$julian_day <- lubridate::yday(sr$survey_date)
sr$dos <- scale(sr$julian_day) # day of season

# Filter to appropriate data
sr <- sr[!(is.na(sr$wesa) | sr$total == 0), ]

# Disconnect from db
DBI::dbDisconnect(db)
rm(db)
