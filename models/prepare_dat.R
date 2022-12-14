## SETUP MODEL DATA
# Connect to db and pull relevant tables together

# Idea is to simply source this file if many different types
# of markdown/modeling R scripts are generated, in case the
# setup ever changes or pulls slightly different data together.

library(magrittr) # for %>% and %<>%

# Connect to DB
db <- DBI::dbConnect(RSQLite::SQLite(), here::here("temp", "bppeeps.db"))

# DAT ==============================================================
# Query table
dat <- DBI::dbGetQuery(db, "with dr as (select date(date_time_pdt) as r_date, sum(count) as count
                       from raptors group by r_date)
                       select bcl.survey_date, start_time, station_n, station_s, 
                       sum(final_count) as final_count, p_wesa, count as raptor_count, ec.*
                       from bp_counts_loc bcl 
                       left join daily_percent_ratio dpr on bcl.survey_date = dpr.survey_date 
                       left join environmental_covariates ec on ec.date = bcl.survey_date
                       left join dr on r_date = bcl.survey_date
                       group by station_n, bcl.survey_date
                       order by bcl.survey_date;")
dat <- dplyr::select(dat, c(-date))

# Extract nrow of full dataset
# This will be used to keep track of how many datapoints are lost
# at each filtering step
filter_s <- "Full dataset" # filter step
filter_n <- nrow(dat) # n records
filter_d <- length(unique(dat$survey_date)) # n dates affected

# Set dates, factors etc.
dat$survey_date <- as.Date(dat$survey_date)
station_levels <- c("Canoe Pass", "Brunswick dike", "Brunswick Point", "View corner", "Pilings", "Bend", "34th St pullout", "Coal Port")
dat$station_n <- factor(dat$station_n, levels = station_levels)
dat$station_s <- factor(dat$station_s, levels = station_levels)
rm(station_levels)

# We want to later filter out any records where birds span 
# across >2 stations
dat$station_n_no <- as.numeric(dat$station_n)
dat$station_s_no <- as.numeric(dat$station_s)
dat$station_diff <- dat$station_s_no - dat$station_n_no

# Filter to the appropriate data
# Remove NA and 0 records
dat <- dat[which(!is.na(dat$final_count)),]
filter_s <- c(filter_s, "Remove NA count records")
filter_n <- c(filter_n, nrow(dat))
filter_d <- c(filter_d, length(unique(dat$survey_date)))

# Include only survey period dates and where total # of birds < 1000
# First select dates where tot # of birds < 1000
tmp <- aggregate(final_count ~ survey_date, dat, sum)
tmp <- tmp[1][tmp[2] < 1000]
dat <- dat[!(dat$survey_date %in% as.Date(tmp)),]
rm(tmp)

filter_s <- c(filter_s, "Exclude dates where total # of birds < 1000")
filter_n <- c(filter_n, nrow(dat))
filter_d <- c(filter_d, length(unique(dat$survey_date)))

# Include only survey dates
dat <- dat[(format(dat$survey_date, "%m-%d") >= "04-15"), ]
dat <- dat[(format(dat$survey_date, "%m-%d") <= "05-15"), ]

filter_s <- c(filter_s, "Exclude dates outside of survey period (<04-15 or >05-15)")
filter_n <- c(filter_n, nrow(dat))
filter_d <- c(filter_d, length(unique(dat$survey_date)))

# Include only stations of interest
dat <- dat[!(dat$station_n %in% c("Intercauseway")) & !is.na(dat$station_n), ]

filter_s <- c(filter_s, "Exclude Intercauseway and NA stations (e.g. location was simply 'inner mud', 'mumblies', 'flying', etc.)")
filter_n <- c(filter_n, nrow(dat))
filter_d <- c(filter_d, length(unique(dat$survey_date)))

# Include only survey dates where birds span <3 stations
tmp <- unique(dat[["survey_date"]][which(dat$station_diff > 2)])
dat <- dat[!(dat$survey_date %in% as.Date(tmp)), ]

filter_s <- c(filter_s, "Exclude records where only bird count occurs in location that spans >2 stations (e.g., 'BP to CP')")
filter_n <- c(filter_n, nrow(dat))
filter_d <- c(filter_d, length(unique(dat$survey_date)))

# Add dat columns as needed
dat$year <- as.factor(lubridate::year(dat$survey_date))
dat$julian_day <- lubridate::yday(dat$survey_date)
dat$dos <- scale(dat$julian_day) # day of season variable
dat$n_s <- ifelse(dat$station_n %in% c("Canoe Pass", "Brunswick Point", "View corner", "Pilings", "Bend"),
                  "N", "S")
dat$n_s <- as.factor(dat$n_s)

# Remove dat columns as needed
dat <- dplyr::select(dat, -c(station_n_no, station_s_no, station_diff))

# Filtering steps table
filtering <- data.frame(cbind(filter_s, filter_d, filter_n))
filtering[[2]] <- as.numeric(filtering[[2]])
filtering[[3]] <- as.numeric(filtering[[3]])
names(filtering) <- c("filter_step", "n_survey_dates", "n_records")
filtering$survey_dates_lost <- c('NA', diff(filtering[[2]]))
filtering$total_records_lost <- c('NA', diff(filtering[[3]]))

rm(filter_d, filter_n, filter_s, tmp)

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

# DAILY TOTALS =====================================================
# Daily totals, for use in yearly population trend model
dt <- DBI::dbGetQuery(db, "select * from daily_total left join environmental_covariates ec on survey_date = ec.date;")

dt$survey_date <- as.Date(dt$survey_date)

# Include only survey dates
dt <- dt[(format(dt$survey_date, "%m-%d") >= "04-15"), ]
dt <- dt[(format(dt$survey_date, "%m-%d") <= "05-15"), ]

# Add columns as needed
dt$year <- as.factor(lubridate::year(dt$survey_date))
dt$julian_day <- lubridate::yday(dt$survey_date)
dt$dos <- scale(dt$julian_day) # day of season variable

# DISCONNECT =======================================================
# Disconnect from db
DBI::dbDisconnect(db)
rm(db)

