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
                       select bcl.survey_date, start_time, sweep, station_n, station_s, 
                       sum(final_count) as final_count, p_wesa, count as raptor_count, ec.*
                       from bp_counts_loc bcl 
                       left join daily_percent_ratio dpr on bcl.survey_date = dpr.survey_date 
                       left join environmental_covariates ec on ec.date = bcl.survey_date
                       left join dr on r_date = bcl.survey_date
                       group by station_n, bcl.survey_date, sweep
                       order by bcl.survey_date, start_time, sweep;")
dat <- dplyr::select(dat, c(-date))

# Add zeroes in cases where no observation per station was noted
# so we have an even number of observations per sweep
z <- expand.grid(unique(dat$survey_date), unique(dat$station_n), KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
names(z) <- c("survey_date", "station_n")
z$sweep <- "1"

z2 <- expand.grid(unique(dat[["survey_date"]][grep("2", dat$sweep)]), unique(dat$station_n), KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
names(z2) <- c("survey_date", "station_n")
z2$sweep <- "2"

z3 <- expand.grid(unique(dat[["survey_date"]][grep("3", dat$sweep)]), unique(dat$station_n), KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
names(z3) <- c("survey_date", "station_n")
z3$sweep <- "3"

z <- rbind(z, z2, z3)
z$final_count <- 0

dat <- dplyr::bind_rows(z, dat)
# TODO: figure this out??
dat <- aggregate(. ~ survey_date + station_n + sweep + start_time + station_s, dat, sum, na.rm = T)
dat <- dat %>% dplyr::select(survey_date, start_time, sweep, station_n, station_s, final_count, dplyr::everything())

rm(z, z2, z3)

# Extract nrow of full dataset
# This will be used to keep track of how many datapoints are lost
# at each filtering step
filter_s <- "Full dataset" # filter step
filter_n <- nrow(dat) # n records
filter_d <- length(unique(dat$survey_date)) # n dates affected

# Add tide rising/falling
dat$date_time_pdt <- as.POSIXct(paste(dat$survey_date, dat$start_time), format = "%Y-%m-%d %H:%M")
dat$date_time_utc <- lubridate::as_datetime(dat$date_time_pdt, tz = "UTC")
# Now calculate tides - at the time/date of the survey, and one hour later
t <- earthtide::calc_earthtide(utc = dat$date_time_utc,
                               method = 'gravity',
                               latitude = 49.054646, 
                               longitude = -123.144756)
t2 <- earthtide::calc_earthtide(utc = (dat$date_time_utc + 3600), 
                                method = 'gravity',
                                latitude = 49.054646, 
                                longitude = -123.144756)
t_diff <- t2$gravity - t$gravity # if gravity @ time 2 > gravity at time 1, the tide is HIGHER
dat$tide <- ifelse(t_diff > 0, "rising", "falling")
rm(t, t2, t_diff)

# Set dates, factors etc.
dat$survey_date <- as.Date(dat$survey_date)
station_levels <- c("Canoe Pass", "Brunswick Point", "View corner", "Pilings", "Bend", "34th St pullout", "Coal Port")
dat$station_n <- factor(dat$station_n, levels = station_levels)
dat$station_s <- factor(dat$station_s, levels = station_levels)
dat$tide <- as.factor(dat$tide)
rm(station_levels)

# We want to later filter out any records where birds span 
# across >2 stations
dat$station_n_no <- as.numeric(dat$station_n)
dat$station_s_no <- as.numeric(dat$station_s)
dat$station_diff <- dat$station_s_no - dat$station_n_no

# Filter to the appropriate data
# Remove NA and 0 records
# dat <- dat[which(!is.na(dat$final_count)),]
# filter_s <- c(filter_s, "Remove NA count records")
# filter_n <- c(filter_n, nrow(dat))
# filter_d <- c(filter_d, length(unique(dat$survey_date)))

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
dat <- dat[!(dat$station_n %in% c("Canoe Pass", "Intercauseway")) & !is.na(dat$station_n), ]

filter_s <- c(filter_s, "Exclude Canoe Pass, Intercauseway, and NA stations (e.g. location was simply 'inner mud', 'mumblies', 'flying', etc.)")
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
dat$ordinal_day <- lubridate::yday(dat$survey_date)
dat$dos <- scale(dat$ordinal_day) # day of season variable
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
sr$ordinal_day <- lubridate::yday(sr$survey_date)
sr$dos <- scale(sr$ordinal_day) # day of season

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
dt$ordinal_day <- lubridate::yday(dt$survey_date)
dt$dos <- scale(dt$ordinal_day) # day of season variable

# DISCONNECT =======================================================
# Disconnect from db
DBI::dbDisconnect(db)
rm(db)

