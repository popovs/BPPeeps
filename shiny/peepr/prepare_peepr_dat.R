# This script executes the same exact code used to generate any models
# PDF reports: first, it sources prepare_dat.R, which connects to the 
# bppeeps database and queries + manipulates the base model data. 
# Then, it sources the code in the binomial species ratio PDF. The
# binomial species ratio is complex enough that it is documented within
# it's own Rmd file. Finally, this script then saves the resulting dat
# dataframe into "peepr_dat.csv" for use in the Shiny app.

source("models/prepare_dat.R")
source(knitr::purl("models/binomial_sr.Rmd", quiet=TRUE))

# Add predicted WESA/DUNL ratio generated in binomial_sr to the dat dataset
# and calculate raw + log WESA/DUNL population for each record
dat <- merge(dat, yrs[,c("year", "julian_day", "predicted_ratio")], 
             by = c("year", "julian_day"))
names(dat)[grep("predicted_ratio", names(dat))] <- "predicted_p_wesa" # Rename so it's a bit clearer what it refers to
dat$wesa_count <- round(dat$final_count * dat$predicted_p_wesa, 0)
dat$dunl_count <- dat$final_count - dat$wesa_count
dat$log_wesa <- log(dat$wesa_count + 1)
dat$log_dunl <- log(dat$dunl_count + 1)

# Rearrange the columns a bit
dat <- dat %>% dplyr::select(year, survey_date, julian_day, dos, 
                             start_time, n_s, station_n, station_s, final_count,
                             wesa_count, dunl_count, log_wesa, log_dunl, 
                             p_wesa, predicted_p_wesa, dplyr::everything())

write.csv(dat, "shiny/peepr/peepr_dat.csv", na = "", row.names = FALSE)
