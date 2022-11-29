## SETUP MODEL DATA
# Connect to db and pull relevant tables together

# Idea is to simply source this file if many different types
# of markdown/modeling R scripts are generated, in case the
# setup ever changes or pulls slightly different data together.

db <- DBI::dbConnect(RSQLite::SQLite(), "temp/bppeeps.db")
dat <- DBI::dbGetQuery(db, "select bcl.*, p_wesa, ec.* 
                       from bp_counts_loc bcl 
                       left join daily_percent_ratio dpr on bcl.survey_date = dpr.survey_date 
                       left join environmental_covariates ec on ec.date = bcl.survey_date ;")
dat <- dplyr::select(dat, -date)
DBI::dbDisconnect(db)
rm(db)
