library(magrittr)

# Connect to the bppeeps database

bppeeps <- DBI::dbConnect(RSQLite::SQLite(), "../../Output/bppeeps.db")

counts <- DBI::dbGetQuery(bppeeps, "select * from bp_counts;")
locations <- DBI::dbGetQuery(bppeeps, "select * from locations;")
sr <- DBI::dbGetQuery(bppeeps, "select * from species_ratios;")

counts <- merge(counts, locations,
                by.x = "location", by.y = "original_location_name",
                all.x = TRUE)

names(counts)[1] <- "original_location_name"
names(counts)[grep("cleaned", names(counts))] <- "cleaned_location_name"

counts <- counts %>% dplyr::select(date, 
                                   date_time_pdt,
                                   station_n,
                                   station_s,
                                   sub_location, 
                                   dplyr::everything())

# Coerce column types
# Dates
counts$date <- as.POSIXct(counts$date)
counts$date_time_pdt <- as.POSIXct(counts$date_time_pdt, tz = "Canada/Pacific")
counts$sweep_start_pdt <- as.POSIXct(counts$sweep_start_pdt, tz = "Canada/Pacific")
counts$sweep_end_pdt <- as.POSIXct(counts$sweep_end_pdt, tz = "Canada/Pacific")
counts$survey_start_pdt <- as.POSIXct(counts$survey_start_pdt, tz = "Canada/Pacific")
counts$survey_end_pdt <- as.POSIXct(counts$survey_end_pdt, tz = "Canada/Pacific")
counts$approx_11_5_ft_tide_or_best_survey_pdt <- as.POSIXct(counts$approx_11_5_ft_tide_or_best_survey_pdt, tz = "Canada/Pacific")
counts$high_tide_time_pdt <- as.POSIXct(counts$high_tide_time_pdt, tz = "Canada/Pacific")
# Factors
counts$station_n <- factor(counts$station_n, levels = c("Westham Island", 
                                                        "Canoe Pass",
                                                        "Brunswick Point",
                                                        "Brunswick dike",
                                                        "View corner",
                                                        "Pilings",
                                                        "Bend",
                                                        "34th St pullout",
                                                        "Coal Port",
                                                        "Intercauseway"))
counts$station_s <- factor(counts$station_s, levels = c("Westham Island", 
                                                        "Canoe Pass",
                                                        "Brunswick Point",
                                                        "Brunswick dike",
                                                        "View corner",
                                                        "Pilings",
                                                        "Bend",
                                                        "34th St pullout",
                                                        "Coal Port",
                                                        "Intercauseway"))
