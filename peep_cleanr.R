library(magrittr) # for assignment pipe '%<>%' operator

## 01 IMPORT ----
# Relative file paths to this source file
f <- list.files("RobertsBankShorebirdSurveys/")
f <- f[!grepl("^~", f)] # Drop hidden excel files, if present
raw <- list()
for (i in 1:length(f)) {
  message("Reading ", f[[i]], "...")
  path <- paste0("RobertsBankShorebirdSurveys/", f[[i]])
  sheets <- readxl::excel_sheets(path)
  # Drop any sheets with "chart" or "Sheet" in them
  # EXCEPT for '98 data, where "counts" is in "Sheet1"
  if (f[[i]] == "BPPEEP98.xls") {
    sheets <- sheets[grepl("^Sheet1$", sheets)]
  } else {
    sheets <- sheets[!grepl("chart|sheet", tolower(sheets))]
  }
  tmp <- lapply(sheets,
                function(x)
                  suppressMessages(
                    as.data.frame(
                      readxl::read_excel(path,
                                         sheet = x,
                                         col_names = FALSE,
                                         col_types = "text", # read all cols as "character" - prevents it importing as a list of lists
                                         na = c("", "#N/A", "-")
                      )
                    )
                  )
  )
  names(tmp) <- sheets
  if (f[[i]] == "BPPEEP98.xls") names(tmp) <- "Counts"
  x <- gsub(".xls|.xlsx", "", f[[i]])
  raw[[x]] <- tmp
  #gdata::mv(from = "tmp", to = x)
  rm(path)
  rm(sheets)
  rm(tmp)
  rm(x)
}
rm(i)

## 02 CLEAN TABS ----
# Clean raw data

# Begin by cleaning the names of raw tabs
# Different cleaning strategies will apply to each tab name
# Core tabs that are repeated across most datasets will be
# standardized into the following 3-6 tables:
# 1) counts
# 2) species_ratios
# 3) summary_totals (possibly redundant/could be a view)
# 4) wesa_totals (possibly redundant/could be a view)
# 5) dunl_totals (possibly redundant/coule be a view)
# 6) raptors (+ raptors fixes)
clean_tabs <- lapply(raw, function (x) {
  janitor::make_clean_names(names(x), parsing_option = 2)
})

# Check the tab names that were produced - gives idea
# of further tab name cleaning
sort(unique(unlist(clean_tabs)))

for (i in 1:length(clean_tabs)) {
  cn <- clean_tabs[[i]]
  # Fix 'sumary' typo
  cn[grepl("sumary", cn)] <- gsub(x = cn[grepl("sumary", cn)], pattern = "sumary", replacement = "summary")
  # 'boundary_bay' -> 'boundary_bay_counts'
  cn[grepl("boundary_bay", cn)] <- "boundary_bay_counts"
  # Standardize 'estimated_[spp]_daily_totals'
  cn[grepl("dunl_daily|dunld_aily", cn)] <- "estimated_daily_dunl_totals"
  cn[grepl("wesa_daily|wesad_aily", cn)] <- "estimated_daily_wesa_totals"
  # Standardize 'species_ratios'
  cn[grepl("sp_ratio", cn)] <- "species_ratios"
  # 'summary_peeps_totals' -> 'summary_totals'
  cn[grepl("summary_peeps_totals", cn)] <- "summary_totals"
  # Standardize yearly totals -> "summary_totals"
  cn[grepl("^x|^totals", cn)] <- "summary_totals"
  clean_tabs[[i]] <- cn
  rm(cn)
}

# Re-check tab names - much better
sort(unique(unlist(clean_tabs)))

# 'locations' and 'count_summary' can be cut - 
# they only appear in one dataset and contain no 
# meaningful raw data.
plyr::count(unlist(clean_tabs))

# Assign clean tab names to raw data.
for (i in 1:length(raw)) {
  names(raw[[i]]) <- clean_tabs[[i]]
}

# Extract and group data from each unique tab into lists
# These will form the basis of each of our tables
tabs <- sort(unique(unlist(clean_tabs)))
for (i in 1:length(tabs)) {
  tmp <- sapply(raw, function(x) {x[[tabs[i]]]})
  tmp <- tmp[!sapply(tmp,is.null)]
  x <- paste0(tabs[i], "_list")
  gdata::mv(from = "tmp", to = x)
  rm(x)
}

# Next section - clean each unique table we've just generated

## 03 REMOVE SUPERFLUOUS TABLES ----
# Several tables are just tabs in the excel files for 
# notekeeping and are not needed in the database
rm(count_summary_list) # This was a pivot table
# 'summary_totals_list', 'compliation_daily_wesa_dunl_list'  
# could probably be removed as well - 
# they just compile data from other tabs
rm(summary_totals_list)
rm(compilation_daily_wesa_dunl_list)
rm(estimated_daily_dunl_totals_list)
rm(estimated_daily_wesa_totals_list)
rm(locations_list) # will be done manually to match new map provided

cleaned <- list()

## 04 CLEAN 'boundary_bay_counts' ----
# 'One day count of shore birds at Boundary Bay - May 8,  2001'
boundary_bay_counts <- boundary_bay_counts_list$BPPEEP01

# Clean colnames
cn <- janitor::make_clean_names(boundary_bay_counts[4,])
cn[11] <- "high_tide_height_ft"
cn[12] <- "high_tide_time_pdt"
boundary_bay_counts <- boundary_bay_counts[5:10,]
names(boundary_bay_counts) <- cn

# Drop irrelevant cols (make sense in excel, but not in db)
boundary_bay_counts <- boundary_bay_counts[,!(names(boundary_bay_counts) %in% "total")]

# Coerce column types
boundary_bay_counts %<>%
  dplyr::mutate_at(c("time",
                     "wesa",
                     "wesa_dunl",
                     "bbpl",
                     "sepl",
                     "dowit",
                     "shore",
                     "high_tide_height_ft",
                     "high_tide_time_pdt"),
                   as.numeric)

boundary_bay_counts$time <- openxlsx::convertToDateTime(boundary_bay_counts$time, origin = "2001-05-08")
boundary_bay_counts$high_tide_time_pdt <- openxlsx::convertToDateTime(boundary_bay_counts$high_tide_time_pdt, origin = "2001-05-08")

names(boundary_bay_counts)[1] <- "date_time_pdt"

boundary_bay_counts$date_time_pdt <- lubridate::force_tz(boundary_bay_counts$date_time_pdt, tzone = "Canada/Pacific")
boundary_bay_counts$high_tide_time_pdt <- lubridate::force_tz(boundary_bay_counts$high_tide_time_pdt, tzone = "Canada/Pacific")

# Fill unspecified dates
# Raw data specifies this was all on same date, but time only
# provided for two records
boundary_bay_counts[["date_time_pdt"]][is.na(boundary_bay_counts$date_time_pdt)] <- lubridate::make_datetime(year = 2001, month = 5, day = 8, hour = 0, min = 0, tz = "Canada/Pacific")

# Move weather to 'notes'
boundary_bay_counts[1,"notes"] <- "cloudy weather; strong wind"
boundary_bay_counts <- boundary_bay_counts[,1:11]

# Pivot table to line up with same format as canoe_pass_counts
boundary_bay_counts <- tidyr::pivot_longer(boundary_bay_counts, cols = c(wesa:shore), names_to = "species", values_to = "count")

# Clean species
boundary_bay_counts$species <- toupper(boundary_bay_counts$species)
boundary_bay_counts$species <- gsub("_", "/", boundary_bay_counts$species)

# Finish up
rm(boundary_bay_counts_list)
cleaned[[1]] <- boundary_bay_counts
names(cleaned)[1] <- "boundary_bay_counts"
rm(boundary_bay_counts)

## 05 CLEAN 'canoe_pass_counts' ----
canoe_pass_counts <- canoe_pass_counts_list$BPPeeps2015

# Clean colnames
cn <- janitor::make_clean_names(canoe_pass_counts[1,])
canoe_pass_counts <- canoe_pass_counts[2:nrow(canoe_pass_counts),]
names(canoe_pass_counts) <- cn

# Coerce column types
canoe_pass_counts$date_time_pdt <- as.numeric(canoe_pass_counts$date) + as.numeric(canoe_pass_counts$time)
canoe_pass_counts$date_time_pdt <- janitor::convert_to_datetime(canoe_pass_counts$date_time_pdt, tz = "Canada/Pacific")

canoe_pass_counts$species <- as.factor(canoe_pass_counts$species)
canoe_pass_counts$number <- as.numeric(canoe_pass_counts$number)

# Drop irrelevant colums & rearrange
canoe_pass_counts <- canoe_pass_counts[,3:6]
canoe_pass_counts <- dplyr::select(.data = canoe_pass_counts, date_time_pdt, dplyr::everything())

# Finish up
rm(canoe_pass_counts_list)
cleaned[[length(cleaned) + 1]] <- canoe_pass_counts
names(cleaned)[length(cleaned)] <- "canoe_pass_counts"
rm(canoe_pass_counts)

## 06 CLEAN 'strip_counts' ----
# Given this data only appears in 3 sheets, it will all
# be summarized into one somewhat unweildy table. If 
# strip counts become standard procedure in any future
# surveys, I recommend splitting into two tables: one 
# table with strip meta-information (date, shore length)
# and another table with the count data per strip.

# Overall cleaning procedure: 
# 1) pull out dates-strip table
# 2) pull out unique dates from header of dates-strip table
# 3) create 'date' column; populate with date data for each count
# 4) grep 'shore length' for each count; create 'shore_length_m'
#    col and populate with grep'd shore length for each count

strip_counts <- list()
for (i in 1:length(strip_counts_list)) {
  # First, pull out rows that contain "Strip", -2 to include 
  # date + count header rows, then + shore_length. 
  # Then rearrange to desired rows/cols.
  tmp <- strip_counts_list[[i]]
  if (names(strip_counts_list)[i] == "BPPEEP97") tmp[7,4] <- "Count 2"
  records <- append((min(grep("Strip|shore", tmp[[1]])) - 2):(min(grep("Strip|shore", tmp[[1]])) - 1), grep("Strip|shore", tmp[[1]]))
  tmp <- tmp[records,]
  tmp[1,1] <- "date"
  tmp[2,1] <- "count"
  names(tmp) <- paste(tmp[2,], tmp[1,])
  tmp <- tidyr::pivot_longer(tmp, cols = 2:length(tmp))
  tmp2 <- tidyr::pivot_wider(tmp, id_cols = "name", values_from = "value", names_from = "count date")
  tmp2 <- janitor::clean_names(tmp2)
  tmp2 <- tmp2 %>% dplyr::select(name, date, shore_length_m, dplyr::everything())
  tmp3 <- tidyr::pivot_longer(tmp2, cols = 5:length(tmp2), names_to = "strip")
  tmp4 <- tidyr::pivot_wider(tmp3, id_cols = c("date", "shore_length_m", "strip"), names_from = "count", values_from = "value")
  # Probably a way of doing this in fewer pivot steps... but it works
  strip_counts[[i]] <- tmp4
  rm(list = ls(pattern = "tmp"))
  rm(records)
}
rm(i)

# Now table-level cleans
strip_counts <- dplyr::bind_rows(strip_counts)

strip_counts <- janitor::clean_names(strip_counts)
strip_counts <- strip_counts[,1:5]
strip_counts$strip <- gsub(pattern = "strip_", replacement = "", x = strip_counts$strip)
strip_counts %<>% 
  dplyr::mutate_all(as.numeric)
strip_counts$date <- janitor::excel_numeric_to_date(strip_counts$date)

# Finish up
rm(strip_counts_list)
cleaned[[length(cleaned) + 1]] <- strip_counts
names(cleaned)[length(cleaned)] <- "strip_counts"
rm(strip_counts)

## 07 CLEAN 'counts' ----
# This is the big one.. compilation_*, estimated_*, summary_*
# tables all compile data from this table.
# Note header changes drastically from 2013 -> 2014.
# But otherwise, overall, the format is the same for all
# files - first few rows contain meta information (e.g., year
# and survey methodology notes), then first row w complete
# cases contains the header names.

# First pare down each df data-only rows + assign clean names.
for (i in 1:length(counts_list)) {
  tmp <- counts_list[[i]]
  # Remove any completely empty rows/columns (e.g. see '95 data)
  #tmp <- tmp[,!sapply(tmp, function(x) all(is.na(x) | x == ""))]
  tmp <- janitor::remove_empty(tmp, which = c("rows", "cols"), quiet = TRUE)
  # Remove columns where 'pilings' is only value (2017-2019 sheets
  # w rogue cell in the upper-right corner of 'counts' tab)
  tmp <- tmp[,!sapply(tmp, function(x) all(is.na(x) | x == "pilings"))]
  # Pull out the first full row - the first row w complete cases
  # is the header. Then clean up.
  cn <- tmp[complete.cases(tmp),]
  if (nrow(cn) > 1) cn <- cn[1,] # Select first row in case it also pulls out full data row
  cn <- as.character(cn)
  cn <- janitor::make_clean_names(cn)
  # Anything BEFORE the first complete row is the metadata for the sheet
  # The [1] is there, much like the if statement above, in case
  # it finds multiple rows w complete cases.
  # For now not actually doing anything with meta; after 
  # scanning everything it pulls out, no valuable information
  # is noted in 'meta' for any file.
  #meta <- tmp[1:(grep(TRUE, complete.cases(tmp))[1]-1),]
  # Extract data (anything AFTER first complete row)
  tmp <- tmp[-(1:grep(TRUE, complete.cases(tmp))[1]),]
  # Set header names
  names(tmp) <- cn
  # Replace old df with tmp
  counts_list[[i]] <- tmp
  message("Set names for ", names(counts_list)[i])
  rm(tmp)
  }

# Add filename column to each one
for (i in 1:length(counts_list)) {
  counts_list[[i]]$raw_datafile <- f[grep(names(counts_list)[i], f)]
}

# Now, similar to clean tab names process above, we will
# standardize all the header names across all the dfs.
counts_cn <- lapply(counts_list, names)

# Check out column names to get idea of cleaning procedures needed
sort(unique(unlist(counts_cn)))
plyr::count(unlist(counts_cn))

for (i in 1:length(counts_cn)) {
  cn <- counts_cn[[i]]
  # 'average_count' -> 'mean_count'
  cn[grepl("average", cn)] <- "mean_count"
  # 'meancount' -> 'mean_count'
  cn[grepl("meancount", cn)] <- "mean_count"
  # 'count' -> 'mean_count' (used from 2014-onwards)
  cn[grep("\\bcount\\b", cn)] <- "mean_count"
  # 'countX' -> 'count_X'
  cn[grep("count1", cn)] <- "count_1"
  cn[grep("count2", cn)] <- "count_2"
  cn[grep("count3", cn)] <- "count_3"
  # 'height' & 'height_ft' -> 'high_tide_height_ft"
  cn[grep("height", cn)] <- "high_tide_height_ft"
  # 'time_2' -> 'high_tide_time_pdt'
  cn[grep("time_2", cn)] <- "high_tide_time_pdt"
  # 'total' -> count_1
  cn[grep("total", cn)] <- "count_1"
  counts_cn[[i]] <- cn
  rm(cn)
}

# Assign clean header names to counts data
for (i in 1:length(counts_list)) {
  names(counts_list[[i]]) <- counts_cn[[i]]
}
rm(counts_cn)

# Finally, row-bind our counts_list into counts df!
# Rest of cleaning will occur on whole dataset.
counts <- dplyr::bind_rows(counts_list)

# Assign a record id to each record
counts$record_id <- as.numeric(row.names(counts))
counts <- counts %>% dplyr::select(record_id, dplyr::everything())

# This record ID is now used to merge a supporting dataset in
# This supporting dataset includes the column "in_daily_total_yn",
# which indicates whether or not the meancount for a given location
# was ultimately included in the daily TOTAL count for the day.
# I manually went through each day to determine if the location
# counts were included in the daily total. No neat way to do this
# programmatically.
# This column ultimately is used for error checking subtotals 
# below, and can also be used by the end user in deciding whether
# to include certain records or not.
in_daily_total_yn <- read.csv("supporting_files/in_total_yn.csv")
in_daily_total_yn <- in_daily_total_yn[,c("record_id", "in_daily_total_yn")]
counts <- merge(counts, in_daily_total_yn, all.x = TRUE)
rm(in_daily_total_yn)

# Merge 'notes' and 'comments' columns
counts$notes <- ifelse(is.na(counts$comments),
                       counts$notes,
                       paste0(counts$notes, "; ", counts$comments))

# Remove any '+' from bird count #s and add note
# (Two records)
counts[grepl("\\+", counts$count_1) |
         grepl("\\+", counts$count_2) |
         grepl("\\+", counts$count_3) |
         grepl("\\+", counts$count_4) |
         grepl("\\+", counts$count_5) |
         grepl("\\+", counts$mean_count) |
         grepl("\\+", counts$high_tide_height_ft) |
         grepl("\\+", counts$bb_plover),]

counts[grepl("\\+", counts$count_1),"notes"] <- paste0(counts[grepl("\\+", counts$count_1),"notes"], "; counts_1 estimate is a minimum")
counts[grepl("\\+", counts$bb_plover),"notes"] <- paste0(counts[grepl("\\+", counts$bb_plover),"notes"], "; bb_plover estimate is a minimum")

counts$count_1 <- gsub(pattern = "\\+", replacement = "", x = counts$count_1)
counts$bb_plover <- gsub(pattern = "\\+", replacement = "", x = counts$bb_plover)

# Fix two 'time' records - "> 15:47" and "~ 7:05"
# Save record #s to fully fix date later, and add hour & min
# manually to hour & minutes column
bad_times <- row.names(counts[which(counts$time %in% c("> 15:47", "~7:05")),])
counts[which(counts$time == "> 15:47"), c("hour", "minutes")] <- c("15", "47")
counts[which(counts$time == "> 15:47"), c("notes")] <- paste0(counts[which(counts$time == "> 15:47"), c("notes")], "; survey time originally '> 15:47'")
counts[which(counts$time == "~7:05"), c("hour", "minutes")] <- c("7", "05")
counts[which(counts$time == "~7:05"), c("notes")] <- paste0(counts[which(counts$time == "~7:05"), c("notes")], "; survey time originally '~7:05'")

# 'mean_count' "no survey" put in to 'notes'
counts[which(counts$mean_count == "no survey"), "notes"] <- "no survey"

# Move any descriptive 'bb_plover' values to 'other_birds'
counts[["other_birds"]][grepl("[^0-9.]", counts$bb_plover)] <- 
  ifelse(is.na(counts[["other_birds"]][grepl("[^0-9.]", counts$bb_plover)]),
       paste0(counts[["bb_plover"]][grepl("[^0-9.]", counts$bb_plover)], " BB plover"),
       paste0(counts[["other_birds"]][grepl("[^0-9.]", counts$bb_plover)], counts[["other_birds"]][grepl("[^0-9.]", counts$bb_plover)], " BB plover")
       )

# Coerce column types
# All necessary non-numeric values flagged by "NAs
# introduced by coersion" have been taken care of
# above 
counts %<>%
  dplyr::mutate_at(c("date",
                     "time",
                     "count_1",
                     "count_2",
                     "count_3",
                     "mean_count",
                     "high_tide_height_ft",
                     "high_tide_time_pdt",
                     "bb_plover",
                     "count_4",
                     "count_5",
                     "julian_date",
                     "day_survey",
                     "year",
                     "month",
                     "day",
                     "hour",
                     "minutes"),
                   as.numeric)

# Standardize tide
counts[["tide"]][grep("rising- |rising/|rising -|small", counts$tide)] <- "rising-high"
counts$tide <- as.factor(counts$tide)

# Extract precipitation and cloud cover from weather
counts$precipitation <- NA
counts[["precipitation"]][grep("drizzle|rain|shower|snow", tolower(counts$weather))] <- counts[["weather"]][grep("drizzle|rain|shower|snow", tolower(counts$weather))]
counts[["precipitation"]][grep("drizzle|light rain", tolower(counts$precipitation))] <- "Light rain"
counts[["precipitation"]][grep("(?=.*shower)(?!.*heavy)", tolower(counts$precipitation), perl = TRUE)] <- "Showers"
counts[["precipitation"]][grep("rain earlier", tolower(counts$precipitation))] <- NA # Going to assume all precipitation should be for present - user can look at 'weather' col later for detailed description
counts[["precipitation"]][grep("to", tolower(counts$precipitation))] <- "Rain" # Originally "showers to heavy rain" - just going to call that "Rain"
counts[["precipitation"]][grep("heavy rain", tolower(counts$precipitation))] <- "Heavy rain"
counts[["precipitation"]][grepl("rain", tolower(counts$precipitation)) & !grepl("light|heavy", tolower(counts$precipitation))] <- "Rain"
counts[["precipitation"]][grep("light snow", counts$precipitation)] <- "Light snow"
counts$precipitation <- as.factor(counts$precipitation)

counts$cloud_cover_percent <- NA
counts[["cloud_cover_percent"]][grep("%", counts$weather)] <- counts[["weather"]][grep("%", counts$weather)]
counts$cloud_cover_percent <- gsub(".*?([0-9]+).*", "\\1", counts$cloud_cover_percent)
counts$cloud_cover_percent <- as.numeric(counts$cloud_cover_percent)

# Standardize wind speed values
counts$wind_direction <- NA
counts$wind_direction <- stringr::str_extract(counts$wind, "\\b(N|S|E|W|NE|NW|SE|SW|NNE|NNW|ESE|ENE|SSE|SSW|WSW|WNW)\\b")
counts[["wind_direction"]][grep("north", tolower(counts$wind))] <- "N"
counts[["wind_direction"]][grep("south", tolower(counts$wind))] <- "S"
counts[["wind_direction"]][grep("east", tolower(counts$wind))] <- "E"
counts[["wind_direction"]][grep("west", tolower(counts$wind))] <- "W"
counts$wind_direction <- as.factor(counts$wind_direction)

counts$wind_speed_kn <- NA
wind_speed <- stringr::str_extract(counts$wind, "\\d+\\s*\\-\\s*\\d+|\\d+")
wind_speed <- gsub(" ", "", wind_speed)
wind_speed <- strsplit(wind_speed, "-")
wind_speed <- sapply(wind_speed, function (x) mean(as.numeric(x)))
counts$wind_speed_kn <- wind_speed
rm(wind_speed)

# Deal with dates
# 1999 data missing date but has time
bad_row <- as.numeric(row.names(counts[is.na(counts$date) & !is.na(counts$time),]))
correct_date <- counts[["date"]][bad_row+1]
counts[["date"]][bad_row] <- correct_date
rm(bad_row)
rm(correct_date)

# Build excel dates from 'date' column
counts$excel_datetime <- rowSums(counts[,c("date", "time")], na.rm = TRUE)
counts$excel_datetime <- janitor::convert_to_datetime(counts$excel_datetime, tz = "Canada/Pacific")

# Now fix date for bad "> 15:47" and "~7:05" records above
# i.e., add correct values to 'year' 'month' and 'day' cols
counts[["year"]][as.numeric(bad_times)] <- lubridate::year(counts[["excel_datetime"]][as.numeric(bad_times)])
counts[["month"]][as.numeric(bad_times)] <- lubridate::month(counts[["excel_datetime"]][as.numeric(bad_times)])
counts[["day"]][as.numeric(bad_times)] <- lubridate::day(counts[["excel_datetime"]][as.numeric(bad_times)])
rm(bad_times)

# Now build other dates from 'year' 'month' 'day' 'hour' 'minutes' cols
counts$other_datetime <- lubridate::make_datetime(counts$year, counts$month, counts$day, counts$hour, counts$minutes, tz = "Canada/Pacific")

# Now assign correct date to date_time_pdt
# Note must use dplyr::if_else as base ifelse strips timestamp attributes
counts$date_time_pdt <- dplyr::if_else(is.na(counts$other_datetime),
                               counts$excel_datetime,
                               counts$other_datetime)

# Remove any datetimes set to 1899-12-30
counts[["date_time_pdt"]][counts$date_time_pdt < "1900-01-01"] <- NA

# Fix a few records with year '2108' instead of '2018'
lubridate::year(counts[["date_time_pdt"]][counts$raw_datafile == "BPPeeps2018.xlsx"]) <- 2018

# Fix high tide time
counts$high_tide_time_pdt <- openxlsx::convertToDateTime(counts$high_tide_time_pdt, origin = as.Date(counts$date_time_pdt))
counts$high_tide_time_pdt <- lubridate::force_tz(counts$high_tide_time_pdt, tzone = "Canada/Pacific")

# Fill 'julian_date' column for all records
# I also checked if the existing values in 'julian_date'
# match what I calculate now (they do).
counts$julian_date <- lubridate::yday(counts$date_time_pdt)

# Standardize 'observer'
counts[["observer"]][which(counts$observer == "RB")] <- "R.B."
counts[["observer"]][which(counts$observer == "ML")] <- "M.L."
counts$observer <- as.factor(counts$observer)

# Data checks
# Some simple & quick data checks to see if excel data is 
# correct vs. calculations in R

## Row means
counts$calc_mean <- round(rowMeans(counts[,c("count_1", "count_2", "count_3", "count_4", "count_5")], na.rm = T))
# Now check - 148 records mismatched
#View(counts[which((round(counts$mean_count) != counts$calc_mean) | (!is.na(counts$calc_mean) & is.na(counts$mean_count))),c("raw_datafile", "location", "count_1", "count_2", "count_3", "count_4", "count_5", "mean_count", "calc_mean")])
# Of those, the vast majority are differences in rounding.
# Next subset records where difference is greater > 200.
counts$mean_diff <- counts$calc_mean - ifelse(is.na(counts$mean_count), 0, round(counts$mean_count))
#View(counts[which(counts$mean_count != counts$calc_mean & (counts$mean_diff > 200 | counts$mean_diff < -200)),c("raw_datafile", "location", "count_1", "count_2", "count_3", "count_4", "count_5", "mean_count", "calc_mean", "mean_diff")])
#View(counts[which(counts$mean_diff > 200 | counts$mean_diff < -200),c("raw_datafile", "location", "count_1", "count_2", "count_3", "count_4", "count_5", "mean_count", "calc_mean", "mean_diff")])
#This doesn't include NA mean_count: mean_errors <- counts[which(counts$mean_count != counts$calc_mean & (counts$mean_diff > 200 | counts$mean_diff < -200)),c("raw_datafile", "location", "count_1", "count_2", "count_3", "count_4", "count_5", "mean_count", "calc_mean", "mean_diff")]
mean_errors <- counts[which(counts$mean_diff > 200 | counts$mean_diff < -200),c("raw_datafile", "location", "count_1", "count_2", "count_3", "count_4", "count_5", "mean_count", "calc_mean", "mean_diff")]

# Fix 3 mean errors that I'm confident are simply Excel errors
counts[["mean_count"]][which(counts$raw_datafile == "BPPeep2007.xls" & counts$mean_count == 145000)] <- 154000
counts[["mean_count"]][which(counts$raw_datafile == "BPPeeps2014MD.xlsx" & counts$mean_count == 1000 & counts$location == "pilings bend")] <- 500
counts[["mean_count"]][which(counts$raw_datafile == "BPPeeps2014MD.xlsx" & counts$mean_count == 700 & counts$location == "34th street pullout")] <- 4200

# Save errors
errors <- list()
errors[[1]] <- mean_errors
names(errors)[1] <- "counts_mean_errors"
rm(mean_errors)

## Column means
# Extract any 'TOTAL' rows - i.e. subtotal rows within dataset
subtotals <- counts[counts$in_daily_total_yn == "total",] # previously grep("tot", tolower(counts$location))
subtotals$date <-  as.Date(subtotals$date_time_pdt, format = "%Y-%m-%d", tz = "Canada/Pacific")

# Compare if manually calc'd col means == R calc'd col means
# Group records by date, then take the mean of calc_mean by date,
# then compare to matching row mean in 'subtotals' df
# Now using the manually labeled "in_daily_total_yn" col
day_means <- counts[counts$in_daily_total_yn == "TRUE",] %>% # previously !grepl("tot", tolower(counts$location))
  dplyr::mutate(date = as.Date(date_time_pdt, format = "%Y-%m-%d", tz = "Canada/Pacific")) %>%
  dplyr::group_by(date) %>%
  dplyr::summarize(calc_tot = sum(round(calc_mean), na.rm = TRUE)) %>%
  merge(subtotals, by = "date") %>%
  dplyr::mutate(tot_diff = calc_tot - mean_count) %>%
  dplyr::select(date, raw_datafile, location, mean_count, calc_tot, tot_diff, count_1, count_2, count_3, count_4, count_5,  calc_mean)

# In some cases there's multiple totals per day (i.e. 
# sub-sub-totals AND sub-totals). Flag any records where
# BOTH sub-sub-total and sub-total doesn't line up, 
# group sub-sub-totals by date, and retry above matching.
subtotal_dates <- day_means %>% 
  dplyr::group_by(date) %>% 
  dplyr::filter(!any(tot_diff == 0)) %>%
  dplyr::select(date) %>%
  dplyr::pull()

subtotals %>%
  dplyr::filter(date %in% subtotal_dates) %>%
  dplyr::group_by(date) %>%
  dplyr::summarize(subtot_tot = sum(round(mean_count), na.rm = TRUE)) %>%
  merge(day_means, by = "date") %>%
  dplyr::mutate(tot_diff = subtot_tot - calc_tot) %>%
  dplyr::filter(abs(tot_diff) > 1000) %>% # Often numbers were manually rounded to nearest thousandth
  dplyr::select(date, raw_datafile, location, subtot_tot, calc_tot, tot_diff, count_1, count_2, count_3, count_4, count_5,  calc_mean) %>%
  View()

rm(day_means)
rm(subtotals)
rm(subtotal_dates)

# 2022-08-07: have sent flagged records to data owners to 
# hear feedback. For now, counts cleaning is done. 
# 'location' standardization will be done with Google
# OpenRefine.
# 2022-08-18: turns out in the 'notes' column there is an
# indication of when the "sweep" started. Some counts are
# just test counts prior to actually doing the proper "sweep"
# count data. As such the data author manually picked and chose
# which data made it into 'MeanCount' base on this and other
# criteria. It will be difficult to automate pulling it out.
# 2022-11-14: I've gone through and manually flagged records
# that should be included (or not) in the daily total; this
# now appears in the supporting_files/in_total_yn.csv doc.
# 2022-11-14: much more manageable number of accounting errors; 
# I have gone through them all and all appear ok and primarily
# flagged either as rounding differences or in cases where
# one day had a ridiculous number of TOTALS for the day (e.g.
# 2011-04-30).

# Rename 'mean_count' to 'final_count' to better reflect
# column value meaning
names(counts)[grep("mean_count", names(counts))] <- "final_count"

# Rearrange and remove any superfluous columns
# Dropping columns if:
# - they were merged with another column above (e.g., 'comments')
# - they have no values (e.g., 'count_id' is only NAs; it was used for excel VLOOKUP purposes)
# - they have only one value (e.g., 'dunlin' has one value in one cell, which was moved to 'other_birds')
# - they have only one unique value (e.g., all records in 'day_survey' simply say '1')
# - they were merged into the date_time_pdt column
# - they were temporary columns used in error checking above (e.g., 'calc_mean' and 'mean_diff')
counts <- counts %>% 
  dplyr::select(record_id,
                date_time_pdt, 
                tide, 
                location, 
                count_1, 
                count_2, 
                count_3, 
                count_4, 
                count_5, 
                final_count,
                in_daily_total_yn,
                notes, 
                high_tide_height_ft, 
                high_tide_time_pdt, 
                weather,
                cloud_cover_percent,
                precipitation,
                wind, 
                wind_direction,
                wind_speed_kn,
                observer, 
                other_birds, 
                julian_date,
                raw_datafile)

# Previously, was extracting all non-TOTAL rows. Now just
# putting it all in the database; end-user can use in_daily_total_yn
# col to filter as needed

# Finish up
rm(counts_list)
cleaned[[length(cleaned) + 1]] <- counts
names(cleaned)[length(cleaned)] <- "counts"
rm(counts)

# Could probably merge 'boundary_bay_counts', 
# 'canoe_pass_counts', 'strip_counts' and 'counts',
# but need to discuss idea first.

## 08 CLEAN 'species_ratios' ----
# This is the second big table - it is used in conjunction
# with the 'counts' table to produce yearly population estimates
# of different peep species at the mudflats.

# Same process to clean these tables as for 'counts', really.
# First pare down each df data-only rows + assign clean names.
for (i in 1:length(species_ratios_list)) {
  tmp <- species_ratios_list[[i]]
  # Remove any completely empty rows/columns
  tmp <- janitor::remove_empty(tmp, which = c("rows", "cols"), quiet = TRUE)
  # Pull out the first full row - the first row w complete cases
  # is the header. Then clean up.
  srn <- tmp[complete.cases(tmp),]
  if (nrow(srn) > 1) srn <- srn[1,] # Select first row in case it also pulls out full data row
  srn <- as.character(srn)
  srn <- janitor::make_clean_names(srn)
  # Anything BEFORE the first complete row is the metadata for the sheet
  # The [1] is there, much like the if statement above, in case
  # it finds multiple rows w complete cases.
  # For now not actually doing anything with meta; after 
  # scanning everything it pulls out, no valuable information
  # is noted in 'meta' for any file.
  #meta <- tmp[1:(grep(TRUE, complete.cases(tmp))[1]-1),]
  # Extract data (anything AFTER first complete row)
  tmp <- tmp[-(1:grep(TRUE, complete.cases(tmp))[1]),]
  # Set header names
  names(tmp) <- srn
  # Add filename column
  tmp$raw_datafile <- f[grep(names(species_ratios_list)[i], f)]
  # Replace old df with tmp
  species_ratios_list[[i]] <- tmp
  message("Set names for ", names(species_ratios_list)[i])
  rm(tmp)
}

# Now we will standardize all the header names across 
# all the dfs.
srn <- lapply(species_ratios_list, names)

# Check out column names to get idea of cleaning procedures needed
sort(unique(unlist(srn)))
plyr::count(unlist(srn))
rm(srn)

# Astoundingly, column names don't appear to need any cleanup
# Now bind rows
species_ratios <- dplyr::bind_rows(species_ratios_list)

# Coerce column types
# First make note of one record w "<10" as %age
species_ratios[["notes"]][grep("<10", species_ratios$percent_wesa)] <- paste0(species_ratios[["notes"]][grep("<10", species_ratios$percent_wesa)], "; %WESA estimated to be '<10' in raw data")
species_ratios[["percent_wesa"]][grep("<10", species_ratios$percent_wesa)] <- 9

# Pull out records with character/descriptive dates (2011-2012 data)
# Referring to them as "interpolated species ratios"
int_spr <- species_ratios[grep("may|apr", tolower(species_ratios$date)),]
# There is likely a more clever way to do this, but manual fixes
# are faster for now...
# Single dates
int_spr_s <- int_spr[!grepl("&|-", int_spr$date),]
int_spr_s$date <- c("2011-04-26", "2011-05-08", "2012-04-20", "2012-05-02")
# Double dates
int_spr_d <- int_spr[grep("&", int_spr$date),]
int_spr_d <- rbind(int_spr_d, int_spr_d)
int_spr_d$date <- c("2011-04-17", "2011-04-21", "2012-04-16", "2011-04-18", "2011-04-22", "2012-04-17")
# Multiple dates (this is ugly ugly code, please forgive me DRY code overlords + future self)
int_spr_m <- int_spr[grep("-", int_spr$date),]
int_spr_m1 <- rbind(int_spr_m[1,], int_spr_m[1,], int_spr_m[1,], int_spr_m[1,], int_spr_m[1,])
int_spr_m1$date <- seq(as.Date("2011-04-28"), as.Date("2011-05-02"), by = "1 day")
int_spr_m2 <- rbind(int_spr_m[2,], int_spr_m[2,], int_spr_m[2,])
int_spr_m2$date <- seq(as.Date("2012-05-04"), as.Date("2012-05-06"), by = "1 day")
int_spr_m3 <- rbind(int_spr_m[3,], int_spr_m[3,])
int_spr_m3$date <- c("2012-05-09", "2012-05-10")

int_spr_m <- rbind(int_spr_m1, int_spr_m2, int_spr_m3)
int_spr_m$date <- as.character(int_spr_m$date)
rm(int_spr_m1)
rm(int_spr_m2)
rm(int_spr_m3)

int_spr <- rbind(int_spr_s, int_spr_d, int_spr_m)
rm(int_spr_s)
rm(int_spr_d)
rm(int_spr_m)

# Convert the dates back to excel numeric so they can cleanly
# fit into date conversion pipeline below
d0 <- as.numeric(as.Date(0, origin="1899-12-30", tz='UTC'))
int_spr$date <- as.numeric(as.Date(int_spr$date)) - d0
rm(d0)

# Bind 'interpolated' data back in
# First remove the int rows
species_ratios <- species_ratios[!grepl("may|apr", tolower(species_ratios$date)),]
# Bind together
species_ratios <- rbind(species_ratios, int_spr)
rm(int_spr)

# Note this will result in NAs introdued by coersion in
# the 'date' column for descriptive/interpolated dates -
# e.g. '21 & 22 April' (interpolated %ages between 20 and 23 April)
# 'time' for 2021 data is also character, but will be rebuilt
# using the 'hour' and 'minutes' columns.
species_ratios %<>%
  dplyr::mutate_at(c("date", 
                     "time", 
                     "sample", 
                     "wesa", 
                     "dunl", 
                     "total", 
                     "percent_wesa",
                     "bbpl",
                     "percent_dunl",
                     "day_survey",
                     "year",
                     "month",
                     "day",
                     "hour",
                     "minutes"),
                   as.numeric)

# Deal with dates

# First fix records that have year, month, day, but NA time
# These will result in "1899-12-30" datetime if not fixed
species_ratios[["hour"]][!is.na(species_ratios$year) & !is.na(species_ratios$month) & !is.na(species_ratios$day) & is.na(species_ratios$hour)] <- 0
species_ratios[["minutes"]][!is.na(species_ratios$year) & !is.na(species_ratios$month) & !is.na(species_ratios$day) & is.na(species_ratios$minutes)] <- 0

# Build excel dates from 'date' column
species_ratios$excel_datetime <- rowSums(species_ratios[,c("date", "time")], na.rm = TRUE)
species_ratios$excel_datetime <- janitor::convert_to_datetime(species_ratios$excel_datetime, tz = "Canada/Pacific")

# Now build other dates from 'year' 'month' 'day' 'hour' 'minutes' cols
species_ratios$other_datetime <- lubridate::make_datetime(species_ratios$year, species_ratios$month, species_ratios$day, species_ratios$hour, species_ratios$minutes, tz = "Canada/Pacific")

# Now assign correct date to date_time_pdt
# Note must use dplyr::if_else as base ifelse strips timestamp attributes
species_ratios$date_time_pdt <- dplyr::if_else(is.na(species_ratios$other_datetime),
                                               species_ratios$excel_datetime,
                                               species_ratios$other_datetime)

# Data error checks
# 1) Recorded %WESA matches calculated %WESA
# 2) Recorded total matches calculated total
species_ratios$calc_total <- rowSums(species_ratios[,c("wesa", "dunl", "bbpl")], na.rm = T) 
species_ratios$calc_p_wesa <- (species_ratios$wesa / species_ratios$calc_total) * 100
species_ratios$p_diff <- species_ratios$calc_p_wesa - species_ratios$percent_wesa

totals_errors <- species_ratios[which(species_ratios$total != species_ratios$calc_total),c("raw_datafile", "date_time_pdt", "wesa", "dunl", "bbpl", "total", "calc_total", "notes")]
p_wesa_errors <- species_ratios[which(species_ratios$percent_wesa != species_ratios$calc_p_wesa & abs(species_ratios$p_diff) > 2),c("raw_datafile", "date_time_pdt", "wesa", "dunl", "bbpl", "total", "calc_total", "percent_wesa", "calc_p_wesa", "notes")]

# Add to errors list
errors[[length(errors) + 1]] <- totals_errors
names(errors)[length(errors)] <- "sr_totals_errors"
rm(totals_errors)
errors[[length(errors) + 1]] <- p_wesa_errors
names(errors)[length(errors)] <- "sr_pcnt_wesa_errors"
rm(p_wesa_errors)

# One %WESA error due to mistake in excel formula. Remaining
# %WESA errors are due to changes in 'total' value if you 
# include the BBPL numbers in the total.
# Fix one typo found in p_wesa_errors above, 2003-04-30 17:00:00
species_ratios[["percent_wesa"]][species_ratios$date_time_pdt == "2003-04-30 17:00:00" & species_ratios$sample == 4] <- species_ratios[["calc_p_wesa"]][species_ratios$date_time_pdt == "2003-04-30 17:00:00" & species_ratios$sample == 4]

# Fill in NA WESA/DUNL pop numbers where no pop # but DOES
# have a %WESA value. e.g. see: 
#View(species_ratios[is.na(species_ratios$wesa) & is.na(species_ratios$dunl),])
r <- row.names(species_ratios[is.na(species_ratios$wesa) & is.na(species_ratios$dunl),])
species_ratios[["wesa"]][is.na(species_ratios$wesa) & is.na(species_ratios$dunl)] <- species_ratios[["percent_wesa"]][is.na(species_ratios$wesa) & is.na(species_ratios$dunl)]
species_ratios[r, "dunl"] <- 100 - species_ratios[r, "percent_wesa"]
rm(r)

# Exclude subtotal rows
species_ratios <- species_ratios[!grepl("total|use|average", tolower(species_ratios$location)),]

# Select final columns
# Drop % dunl column - only one value out of 6644 records, and
# it can be inferred from other columns anyways
species_ratios <- species_ratios %>%
  dplyr::select(date_time_pdt,
                location,
                sample,
                wesa,
                dunl,
                bbpl,
                #total, # To be calculated on-the-fly in views
                #percent_wesa, # To be calculated on-the-fly in views
                notes,
                raw_datafile)

# Finish up
rm(species_ratios_list)
cleaned[[length(cleaned) + 1]] <- species_ratios
names(cleaned)[length(cleaned)] <- "species_ratios"
rm(species_ratios)

# 09 CLEAN 'raptors' ----
# Raptor data starts in 1997. From 1997-2014, header row is
# weirdly split up across two rows. That needs to be accounted
# for in any R script. 

# Instead of pulling first row with complete cases (as has 
# been the strategy in other tables), pull first row w
# >3 cells filled in.
raptors <- list()
for (i in 1:length(raptors_list)) {
  tmp <- raptors_list[[i]]
  header <- row.names(tmp[rowSums(!is.na(tmp)) > 3, ][1,])
  tmp <- tmp[header:nrow(tmp),]
  if (is.na(tmp[1,1])) {
    tmp[1,] <- paste(tmp[1,], tmp[2,])
    tmp[1,] <- gsub("NA", "", tmp[1,])
    tmp[1,] <- gsub("- ", "", tmp[1,])
    names(tmp) <- janitor::make_clean_names(tmp[1,])
    tmp <- tmp[3:nrow(tmp),]
  } else {
    names(tmp) <- janitor::make_clean_names(tmp[1,])
    if (nrow(tmp) > 1) tmp <- tmp[2:nrow(tmp),]
  }
  # Skip wonky 2015-2017 data until fixes are sent
  if (any(grepl("na", names(tmp))) | nrow(tmp) < 2) next
  # Add filename column
  tmp$raw_datafile <- f[grep(names(raptors_list)[i], f)]
  message("Set names for ", names(raptors_list)[i])
  raptors[[i]] <- tmp
  rm(header)
  rm(tmp)
}

# Bind rows and begin table-level cleaning
raptors <- dplyr::bind_rows(raptors)

# Set 'observers' to 'observer', to line up with col-name
# convention in other tables
names(raptors)[grep("observers", names(raptors))] <- "observer"

# Standardize spp names
raptors[["notes"]][grep("falcon", raptors$species)] <- ifelse(is.na(raptors[["notes"]][grep("unid falcon|falcon", raptors$species)]),
                                                                          "Unidentified falcon",
                                                                          paste0(raptors[["notes"]][grep("unid falcon|falcon", raptors$species)], "; Unidentified falcon"))
raptors[["species"]][grep("falcon", raptors$species)] <- "UNK"
raptors[["species"]][which(raptors$species == "Kestrel")] <- "AMKE"
raptors[["species"]][which(raptors$species == "NA")] <- NA

raptors[["notes"]][grep("\\?", raptors$species)] <- paste0(raptors[["notes"]][grep("\\?", raptors$species)], "; species ID uncertain")
raptors[["species"]][grep("\\?", raptors$species)] <- gsub("\\?", "", raptors[["species"]][grep("\\?", raptors$species)])

# Add numeric column indicating how many raptors were seen in a
# particular observation (if noted)
raptors$count <- NA
raptors[["count"]][grep("(3)", raptors$species)] <- 3
raptors[["species"]][grep("(3)", raptors$species)] <- "PEFA"

# Remove whitespace
raptors$species <- stringr::str_squish(raptors$species)

# Now names have been standardized, set column type
raptors$species <- as.factor(raptors$species)

# Finish pulling out number of raptors for count col
# 2 raptors
raptors[["count"]][grep("2 perched|2 pefa|2 merl|2 fly|2 attack |2 baea|2 birds|2nd|two", tolower(raptors$observations))] <- 2
raptors[["count"]][grep("2 perched|2 pefa|2 merl|2 fly|2 attack |2 baea|2 birds|2nd", tolower(raptors$notes))] <- 2

# 3 raptors
raptors[["count"]][grep("3 perched|3 pefa|3 merl|3 fly|3 attack |3 baea|3 birds|3rd|three", tolower(raptors$observations))] <- 3
raptors[["count"]][grep("3 perched|3 pefa|3 merl|3 fly|3 attack |3 baea|3 birds|3rd", tolower(raptors$notes))] <- 3

# Pull out record that has "2 PEFA & 1 BAEA" - split into two 
# records, one record w count = 2 and species = PEFA; 
# another record with count = 1 and species = BAEA.
raptors[nrow(raptors) + 1,] <- raptors[grep("2 PEFA & 1 BAEA", raptors$observations),]
raptors[grep("2 PEFA & 1 BAEA", raptors$observations),"species"][2] <- "BAEA"
raptors[grep("2 PEFA & 1 BAEA", raptors$observations),"count"][2] <- 1

# Same as above - pull out record that has "joined by two BAEA"
# and add secord record for 2 BAEA.
raptors[nrow(raptors) + 1,] <- raptors[grep("two BAEA", raptors$observations),]
raptors[grep("two BAEA", raptors$observations),"species"][2] <- "BAEA"
raptors[grep("two BAEA", raptors$observations),"count"][2] <- 2

# Fix species where notes says "2 BAEA"
raptors[["species"]][grep("2 BAEA", raptors$notes)] <- "BAEA"

# 1 raptor - assume all records w species !NA but no other numbers
# specified = 1 raptor
raptors[["count"]][!is.na(raptors$species) & is.na(raptors$count)] <- 1

# Standardized 'age'
raptors[["age"]][grep("unk", tolower(raptors$age))] <- "UNK"
raptors[["age"]][grep("ad", tolower(raptors$age))] <- "Adult"
raptors[["age"]][grep("juv", tolower(raptors$age))] <- "Juvenile"
raptors$age <- as.factor(raptors$age)

# Standardize 'success'
raptors[["notes"]][grep("unk.but peep hit water", raptors$success)] <- "Unknown if attack successful but peep hit water"
raptors[["success"]][grep("unk", tolower(raptors$success))] <- "Unknown"
raptors[["success"]][grep("no |none|unsuccessful|didn't", tolower(raptors$success))] <- "No"
raptors[["success"]][grep("caught|kill|eat|capture|talons", tolower(raptors$success))] <- "Yes"
raptors[["success"]][grep("NA", raptors$success)] <- NA
raptors[["success"]][!is.na(raptors$success) & !(raptors$success %in% c("Unknown", "Yes", "No"))] <- "Unknown"
raptors$success <- as.factor(raptors$success)

# Merge 'observations' and 'notes'
raptors$notes <- apply(raptors[,c("observations", "notes")], 1, function(x) paste(x[!is.na(x)], collapse = "; "))

# Deal with dates
raptors %<>% 
  dplyr::mutate_at(c("date",
              "begin_obs",
              "end_obs",
              "time",
              "year",
              "month",
              "day",
              "hour",
              "minutes"),
            as.numeric)

raptors$excel_datetime <- ifelse(is.na(raptors$time),
                                 rowSums(raptors[,c("date", "begin_obs")], na.rm = T),
                                 rowSums(raptors[,c("date", "time")], na.rm = T))
raptors$excel_datetime[raptors$excel_datetime == 0] <- NA
raptors$excel_datetime <- janitor::convert_to_datetime(raptors$excel_datetime, tz = "Canada/Pacific")

raptors$other_datetime <- lubridate::make_datetime(raptors$year, raptors$month, raptors$day, raptors$hour, raptors$minutes, tz = "Canada/Pacific")

raptors$date_time_pdt <- dplyr::if_else(is.na(raptors$other_datetime),
                                        raptors$excel_datetime,
                                        raptors$other_datetime)

# Clean up 'begin_obs' and 'end_obs' columns
raptors$begin_obs <- openxlsx::convertToDateTime(raptors$begin_obs, origin = as.Date(raptors$date_time_pdt))
raptors$begin_obs <- lubridate::force_tz(raptors$begin_obs, tzone = "Canada/Pacific")

raptors$end_obs <- openxlsx::convertToDateTime(raptors$end_obs, origin = as.Date(raptors$date_time_pdt))
raptors$end_obs <- lubridate::force_tz(raptors$end_obs, tzone = "Canada/Pacific")

# Select final columns
raptors <- raptors %>%
  dplyr::select(date_time_pdt,
                begin_obs,
                end_obs,
                time,
                species,
                count,
                age,
                success,
                notes,
                observer,
                raw_datafile)

# Merge with raptor fixes for 2015-2017
raptors_fix <- dplyr::bind_rows(raptors_oct2020_list)
rm(raptors_oct2020_list)
names(raptors_fix) <- janitor::make_clean_names(raptors_fix[1,])
raptors_fix <- raptors_fix[2:nrow(raptors_fix),]

raptors_fix$date <- as.Date(raptors_fix$date, format = "%d/%m/%Y")

raptors_fix %<>%
  dplyr::mutate_at(c("pefa", 
                     "merl"),
                   as.numeric)

raptors_fix <- raptors_fix[raptors_fix$date > "2015-01-01" & raptors_fix$date < "2019-01-01",]
raptors_fix <- raptors_fix[,c("date", "pefa", "merl")]

raptors_fix <- tidyr::pivot_longer(raptors_fix, cols = c("pefa", "merl"), names_to = "species", values_to = "count")

raptors_fix <- raptors_fix[raptors_fix$count != 0,]
raptors_fix$species <- toupper(raptors_fix$species)

names(raptors_fix)[1] <- "date_time_pdt"

raptors_fix$raw_datafile <- "raptorsOct2020.xlsx"
raptors_fix$notes <- "Data from Oct 2020 file rather than field books"

raptors <- dplyr::bind_rows(raptors, raptors_fix)
rm(raptors_fix)

# Finish up
rm(raptors_list)
cleaned[[length(cleaned) + 1]] <- raptors
names(cleaned)[length(cleaned)] <- "raptors"
rm(raptors)

# 10 CLEAN 'daily_conditions' ----
# These tables were used in 2014-onwards in lieu of 
# noting daily conditions in the counts data.

# Same process to clean these tables as above.
# First pare down each df data-only rows + assign clean names.
for (i in 1:length(daily_conditions_list)) {
  tmp <- daily_conditions_list[[i]]
  # Remove any completely empty rows/columns
  tmp <- janitor::remove_empty(tmp, which = c("rows", "cols"), quiet = TRUE)
  # Pull out the first full row - the first row w complete cases
  # is the header. Then clean up.
  cn <- tmp[complete.cases(tmp),]
  if (nrow(cn) > 1) cn <- cn[1,] # Select first row in case it also pulls out full data row
  cn <- as.character(cn)
  cn <- janitor::make_clean_names(cn)
  # Anything BEFORE the first complete row is the metadata for the sheet
  # The [1] is there, much like the if statement above, in case
  # it finds multiple rows w complete cases.
  # For now not actually doing anything with meta; after 
  # scanning everything it pulls out, no valuable information
  # is noted in 'meta' for any file.
  #meta <- tmp[1:(grep(TRUE, complete.cases(tmp))[1]-1),]
  # Extract data (anything AFTER first complete row)
  tmp <- tmp[-(1:grep(TRUE, complete.cases(tmp))[1]),]
  # Set header names
  names(tmp) <- cn
  # Add filename column
  tmp$raw_datafile <- f[grep(names(daily_conditions_list)[i], f)]
  # Replace old df with tmp
  daily_conditions_list[[i]] <- tmp
  message("Set names for ", names(daily_conditions_list)[i])
  rm(tmp)
}

# Now we will standardize all the header names across 
# all the dfs.
dcn <- lapply(daily_conditions_list, names)

# Check out column names to get idea of cleaning procedures needed
sort(unique(unlist(dcn)))
plyr::count(unlist(dcn))

for (i in 1:length(dcn)) {
  cn <- dcn[[i]]
  # giant 'approx_time_of_11_5_ft_tide_or_best_survey' -> 'best_survey_time'
  cn[grep("approx", cn)] <- "best_survey_time"
  # 'comments' -> 'tide' (all comments in line with 'falling', 'rising', etc)
  cn[grep("comments", cn)] <- "tide"
  # 'tide_direction' -> 'tide', to follow col name convention in other tables
  cn[grep("tide_direction", cn)] <- "tide"
  # 'high_tide_time' -> 'high_tide_time_pdt'
  cn[grep("high_tide_time", cn)] <- "high_tide_time_pdt"
  # 'ppn' -> 'precipitation'
  cn[grep("ppn", cn)] <- "precipitation"
  # 'temp_degrees' -> 'degrees_c'
  cn[grep("temp", cn)] <- "degrees_c"
  # '*_km_h' -> '*_kmh'
  cn[grep("wind_speed_km_h", cn)] <- "wind_speed_kmh"
  # Various Mark Lieu cols that will be deleted later
  # 'mark_lieu_hours_hrs' -> 'lieu_hrs'
  cn[grep("lieu", cn)] <- "lieu_hours"
  # 'extra_hours_worked' -> 'hours_worked'
  cn[grep("extra", cn)] <- "hours_worked"
  dcn[[i]] <- cn
  rm(cn)
}

# Assign clean header names to counts data
for (i in 1:length(daily_conditions_list)) {
  names(daily_conditions_list[[i]]) <- dcn[[i]]
}
rm(dcn)

# Bind rows and begin table-level cleaning
daily_conditions <- dplyr::bind_rows(daily_conditions_list)

# Drop NA date records
daily_conditions <- daily_conditions[!is.na(daily_conditions$date),]

# Drop data from 2022 file (it's just example data)
daily_conditions <- daily_conditions[daily_conditions$raw_datafile != "RobertsBankShorebirdSurveys2022.xlsx",]

# Standardize weekday
daily_conditions[["weekday"]][grep("Thursday", daily_conditions$weekday)] <- "Thu"
daily_conditions[["weekday"]][grep("Fri ", daily_conditions$weekday)] <- "Fri"
daily_conditions[["weekday"]][grep("Mon ", daily_conditions$weekday)] <- "Mon"
daily_conditions$weekday <- as.factor(daily_conditions$weekday)

# Standardize tide (matching precidents in 'counts')
daily_conditions[["tide"]][grep("peak|rising/falling", tolower(daily_conditions$tide))] <- "high"
daily_conditions[["tide"]][grep("falling", tolower(daily_conditions$tide))] <- "dropping"
daily_conditions[["tide"]][grep("rising", tolower(daily_conditions$tide))] <- "rising"
daily_conditions[["notes"]][grep("had to leave early", tolower(daily_conditions$tide))] <- "Had to leave early"
daily_conditions[["tide"]][grep("had to leave early", tolower(daily_conditions$tide))] <- NA
daily_conditions$tide <- as.factor(daily_conditions$tide)

# Standardize precipitation
daily_conditions[["precipitation"]][grep("0", tolower(daily_conditions$precipitation))] <- "None"
daily_conditions[["precipitation"]][grep("^rain\\b", tolower(daily_conditions$precipitation))] <- "Rain"
daily_conditions[["precipitation"]][grep("light rain", tolower(daily_conditions$precipitation))] <- "Light rain"
daily_conditions[["precipitation"]][grep("showers", tolower(daily_conditions$precipitation))] <- "Showers"
daily_conditions$precipitation <- as.factor(daily_conditions$precipitation)

# Merge 'notes' and 'notes2'
daily_conditions$notes <- apply(daily_conditions[,c("notes", "notes2")], 1, function(x) paste(x[!is.na(x)], collapse = "; "))

# Coerce numeric column types
daily_conditions %<>% 
  dplyr::mutate_at(c("date",
                      "high_tide_time_pdt",
                      "high_tide_height_ft",
                      "best_survey_time",
                      "estimated_survey_start_time",
                      "estimated_survey_end_time",
                      "degrees_c",
                      "cloud_cover_percent",
                      "wind_speed_kmh",
                      "sweep_start",
                      "sweep_end",
                      "survey_start",
                      "survey_end",
                      "year",
                      "month",
                      "day",
                      "high_tide_height_m",
                      "survey_start_hour",
                      "survey_start_minute",
                      "survey_end_hour",
                      "survey_end_minute"),
                    as.numeric)

# Drop some irrelevant columns + reorder - makes working with
# remaining data easier
daily_conditions <- daily_conditions %>% dplyr::select(weekday,
                                                       date,
                                                       high_tide_time_pdt,
                                                       high_tide_height_ft,
                                                       high_tide_height_m,
                                                       best_survey_time,
                                                       tide,
                                                       degrees_c,
                                                       cloud_cover_percent,
                                                       precipitation,
                                                       wind_direction,
                                                       wind_speed_kmh,
                                                       sweep_start,
                                                       sweep_end,
                                                       survey_start,
                                                       survey_end,
                                                       observer,
                                                       notes,
                                                       raw_datafile,
                                                       year,
                                                       month,
                                                       day,
                                                       survey_start_hour,
                                                       survey_start_minute,
                                                       survey_end_hour,
                                                       survey_end_minute)

# Deal with dates
# Not the most concise but it works..
daily_conditions$high_tide_time_pdt <- daily_conditions$date + daily_conditions$high_tide_time_pdt
daily_conditions$best_survey_time <- daily_conditions$date + daily_conditions$best_survey_time
daily_conditions$sweep_start <- daily_conditions$date + daily_conditions$sweep_start
daily_conditions$sweep_end <- daily_conditions$date + daily_conditions$sweep_end
daily_conditions$survey_start <- daily_conditions$date + daily_conditions$survey_start
daily_conditions$survey_end <- daily_conditions$date + daily_conditions$survey_end

daily_conditions$date <- janitor::excel_numeric_to_date(daily_conditions$date)
daily_conditions$high_tide_time_pdt <- janitor::excel_numeric_to_date(daily_conditions$high_tide_time_pdt, include_time = T, tz = "Canada/Pacific")
daily_conditions$best_survey_time <- janitor::excel_numeric_to_date(daily_conditions$best_survey_time,include_time = T,  tz = "Canada/Pacific")
daily_conditions$sweep_start <- janitor::excel_numeric_to_date(daily_conditions$sweep_start,include_time = T,  tz = "Canada/Pacific")
daily_conditions$sweep_end <- janitor::excel_numeric_to_date(daily_conditions$sweep_end, include_time = T,  tz = "Canada/Pacific")
daily_conditions$survey_start <- janitor::excel_numeric_to_date(daily_conditions$survey_start, include_time = T,  tz = "Canada/Pacific")
daily_conditions$survey_end <- janitor::excel_numeric_to_date(daily_conditions$survey_end, include_time = T, tz = "Canada/Pacific")

# 2015 data all has wrong year
lubridate::year(daily_conditions[["date"]][daily_conditions$raw_datafile == "BPPeeps2015.xlsx"]) <- 2015

# Other date format
daily_conditions$other_survey_start <- lubridate::make_datetime(year = daily_conditions$year, month = daily_conditions$month, day = daily_conditions$day, hour = daily_conditions$survey_start_hour, min = daily_conditions$survey_start_minute, tz = "Canada/Pacific")
daily_conditions$other_survey_end <- lubridate::make_datetime(year = daily_conditions$year, month = daily_conditions$month, day = daily_conditions$day, hour = daily_conditions$survey_end_hour, min = daily_conditions$survey_end_minute, tz = "Canada/Pacific")

daily_conditions$survey_start_pdt <- dplyr::if_else(is.na(daily_conditions$other_survey_start),
                                                    daily_conditions$survey_start,
                                                    daily_conditions$other_survey_start)
daily_conditions$survey_end_pdt <- dplyr::if_else(is.na(daily_conditions$other_survey_end),
                                                    daily_conditions$survey_end,
                                                    daily_conditions$other_survey_end)

# Select final columns
daily_conditions <- daily_conditions %>% dplyr::select(date,
                                                       sweep_start,
                                                       sweep_end,
                                                       survey_start_pdt,
                                                       survey_end_pdt,
                                                       best_survey_time,
                                                       high_tide_time_pdt,
                                                       high_tide_height_ft,
                                                       high_tide_height_m,
                                                       tide,
                                                       degrees_c,
                                                       cloud_cover_percent,
                                                       precipitation,
                                                       wind_direction,
                                                       wind_speed_kmh,
                                                       weekday,
                                                       observer,
                                                       notes,
                                                       raw_datafile
                                                       )

names(daily_conditions)[2:3] <- c("sweep_start_pdt", "sweep_end_pdt")
names(daily_conditions)[6] <- "approx_11_5_ft_tide_or_best_survey_pdt" # Rename back to unwieldy ridiculous but informative name

# Finish up
rm(daily_conditions_list)
cleaned[[length(cleaned) + 1]] <- daily_conditions
names(cleaned)[length(cleaned)] <- "daily_conditions"
rm(daily_conditions)

# 11 STANDARDIZE LOCATION DATA ----
# Extract location data from counts and species_ratios,
# import into OpenRefine, and clean. 
locations <- sort(unique(c(cleaned$counts$location, 
                      cleaned$species_ratios$location, 
                      cleaned$boundary_bay_counts$location)))
#write.csv(locations, "Output/locations_raw.csv", na = "", row.names = F)

# Now open OpenRefine - this requires the user to have OpenRefine
# installed on their machine and will open the program in a browser window!
# (only do this)
#rrefine::refine_upload("Output/locations_raw.csv", project.name = "bppeep_locations", open.browser = TRUE)

# On local machine this is at:
# http://127.0.0.1:3333/project?project=2454790870924&ui=%7B%22facets%22%3A%5B%5D%7D

# 12 ASSEMBLE SQLITE DATABASE ----

# The data in 'cleaned' can now be assembled into tables
# for the cleaned SQLite BPPeeps database.
# Some tables can be merged to reduce redundancy in the SQLite
# database.
# For example, canoe_pass_counts and boundary_bay_counts can
# likely be merged into a "other_region_counts" table
# counts and daily_conditions can be merged for consistency.
# That, or weather condition information should be fully
# split off counts into the daily_conditions table, with a 
# survey_id/count_id to be able to merge them easily.

sqlite_tables <- list()

## - 12.1 OTHER COUNTS ----
# Merge canoe_pass_counts and boundary_bay_counts
bbc <- cleaned$boundary_bay_counts
cpc <- cleaned$canoe_pass_counts

names(bbc)[2] <- "sub_location"
names(cpc)[3] <- "count"
names(cpc)[4] <- "notes"

bbc$location <- "Boundary Bay"
cpc$location <- "Canoe Pass"

other_region_counts <- dplyr::bind_rows(bbc, cpc)
rm(bbc)
rm(cpc)
other_region_counts <- other_region_counts %>% dplyr::select(date_time_pdt,
                                                             location,
                                                             sub_location,
                                                             species,
                                                             count,
                                                             high_tide_height_ft,
                                                             high_tide_time_pdt,
                                                             notes
                                                             )

# Convert dates to strings for SQLite compatibility
other_region_counts <- other_region_counts %>% 
  dplyr::mutate(dplyr::across(where(lubridate::is.POSIXct), as.character)) %>%
  dplyr::mutate(dplyr::across(where(lubridate::is.Date), as.character))

sqlite_tables[["other_region_counts"]] <- other_region_counts
rm(other_region_counts)

## - 12.2 STRIP COUNTS ----
# Convert dates to strings for SQLite compatibility
cleaned$strip_counts <- cleaned$strip_counts %>% 
  dplyr::mutate(dplyr::across(where(lubridate::is.POSIXct), as.character)) %>%
  dplyr::mutate(dplyr::across(where(lubridate::is.Date), as.character))

sqlite_tables[["strip_counts"]] <- cleaned$strip_counts

## - 12.3 BP COUNTS + DAILY CONDITIONS ----
# 'counts' and 'daily_conditions' will be two separate tables,
# but there are some redundant columns between the two - 
# may be good to pull weather data from 'counts' and move to 
# 'daily_conditions' table. tbd
c <- cleaned$counts
dc <- cleaned$daily_conditions

c$date <- as.Date(c$date_time_pdt, format = "%Y-%m-%d", tz = "Canada/Pacific")

# This will result in some duplicated column names. Only the
# 'notes' columns need to be concatenated. 
tmp <- merge(c, dc, by = "date", all = TRUE)
tmp$notes <- apply(tmp[,c("notes.x", "notes.y")], 1, function(x) paste(x[!is.na(x)], collapse = "; "))

# Remaining can simply be ifelse statements - choose whichever
# column is not null for the value
dupes <- names(tmp)[grepl("\\.x", names(tmp)) & !grepl("notes", names(tmp))]
dupes <- gsub(".x", "", dupes)

for (i in 1:length(dupes)){
  col.x <- paste0(dupes[i], ".x")
  col.y <- paste0(dupes[i], ".y")
  # If/else gets wonky with factors if levels don't exactly
  # match between the two columns
  if (class(tmp[[col.x]])[1] == "factor") {
    tmp[[col.x]] <- as.character(tmp[[col.x]])
    tmp[[col.y]] <- as.character(tmp[[col.y]])
    }
  tmp[[dupes[i]]] <- dplyr::if_else(is.na(tmp[[col.x]]),
                                    tmp[[col.y]],
                                    tmp[[col.x]])
}
rm(col.x)
rm(col.y)
rm(dupes)
rm(i)

# Select final columns
tmp <- tmp %>% dplyr::select(date,
              date_time_pdt,
              location,
              weekday,
              sweep_start_pdt,
              sweep_end_pdt,
              survey_start_pdt,
              survey_end_pdt,
              approx_11_5_ft_tide_or_best_survey_pdt,
              count_1,
              count_2,
              count_3,
              count_4,
              count_5,
              final_count, 
              in_daily_total_yn,
              other_birds,
              high_tide_time_pdt,
              high_tide_height_ft,
              high_tide_height_m,
              tide,
              degrees_c,
              weather,
              cloud_cover_percent,
              precipitation,
              wind,
              wind_direction,
              wind_speed_kn,
              wind_speed_kmh,
              notes,
              observer,
              julian_date,
              raw_datafile)

tmp$tide <- as.factor(tmp$tide)
tmp$wind_direction <- as.factor(tmp$wind_direction)
tmp$julian_date <- lubridate::yday(tmp$date)

# Convert dates to strings for SQLite compatibility
tmp <- tmp %>% 
  dplyr::mutate(dplyr::across(where(lubridate::is.POSIXct), as.character)) %>%
  dplyr::mutate(dplyr::across(where(lubridate::is.Date), as.character))

sqlite_tables[["bp_counts"]] <- tmp
rm(tmp)
rm(c)
rm(dc)

## - 12.4 SPECIES RATIOS ----
# Convert dates to strings for SQLite compatibility
cleaned$species_ratios <- cleaned$species_ratios %>% 
  dplyr::mutate(dplyr::across(where(lubridate::is.POSIXct), as.character)) %>%
  dplyr::mutate(dplyr::across(where(lubridate::is.Date), as.character))

sqlite_tables[["species_ratios"]] <- cleaned$species_ratios

## - 12.5 RAPTORS ----
# Convert dates to strings for SQLite compatibility
cleaned$raptors <- cleaned$raptors %>% 
  dplyr::mutate(dplyr::across(where(lubridate::is.POSIXct), as.character)) %>%
  dplyr::mutate(dplyr::across(where(lubridate::is.Date), as.character))

sqlite_tables[["raptors"]] <- cleaned$raptors

## - 12.6 CLEANED LOCATIONS ----
# This assumes locations were cleaned using OpenRefine
# above.
# bppeep_locations <- rrefine::refine_export(project.name = "bppeep_locations",
#                                            show_col_types = FALSE)
bppeep_locations <- read.csv("supporting_files/bppeep_locations-2.csv")

# [Further modifications as needed can be done in R here]
sqlite_tables[["locations"]] <- bppeep_locations

# 13 POPULATE SQLITE DB ----
bppeeps <- DBI::dbConnect(RSQLite::SQLite(), "Output/bppeeps.db")

# Populate tables
for (i in 1:length(sqlite_tables)) {
  DBI::dbWriteTable(bppeeps, names(sqlite_tables)[i], sqlite_tables[[i]], overwrite = TRUE)
}

# 13.1 CREATE VIEWS ----
# Daily percentage WESA/DUNL view
DBI::dbExecute(bppeeps, "drop view if exists daily_percent_ratios;")
DBI::dbExecute(bppeeps, "create view daily_percent_ratios as 
               with temp as (select date(date_time_pdt) as date, 
               avg(wesa) as wesa, 
               avg(dunl) as dunl 
               from species_ratios 
               group by date(date_time_pdt)) 
               select *, 
               (wesa + dunl) as total, 
               (wesa / (wesa + dunl) * 100) as p_wesa, 
               (dunl / (wesa + dunl) * 100) as p_dunl 
               from temp;")

# Daily WESA/DUNL population totals view
DBI::dbExecute(bppeeps, "drop view if exists daily_wesa_dunl_population;")
DBI::dbExecute(bppeeps, "create view daily_wesa_dunl_population as
               with temp as (select date, 
               sum(mean_count) as daily_count, 
               cleaned 
               from bp_counts 
               inner join locations 
               on bp_counts.location = locations.original_location_name
                group by date, cleaned)
               select temp.date, 
                cleaned as location, 
                round(((p_wesa/100) * daily_count), 0) as pop_wesa, 
                round(((p_dunl/100) * daily_count), 0) as pop_dunl 
                from temp 
                inner join daily_percent_ratios d 
                on temp.date = d.date;")

DBI::dbDisconnect(bppeeps)
