library(magrittr) # for assignment pipe '%<>%' operator

## 01 IMPORT ----
# Relative file paths to this source file
f <- list.files("RobertsBankShorebirdSurveys/")
f <- f[!grepl("^~", f)] # Drop hidden excel files, if present
raw <- list()
for (i in 1:length(f)) {
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
# 6) raptors
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
# 'summary_totals_list', 'compliation_dailt_wesa_dunl_list'  
# could probably be removed as well - 
# they just compile data from other tabs

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

# Finish up
rm(boundary_bay_counts_list)
cleaned[[1]] <- boundary_bay_counts
names(cleaned)[1] <- "boundary_bay_counts"

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
  counts_list[[i]]$raw_datafile <- f[i]
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
View(counts[which((round(counts$mean_count) != counts$calc_mean) | (!is.na(counts$calc_mean) & is.na(counts$mean_count))),c("raw_datafile", "location", "count_1", "count_2", "count_3", "count_4", "count_5", "mean_count", "calc_mean")])
# Of those, the vast majority are differences in rounding.
# Next subset records where difference is greater > 200.
counts$mean_diff <- counts$calc_mean - ifelse(is.na(counts$mean_count), 0, round(counts$mean_count))
#View(counts[which(counts$mean_count != counts$calc_mean & (counts$mean_diff > 200 | counts$mean_diff < -200)),c("raw_datafile", "location", "count_1", "count_2", "count_3", "count_4", "count_5", "mean_count", "calc_mean", "mean_diff")])
View(counts[which(counts$mean_diff > 200 | counts$mean_diff < -200),c("raw_datafile", "location", "count_1", "count_2", "count_3", "count_4", "count_5", "mean_count", "calc_mean", "mean_diff")])
#This doesn't include NA mean_count: mean_errors <- counts[which(counts$mean_count != counts$calc_mean & (counts$mean_diff > 200 | counts$mean_diff < -200)),c("raw_datafile", "location", "count_1", "count_2", "count_3", "count_4", "count_5", "mean_count", "calc_mean", "mean_diff")]
mean_errors <- counts[which(counts$mean_diff > 200 | counts$mean_diff < -200),c("raw_datafile", "location", "count_1", "count_2", "count_3", "count_4", "count_5", "mean_count", "calc_mean", "mean_diff")]

errors <- list()
errors[[1]] <- mean_errors
names(errors)[1] <- "counts_mean_errors"
rm(mean_errors)

## Column means
# Extract any 'TOTAL' rows - i.e. subtotal rows within dataset
subtotals <- counts[grep("tot", tolower(counts$location)),]
subtotals$date <-  as.Date(subtotals$date_time_pdt, format = "%Y-%m-%d", tz = "Canada/Pacific")

# Compare if manually calc'd col means == R calc'd col means
# Group records by date, then take the mean of calc_mean by date,
# then compare to matching row mean in 'subtotals' df
day_means <- counts[!grepl("tot", tolower(counts$location)),] %>% 
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
  dplyr::select(date, raw_datafile, location, subtot_tot, calc_tot, tot_diff, count_1, count_2, count_3, count_4, count_5,  calc_mean)

rm(day_means)
rm(subtotals)
rm(subtotal_dates)

# 2022-08-07: have sent flagged records to data owners to 
# hear feedback. For now, counts cleaning is done. 
# 'location' standardization will be done with Google
# OpenRefine. 

# Rearrange and remove any superfluous columns
# Dropping columns if:
# - they were merged with another column above (e.g., 'comments')
# - they have no values (e.g., 'count_id' is only NAs; it was used for excel VLOOKUP purposes)
# - they have only one value (e.g., 'dunlin' has one value in one cell, which was moved to 'other_birds')
# - they have only one unique value (e.g., all records in 'day_survey' simply say '1')
# - they were merged into the date_time_pdt column
# - 'mean_count' col - this column should be calculated on-the-fly so as to account for any changes to underlying count data and prevent future 'mean_count' errors
counts <- counts %>% 
  dplyr::select(date_time_pdt, 
                tide, 
                location, 
                count_1, 
                count_2, 
                count_3, 
                count_4, 
                count_5, 
                #mean_count, 
                notes, 
                high_tide_height_ft, 
                high_tide_time_pdt, 
                weather, 
                wind, 
                observer, 
                other_birds, 
                julian_date, 
                raw_datafile)

counts <- counts[!grepl("tot", tolower(counts$location)),]

# Finish up
rm(counts_list)
cleaned[[length(cleaned) + 1]] <- counts
names(cleaned)[length(cleaned)] <- "counts"

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



# 10 CLEAN 'daily_conditions' ----
