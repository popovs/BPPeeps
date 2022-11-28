# Supporting files

Certain data cleaning processes were not done in R as it was not the most expedient means to do so. 

## Locations
**Locations** data were cleaned using **OpenRefine**. The data cleaning steps from OpenRefine were exported as a JSON file, available in **`supporting_files/history.json`**; the resulting cleaned locations data were subsequently exported from OpenRefine as **`supporting_files/bppeep_locations.csv`**. Simply import the raw locations data + JSON cleaning steps into your own OpenRefine project if you would like to further modify any of the locations data cleaning. Contact me if you would like me to just send you my OpenRefine project file.

## Daily totals
Any pre-2014 raw data includes daily population count data tallied up into a *daily count total*. However, the methodology for adding up count data varies year-by-year and indeed even day-by-day. As such it was more expedient to simply go through each raw data point and manually determine whether or not certain count data was included in the daily total for a given day rather than figuring out some hacky and needlessly complex programmatic workaround. The result of this manual data check is **`supporting_files/in_total_yn.csv`**. 
- `TRUE` - means the `final_count` for that location was included in the daily total.
- `FALSE` - means the `final_count` for that location was excluded from the daily total.
- `AVG` - means that there were likely multiple bird surveys ('sweeps') undertaken that day; the `final_count` values for each sweep were added together for a daily *subtotal*, and the subtotals were then averaged together to get the daily *total* for that day.
- `only total` - means that there were no locations counts for that day - `final_count` is the daily total population estimate for that day
- `total` - means that that the value in `final_count` for a particular record is the daily total
- `NA` - means that there was no daily total for that day calculated, and that the data in `final_count` can just be used as-is (this is the case for any data 2014-onwards).