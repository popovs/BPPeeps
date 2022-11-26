## Prepare data for models

# Pull relevant data from the sqlite db, format, and add covariates

# 01 Pull db  ----
# TODO: store peep db in better location and update path
db <- DBI::dbConnect(RSQLite::SQLite(), "Output/bppeeps.db")
