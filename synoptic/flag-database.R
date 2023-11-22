
# The flag database

library(DBI)

FDB <- dbConnect(RSQLite::SQLite(), "flag-db.sqlite")

flag_db_template <- data.frame(Timestamp = as.POSIXct(NA),
                               ID = NA_character_,
                               Type = NA_character_)
if(!"flags" %in% dbListTables(FDB)) {
    dbWriteTable(FDB, "flags", flag_db_template)
}
dbDisconnect(FDB)


fdb_add_flags <- function(flag_data, fdb = FDB) {
    ra <- dbAppendTable(fdb, "flags", flag_data)
    message("Added ", ra, " rows")
}

test1 <- data.frame(Timestamp = rep(Sys.time(), 3), ID = 4:6, Type = letters[1:3])
