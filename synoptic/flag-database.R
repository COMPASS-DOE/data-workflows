
# The flag database

library(DBI)

#FDB <- dbConnect(RSQLite::SQLite(), "flag-db.sqlite")
#dbDisconnect(FDB)


# Add flag data to the database
# Each site gets its own table, which is created if necessary
fdb_add_flags <- function(site, flag_data, fdb = FDB) {
    if(site %in% dbListTables(fdb)) {
        rows <- dbAppendTable(fdb, site, flag_data)
    } else {
        message("Creating new site ", site)
        dbWriteTable(fdb, site, flag_data)
        rows <- nrow(flag_data)
    }
    message("Added ", rows, " rows")
}

# Get flag data for a single site
# If there are no flags for the site, return NULL
fdb_get_flags_site <- function(site, fdb = FDB) {
    if(site %in% dbListTables(fdb)) {
        dbReadTable(fdb, site)
    } else {
        messsage("No flags for site ", site)
        NULL
    }
}

# Get flag data for one or more IDs in a site
# Supplying an invalid site is an error
fdb_get_flags_ids <- function(site, ids, fdb = FDB) {
    # Using paste() is theoretically dangerous in this context (see
    # https://rsqlite.r-dbi.org/articles/rsqlite#queries) but I couldn't
    # get the table name to work as a parameter
    dbGetQuery(fdb,
               paste('SELECT * FROM', site, 'WHERE ID IN (?)'),
               params = list(ids))
}

# Delete flag data for one or more IDs in a site
# Supplying an invalid site is an error
fdb_delete_ids <- function(site, ids, fdb = FDB) {
    # Using paste() is theoretically dangerous in this context (see
    # https://rsqlite.r-dbi.org/articles/rsqlite#queries) but I couldn't
    # get the table name to work as a parameter
    res <- dbSendStatement(fdb,
               paste('DELETE FROM', site, 'WHERE ID IN (?)'),
               params = list(ids))
    message("Deleted ", dbGetRowsAffected(res), " rows")
    dbClearResult(res)
}



# ============= Test code =============

# Create test database
test_db <- dbConnect(RSQLite::SQLite(), ":memory:")
test1 <- data.frame(ID = 1:3, Flag_type = letters[1:3])
fdb_add_flags("site1", test1, fdb = test_db)

# Pull out data for an entire site (table)
x <- fdb_get_flags_site("site1", fdb = test_db)
stopifnot(identical(x, test1))

# Search by Site-ID
x <- fdb_get_flags_ids("site1", ids = 1:2, fdb = test_db)
stopifnot(identical(x, subset(test1, ID %in% 1:2)))

# Remove IDs
fdb_delete_ids("site1", ids = 1, fdb = test_db)
x <- fdb_get_flags_site("site1", fdb = test_db)
stopifnot(!1 %in% x$ID)

dbDisconnect(test_db)
