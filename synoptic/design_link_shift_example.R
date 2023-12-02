# Preparing for the design_link table to have a "valid_until" column
# that we can handle EFFICIENTLY


# The design links might not be stable over time; for example, if a tree
# dies, its sensor might get reassigned to a new tree. In this case the
# design_link table will have two entries, one for the old assignment and
# one for the new. We know which one to use by the "valid_until" column,
# which give a end date for a design link. Most entries will be NA.
#
# So, when the design_link table is merged with a data table, if a reassignment
# has occurred, some data rows will get repeated with the different possible
# design links.
#
# This function uses the object name (group identifier -- typically, Logger+
# Table+Loggernet_variable), timestamp, and valid_until timestamps to identify
# which rows to keep (correct design_link assignment) and which to drop.
valid_entries <- function(objects, times, valid_until) {
    # Any NA valid_until entries apply into the far future
    valid_until[is.na(valid_until)] <- 99999 # max int basically
    past_valid_time <- times > valid_until
    # Create a data frame to aggregate and then merge, below
    x <- data.frame(obj = objects, time = times, vu = valid_until)
    # Compute the minimum valid_until entry for each object and time that is
    # not past the valid_until point; this is the 'controlling' value
    y <- aggregate(vu ~ obj + time, data = x[!past_valid_time,], FUN = min)
    names(y)[3] <- "controlling"
    # Figure out controlling valid_until for each object/time
    z <- merge(x, y, all.x = TRUE)
    # An NA controlling entry means there is none
    z$controlling[is.na(z$controlling)] <- FALSE

    return(z$vu == z$controlling)
}

# Sample data. We have two objects (sensors) at time points 1:3
test_data <- data.frame(obj = c(1, 1, 1, 2, 2, 2), time = c(1, 2, 3, 1, 2, 3))
# Object 2 changes its design link after time 2
test_dt <- data.frame(obj = c(1,2,2),
                      dl = c("A", "B", "C"),
                      valid_until = c(NA, 2, NA))
# Merge the 'data' with the 'design link table'
x <- merge(test_data, test_dt)
# Call valid_entries. It figures out that all the object 1 entries should be
# retained, but 1 of 2 entries in each timestep should be dropped for object 2.
# This is because there are two design_table entries for it (see above); the
# first ends at time point 2, and the second is indefinite after that.
valid_entries(x$obj, x$time, x$valid_until)

# Test code

# No shifting objects
ret <- valid_entries(c(1, 1, 1), c(1, 2, 3), c(NA, NA, NA))
stopifnot(all(ret))
# One object, shift time is never reached
ret <- valid_entries(c(1, 1, 1, 1), c(1, 1, 2, 2), c(4, NA, 4, NA))
stopifnot(ret == c(TRUE, FALSE, TRUE, FALSE))
# One object, shift time is in the past
ret <- valid_entries(c(1, 1, 1, 1), c(3, 3, 4, 4), c(2, NA, 2, NA))
stopifnot(ret == c(FALSE, TRUE, FALSE, TRUE))
# One object, shifts
ret <- valid_entries(c(1, 1, 1, 1), c(2, 2, 3, 3), c(2, NA, 2, NA))
stopifnot(ret == c(TRUE, FALSE, FALSE, TRUE))
# One objects, shifts twice (valid_untils at 1 and 2)
ret <- valid_entries(objects = rep(1, 9),
                     times = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
                     valid_until = c(1, 2, NA, 1, 2, NA, 1, 2, NA))
stopifnot(ret == c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE))
# Two objects, only one shifts
ret <- valid_entries(objects = c(1, 1, 1, 2, 2, 2, 2, 2, 2),
                    times = c(1, 2, 3, 1, 1, 2, 2, 3, 3),
                    valid_until = c(NA, NA, NA, 2, NA, 2, NA, 2, NA))
stopifnot(ret == c(TRUE, TRUE, TRUE, # obj 1
                   TRUE, FALSE, TRUE, FALSE, FALSE, TRUE)) # obj 2
# There's a valid_until but no new entry
ret <- valid_entries(objects = c(1, 1),
                     times = c(1, 2),
                     valid_until = c(1, 1))
stopifnot(ret == c(TRUE, FALSE))

