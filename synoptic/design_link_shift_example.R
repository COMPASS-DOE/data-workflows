# Preparing for the design_link table to have a "valid_until" column
# that we can handle EFFICIENTLY

# Two objects (sensors) at time points 1:3
test_data <- data.frame(obj = c(1,1,1,2,2,2), time = c(1,2,3,1,2,3))
# Object 2 changes its design link after time 2
test_dt <- data.frame(obj = c(1,2,2),
                      dl = c("A", "B", "C"),
                      valid_until = c(NA, 2, NA))

x <- merge(test_data, test_dt)

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
    # If no non-NA valid_until entries, nothing will change
    if(all(is.na(valid_until))) {
        return(rep(TRUE, length(objects)))
    }

    # Identify entries where the time is after valid_until
    # Most valid_until entries will likely be NA
    past_valid_time <- !is.na(valid_until) & times > valid_until
    # If nothing is past valid time, nothing changed, just drop NA rows
    if(all(!past_valid_time)) {
        return(!is.na(valid_until))
    }

    # Identify 'shift objects': they passed a time_valid
    shift_objects <- unique(objects[past_valid_time])
    # Identify 'shift happened' object/times
    x <- data.frame(obj = objects, time = times, valid_until = valid_until,
                    past_valid_time = past_valid_time)
    y <- x[past_valid_time,]
    y <- y[!duplicated(y),]
    y$sh <- TRUE # mark 'shift happened'
    z <- merge(x, y, all.x = TRUE)
    z$sh[is.na(z$sh)] <- FALSE
    shift_happened <- z$sh

    # con <- aggregate(valid_until ~ obj + time, data = z[!past_valid_time,],
    #                  FUN = min, na.rm = TRUE)
    # names(con)[3] <- "controlling"
    # z <- merge(z, con, all.x = TRUE)
    # controlling_vu <- z$controlling

    # Identify which entries to retain
    retain <-
        # objects that have no shift possibility
        !objects %in% shift_objects |
        # shift objects in which a shift happened
        (objects %in% shift_objects & shift_happened) |
        # shift objects but no shift happened and non-NA valid_until
        (shift_objects & !shift_happened & !is.na(valid_until))
    # Definitely want to drop entries that have exceeded valid_until, however
    retain[past_valid_time] <- FALSE
    retain
}
x <- merge(test_data, test_dt)
debug(valid_entries)
valid_entries(x$obj, x$time, x$valid_until)

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

# Two objects, one shifts
ret <- valid_entries(objects = c(1, 1, 1, 2, 2, 2, 2, 2, 2),
                    times = c(1, 2, 3, 1, 1, 2, 2, 3, 3),
                    valid_until = c(NA, NA, NA, 2, NA, 2, NA, 2, NA))
stopifnot(ret == c(TRUE, TRUE, TRUE, # obj 1
                   TRUE, FALSE, TRUE, FALSE, FALSE, TRUE)) # obj 2


