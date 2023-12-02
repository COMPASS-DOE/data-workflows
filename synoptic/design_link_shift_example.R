# Preparing for the design_link table to have a "valid_until" column
# that we can handle EFFICIENTLY

# Two objects (sensors) at time points 1:3
test_data <- data.frame(obj = c(1,1,1,2,2,2), time = c(1,2,3,1,2,3))
# Object 2 changes its design link after time 2
test_dt <- data.frame(obj = c(1,2,2),
                      dl = c("A", "B", "C"),
                      valid_until = c(NA, 2, NA))

x <- merge(test_data, test_dt)

# Identify entries where the time is after valid_until
x$past_valid_time <- !is.na(x$valid_until) & x$time > x$valid_until
# Identify objects and times where a shift occurs
# A "shift" is an observation (object + time) where we change design links
pastvalids <- x[x$past_valid_time, c("obj", "time")]
pastvalids$shift_happened <- TRUE
x <- merge(x, pastvalids, all.x = TRUE)
x$shift_happened[is.na(x$shift_happened)] <- FALSE
# Identify shift objects
shifting_objects <- test_dt[!is.na(test_dt$valid_until),]$obj
x$shift_object <- x$obj %in% shifting_objects

# Whew! Now we can
# 1. Delete past-valid-time entries
x <- x[!x$past_valid_time,]
# 2. Retain rows that are not shift objects...
x <- x[!x$shift_object |
           # or are shift objects and a shift happened
           # (i.e. something got deleted in step 1 above)
           (x$shift_object & x$shift_happened) |
           # or are shift objects, no shift happened, with a non-NA valid_until
           (x$shift_object & !x$shift_happened & !is.na(x$valid_until)),
           ]

