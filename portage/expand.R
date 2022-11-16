

# Expand a string: look for patterns like {x,y,z} within a possibly
# larger string, and break them apart at the commas
# So "Hello {A,B2,C}" -> c("Hello A", "Hello B2", "Hello C")
# It also handles numerical sequences: "x{1:3}" -> c("x1", "x2", "x3")
# Comma expansions are performed before colon expansions:
# "{A,B{1:3},C}" -> c("A", "B1", "B2", "B3", "C")
expand_string <- function(s, expand_comma = TRUE, expand_colon = TRUE, quiet = TRUE) {
    if(!quiet) message(s)
    if(is.na(s)) return(s)

    # Look for 1+ "words" (groups of characters followed by a comma)
    # followed by commas (and perhaps white space), and then a final word
    if(expand_comma) {
        COMMA_PATTERN <- "\\{(.+,)+.+\\}"
        matches <- regexpr(COMMA_PATTERN, s)
        if(matches > 0) {
            subs <- strsplit(regmatches(s, matches), ",")[[1]]
            subs[1] <- gsub("^\\{", "", subs[1]) # get rid of beginning...
            subs[length(subs)] <- gsub("\\}$", "", subs[length(subs)]) # ...and end curly braces
            s <- rep(s, length(subs))
            newmatches <- regexpr(COMMA_PATTERN, s)
            regmatches(s, newmatches) <- trimws(subs)
            # Recurse once to look for possible colon expansions
            s <- unlist(sapply(s, expand_string,
                               expand_comma = FALSE, expand_colon = expand_colon,
                               USE.NAMES = FALSE))
            return(s)
        }
    }

    if(expand_colon) {
        # Look for two numbers separated by a colon, with optional white space
        COLON_PATTERN <- "\\{\\s*\\d+\\s*:\\s*\\d+\\s*\\}"
        matches <- regexpr(COLON_PATTERN, s)
        if(matches > 0) {
            subs <- strsplit(regmatches(s, matches), ":")[[1]]
            subs <- gsub("[\\{\\}]", "", subs) # get rid of curly braces
            subs <- seq.int(from = subs[1], to = subs[2])
            s <- rep(s, length(subs))
            newmatches <- regexpr(COLON_PATTERN, s)
            regmatches(s, newmatches) <- subs
        }
    }

    s
}

# Expand a data frame: look for patterns like x,y,z within entries and,
# for that row, replicate it and use expand_string to break apart the x,y,z
# Multiple expansions within a row are OK as long as they're the same length
expand_df <- function(df) {
    results <- list()
    for(i in seq_len(nrow(df))) {
        dfr <- df[i,]  # row we're working on
        # Figure out max expansion (may be 1) and replicate row that many times
        expand_lens <- sapply(dfr, function(x) length(expand_string(x)))
        new_df <- dfr[rep(1, max(expand_lens)),]

        # Can't have mismatches (except for 1-length)
        # E.g. "x,y" in one cell and "x,y,z" in another
        if(length(setdiff(unique(expand_lens), 1)) > 1) {
            stop("Row ", i, " has mismatched expansion entries")
        }

        # For each column, expand its entry string as needed
        if(nrow(new_df) > 1) {
            for(col in seq_along(new_df)) {
                new_df[col] <- expand_string(new_df[[1, col]])
            }
        }
        results[[i]] <- new_df
    }

    bind_rows(results)
}
