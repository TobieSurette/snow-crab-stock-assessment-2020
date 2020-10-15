# Regular survey measurements:
x <- read.csv("data/raw/scs.len.2020.csv", stringsAsFactors = FALSE)
names(x) <- tolower(names(x))
x$tow.id <- toupper(x$gpnum)

x$date <- unlist(lapply(strsplit(x$datetime, " "), function(x) x[1]))
x$time <- unlist(lapply(strsplit(x$datetime, " "), function(x) x[2]))

# Parse measurement field:
x$length.unit <- gsub("[0-9.]", "", x$measurement)
x$length <- as.numeric(gsub("m", "", x$measurement))

# Remove irrelevant data:
x <- x[, -which(names(x) %in% c("measurement", "measurementnum", "measurement.transformed", "measurement.comment", "one.hundred.message", "dataentrybox.measure", "datetime", "gpnum"))]

# Define length precision:
x$length.precision <- 1

vars <- c("date", "time", "tow.id", "species", "length", "length.unit", "length.precision")
x <- x[vars]

# Corrections:


# Write to file:
write.csv(x, "data/by-catch/scs.len.2020.csv", row.names = FALSE)
