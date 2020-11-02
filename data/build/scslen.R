# Regular survey measurements:
x <- read.csv(paste0("data/raw/scs.len.", year, ".csv"), header = TRUE, stringsAsFactors = FALSE)

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
write.table(y, file = paste0("data/by-catch/scs.len.", year, ".csv"), sep = ",", row.names = FALSE)

# Write to gulf.data repository:
tmp <- unlist(lapply(strsplit(getwd(), "/"), function(x) x[length(x)]))
path <- paste0(gsub(tmp, "", getwd()), "gulf.data/inst/extdata")
if (file.exists(path)){
   file <- paste0(path, "/", "scs.cat.", year, ".csv")
   write.csv(x, file = file, row.names = FALSE)
}
