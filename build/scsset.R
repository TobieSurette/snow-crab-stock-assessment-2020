library(gulf.data)

# Load raw data export:
x <- read.table("data/raw/scs.set.2020.csv", header = TRUE, sep =",", stringsAsFactors = FALSE, fileEncoding = "Windows-1252")
names(x) <- gsub("_", ".", tolower(names(x)))

# Rename fields:
x$tow.id <- toupper(x$gpnumber)

# Reformat data:
x$bottom.temperature <- gsub(",", ".", x$bottom.temperature)
x$bottom.temperature <- gsub("O", "0", x$bottom.temperature)
x$bottom.temperature[x$bottom.temperature == ""] <- NA
x$bottom.temperature <- as.numeric(x$bottom.temperature)

# Parse comment fields:
x$comment <- gsub("[ ]*$", "", x$comment)
index <- x$comment == ""
x$comment[index] <- x$speed.comment[index]
x$speed.comment <- gsub("[ ]*$", "", x$speed.comment)
x$comment[!index] <- paste0(x$comment[!index], " - ", x$speed.comment[!index])
x$comment <- paste0(substr(x$comment, 1, 1), substr(tolower(x$comment), 2, nchar(x$comment)))

# Parse date fields:
x$day   <- as.numeric(unlist(lapply(strsplit(x$date, ".", fixed = TRUE), function(x) x[1])))
x$month <- as.numeric(unlist(lapply(strsplit(x$date, ".", fixed = TRUE), function(x) x[2])))
x$year  <- as.numeric(unlist(lapply(strsplit(x$date, ".", fixed = TRUE), function(x) x[3])))

# Zone and tow ID fields:
x$zone <- gsub("ZONE", "", toupper(x$zone))

# Fix time fields:
x$gpa.time.start <- gsub("?", "", x$gpa.time.start, fixed = TRUE)
x$gpa.time.start[x$gpa.time.start == ""] <- "        "
x$gpa.time.mid <- gsub("?", "", x$gpa.time.mid, fixed = TRUE)
x$gpa.time.mid[x$gpa.time.mid == ""] <- "        "
x$gpa.time.end <- gsub("?", "", x$gpa.time.end, fixed = TRUE)
x$gpa.time.end[x$gpa.time.end == ""] <- "        "

# Observed time fields:
x$start.time.logbook <- x$gpa.time.start
x$mid.time.logbook   <- x$gpa.time.mid
x$stop.time.logbook   <- x$gpa.time.end
x$start.time.logbook[nchar(x$start.time.logbook) > 8] <- "        "
x$mid.time.logbook[nchar(x$mid.time.logbook) > 8]     <- "        "
x$stop.time.logbook[nchar(x$stop.time.logbook) > 8]     <- "        "
x$haul.time <- x$net.end

# Coordinate conversion:
#x$longitude.start.logbook <- -dmm2deg(x$gpa.lon.start)
#x$longitude.end.logbook   <- -dmm2deg(x$gpa.lon.end)
#x$latitude.start.logbook  <- dmm2deg(x$gpa.lat.start)
#x$latitude.end.logbook    <- dmm2deg(x$gpa.lat.end)

x$longitude.start.logbook <- -abs(x$gpa.lon.start)
x$longitude.end.logbook   <- -abs(x$gpa.lon.end)
x$latitude.start.logbook  <- x$gpa.lat.start
x$latitude.end.logbook    <- x$gpa.lat.end

# Tow validity:
x$valid <- as.numeric(tolower(x$tow.quality) == "good")

# Add variables to be filled-in later:
x$start.time        <- "        "
x$stop.time          <- "        "
x$swept.area        <- as.numeric(NA)
x$swept.area.method <- as.numeric(NA)
x$groundfish.sample	<- 0
x$water.sample      <- as.numeric(NA)
x$longitude         <- as.numeric(NA)
x$latitude          <- as.numeric(NA)

x$tow.number <- x$trawl.number
x$warp <- x$cables

x <- x[setdiff(names(x), "date")]
x$date <- as.character(date(year = x$year, month = x$month, day = x$day))

# Remove irrelevant variables:
vars <- c("date", "zone", "tow.number", "tow.id", "valid",
          "start.time.logbook", "stop.time.logbook", "start.time", "stop.time", "haul.time",
          "longitude", "latitude", "longitude.start.logbook", "longitude.end.logbook", "latitude.start.logbook", "latitude.end.logbook",
          "depth", "bottom.temperature", "warp", "swept.area", "swept.area.method", "groundfish.sample", "water.sample", "comment")
x <- x[vars]

# Corrections:
x <- x[x$date != "2020-07-10", ]
x$stop.time.logbook[x$tow.id == "GP263F"] <- "10:30:55"
x$stop.time.logbook[x$tow.id == "GP159F"] <- "12:55:47"

# Load touchdown times:
if (file.exists("data/raw/scs.touchdown.time.2020.csv")){
   tmp <- read.csv("data/raw/scs.touchdown.time.2020.csv", header = TRUE, stringsAsFactors = FALSE)
   x$touchdown.time <- tmp$touchdown[match(x$tow.id, tmp$tow.id)]
   x$touchdown.time[is.na(x$touchdown.time)] <- "        "
}

# Load liftoff times:
if (file.exists("data/raw/scs.liftoff.time.2020.csv")){
   tmp <- read.csv("data/raw/scs.liftoff.time.2020.csv", header = TRUE, stringsAsFactors = FALSE)
   x$liftoff.time <- tmp$liftoff[match(x$tow.id, tmp$tow.id)]
   x$liftoff.time[is.na(x$liftoff.time)] <- "        "
}

tvars <- names(x)[grep("time", names(x))]

vars <- c("date", "zone", "tow.number", "tow.id", "valid", tvars, setdiff(vars, c("date", "zone", "tow.number", "tow.id", "valid", tvars)))
x <- x[vars]

write.csv(x, file = "data/scs.set.2020.csv", row.names = FALSE)
if (file.exists("C:/Users/SuretteTJ/Desktop/gulf.data")){
   write.csv(x, file = "C:/Users/SuretteTJ/Desktop/gulf.data/inst/extdata/scs.set.2020.csv", row.names = FALSE)
}

if (file.exists("W:/Crab/SuretteTJ/Desktop/gulf.data")){
   write.csv(x, file = "W:/Crab/SuretteTJ/Desktop/gulf.data/inst/extdata/scs.set.2020.csv", row.names = FALSE)
}

