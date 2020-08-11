library(gulf)

# Load raw data export:
x <- read.table("U:/Snow Crab/Stock Assessment 2020/data/raw/Tow Data.txt", sep = "\t", header = TRUE, stringsAsFactors = FALSE)
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
x$comment <- gsub("(É)|(Ã‰)", "E", x$comment)
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
x$end.time.logbook   <- x$gpa.time.end
x$start.time.logbook[nchar(x$start.time.logbook) > 8] <- "        "
x$mid.time.logbook[nchar(x$mid.time.logbook) > 8]     <- "        "
x$end.time.logbook[nchar(x$end.time.logbook) > 8]     <- "        "

# Coordinate conversion:
x$longitude.start.logbook <- -dmm2deg(x$gpa.lon.start)
x$longitude.end.logbook   <- -dmm2deg(x$gpa.lon.end) 
x$latitude.start.logbook  <- dmm2deg(x$gpa.lat.start) 
x$latitude.end.logbook    <- dmm2deg(x$gpa.lat.end) 

# Tow validity:
x$valid <- as.numeric(tolower(x$tow.quality) == "good")

# Add variables to be filled-in later:
x$start.time        <- "        "
x$end.time          <- "        "
x$swept.area        <- as.numeric(NA)
x$swept.area.method <- as.numeric(NA)
x$groundfish.sample	<- 0
x$water.sample      <- as.numeric(NA)
x$longitude         <- as.numeric(NA)
x$latitude          <- as.numeric(NA)

x$tow.number <- x$trawl.number
x$warp <- x$cables

# Remove irrelevant variables:
vars <- c("year", "month", "day", "zone", "tow.number", "tow.id", 
          "start.time.logbook", "end.time.logbook", "start.time", "end.time", "valid",
          "longitude", "latitude", "longitude.start.logbook", "longitude.end.logbook", 
          "latitude.start.logbook", "latitude.end.logbook",
          "depth", "bottom.temperature", "warp", "swept.area", "swept.area.method", "groundfish.sample", "water.sample", "comment")
x <- x[vars]    


write.csv(x, file = "U:/Snow Crab/Stock Assessment 2020/data/scs.set.2020.csv", row.names = FALSE)

class(x) <- c("scset", "gulf.set", "ascii", "data.frame")
attr(x, "converted") <- TRUE

write(x, file = "W:/Crab/Offshore Crab Common/Fishing Year 2020/Trawl Data/South Western Gulf/Tow Data/Tows 2020.txt")
       