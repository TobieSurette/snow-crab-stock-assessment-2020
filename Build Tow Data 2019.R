library(gulf)

year <- 2019

# Regular survey data export from FileMaker Pro:
y <- read.table(paste0("U:/Snow Crab/Stock Assessment 2019/Tows 2019.tab"), sep = "\t", header = TRUE, stringsAsFactors = FALSE)  
names(y) <- c("bottom.temperature", "warp", "clock.time", "comment", "date", "dawn", "depth", "diff.time.start", "diff.time.start.clock",
              "dusk", "end.time.clock", "esonar.file", "gpa.lat.end",  "gpa.lat.mid", "gpa.lat.start", "gpa.lon.end",  "gpa.lon.mid", "gpa.lon.start",
              "gpa.n.end",  "gpa.n.mid", "gpa.n.start", "gpa.time.end",  "gpa.time.mid", "gpa.time.start", "gpa.w.end",  "gpa.w.mid", "gpa.w.start",
              "tow.id", "latitude.end", "latitude.mid", "latitude.start", "longitude.end", "longitude.mid", "longitude.start", 
              "mid.time.clock", "minilog.file", "minilog.file.code", "minilog.file.seq", "net.end", "net.start", "speed.comment", 
              "star.oddi.headline.code", "star.oddi.headline.file", "star.oddi.tilt.code", "star.oddi.tilt.file",
              "start.time.clock", "sunrise", "sunset", "valid", "tow.number", "validate", "zone")
              
# Regular survey data export from FileMaker Pro:
z <- read.table(paste0("U:/Snow Crab/Stock Assessment 2019/Comparative Tows 2019.tab"), sep = "\t", header = TRUE, stringsAsFactors = FALSE)  
names(z) <- c("bottom.temperature", "warp", "clock.time", "comment", "date", "dawn", "depth", "diff.time.start", "diff.time.start.clock",
              "dusk", "end.time.clock", "esonar.file", "gpa.lat.end",  "gpa.lat.mid", "gpa.lat.start", "gpa.lon.end",  "gpa.lon.mid", "gpa.lon.start",
              "gpa.n.end",  "gpa.n.mid", "gpa.n.start", "gpa.time.end",  "gpa.time.mid", "gpa.time.start", "gpa.w.end",  "gpa.w.mid", "gpa.w.start",
              "tow.id", "latitude.end", "latitude.mid", "latitude.start", "longitude.end", "longitude.mid", "longitude.start", 
              "mid.time.clock", "minilog.file", "minilog.file.code", "minilog.file.seq", "net.end", "net.start", "speed.comment", 
              "star.oddi.headline.code", "star.oddi.headline.file", "star.oddi.tilt.code", "star.oddi.tilt.file",
              "start.time.clock", "sunrise", "sunset", "valid", "tow.number", "validate", "zone")

# Combine the two surveys:
y <- rbind(y, z)
                    
# Reformat data:
y$bottom.temperature <- gsub(",", ".", y$bottom.temperature)
y$bottom.temperature <- gsub("O", "0", y$bottom.temperature)
y$bottom.temperature[y$bottom.temperature == ""] <- NA
y$bottom.temperature <- as.numeric(y$bottom.temperature)

substr(y$tow.id[grep("FR$", y$tow.id)], nchar(y$tow.id[grep("FR$", y$tow.id)])-1, nchar(y$tow.id[grep("FR$", y$tow.id)]))
substr(y$tow.id[grep("FR$", y$tow.id)], nchar(y$tow.id[grep("FR$", y$tow.id)])-1, nchar(y$tow.id[grep("FR$", y$tow.id)]))

# Parse comment fields:
y$comment <- gsub("[ ]*$", "", y$comment)
index <- y$comment == ""
y$comment[index] <- y$speed.comment[index]
y$speed.comment <- gsub("[ ]*$", "", y$speed.comment)
y$comment[!index] <- paste0(y$comment[!index], " - ", y$speed.comment[!index])
y$comment <- gsub("(É)|(Ã‰)", "E", y$comment)
y$comment <- paste0(substr(y$comment, 1, 1), substr(tolower(y$comment), 2, nchar(y$comment)))
           
# Parse date fields:
y$day   <- as.numeric(unlist(lapply(strsplit(y$date, ".", fixed = TRUE), function(x) x[1])))
y$month <- as.numeric(unlist(lapply(strsplit(y$date, ".", fixed = TRUE), function(x) x[2])))
y$year  <- as.numeric(unlist(lapply(strsplit(y$date, ".", fixed = TRUE), function(x) x[3])))

# Zone and tow ID fields:
y$zone <- gsub("ZONE", "", toupper(y$zone))

# Do tow ID corrections:
y$tow.id <- toupper(y$tow.id)  
   
# Fix time fields:
y$gpa.time.start <- gsub("?", "", y$gpa.time.start, fixed = TRUE)
y$gpa.time.start[y$gpa.time.start == ""] <- "        "
y$gpa.time.mid <- gsub("?", "", y$gpa.time.mid, fixed = TRUE)
y$gpa.time.mid[y$gpa.time.mid == ""] <- "        "
y$gpa.time.end <- gsub("?", "", y$gpa.time.end, fixed = TRUE)
y$gpa.time.end[y$gpa.time.end == ""] <- "        "

# Observed time fields:
y$start.time.logbook <- y$gpa.time.start
y$mid.time.logbook   <- y$gpa.time.mid
y$end.time.logbook   <- y$gpa.time.end
y$end.time.logbook[y$tow.id == "GP096F"] <- "15:29:43"
y$start.time.logbook[y$tow.id == "GP353FR"] <- "09:51:05"
y$start.time.logbook[y$tow.id == "GP352F"] <- "11:09:54"
y$start.time.logbook[y$tow.id == "GC330F"] <- "06:46:59"
y$end.time.logbook[y$tow.id == "GC330F"] <- "06:53:30"
y$end.time.logbook[y$tow.id == "GC309F"] <- "11:25:47"
y$gpa.lon.start[y$tow.id == "GP154F"] <- "06304.182"
y$start.time.logbook[nchar(y$start.time.logbook) > 8] <- "        "
y$mid.time.logbook[nchar(y$mid.time.logbook) > 8]     <- "        "
y$end.time.logbook[nchar(y$end.time.logbook) > 8]     <- "        "

# Coordinate corrections:
# Coordinate fixes:
y$gpa.lon.start[y$tow.id == "GP115F"] <- "6318.098"
y$gpa.lon.start[y$tow.id == "GP347F"] <- "6035.479"
y$gpa.lon.start[y$tow.id == "GP317F"] <- "6103.023"
y$gpa.lon.start[y$tow.id == "GC330F"] <- "06053.916"
y$gpa.lat.start[y$tow.id == "GC330F"] <- "4649.766"
y$gpa.lon.end[y$tow.id == "GC330F"] <- "06054.006"
y$gpa.lat.end[y$tow.id == "GC330F"] <- "4649.622"
y$gpa.lat.start[y$tow.id == "GP284F"]  <- "04613.019900"
y$gpa.lon.start[y$tow.id == "GP284F"]  <- "06135.668400"
y$gpa.lat.end[y$tow.id == "GP284F"]    <- "04613.166600"
y$gpa.lon.end[y$tow.id == "GP284F"]    <- "06135.831800" 
y$gpa.lat.start[y$tow.id == "GP295F"]  <- "04805.648900" 
y$gpa.lon.start[y$tow.id == "GP295F"]  <- "06124.024600"
y$gpa.lat.end[y$tow.id == "GP295F"]    <- "04805.742100"
y$gpa.lon.end[y$tow.id == "GP295F"]    <- "06124.265300" 
y$gpa.lat.start[y$tow.id == "GP209F"]  <- "04754.461100" 
y$gpa.lon.start[y$tow.id == "GP209F"]  <- "06225.853100"
y$gpa.lat.end[y$tow.id == "GP209F"]    <- "04754.358400"
y$gpa.lon.end[y$tow.id == "GP209F"]    <- "06225.628300"
y$gpa.lat.start[y$tow.id == "GP107A1"] <- "04837.919500"
y$gpa.lon.start[y$tow.id == "GP107A1"] <- "06328.801700"
y$gpa.lat.end[y$tow.id == "GP107A1"]   <- "04837.981300"
y$gpa.lon.end[y$tow.id == "GP107A1"]   <- "06328.508900"

# Coordinate conversion:
y$longitude.start.logbook <- -dmm2deg(y$gpa.lon.start)
y$longitude.end.logbook   <- -dmm2deg(y$gpa.lon.end) 
y$latitude.start.logbook  <- dmm2deg(y$gpa.lat.start) 
y$latitude.end.logbook    <- dmm2deg(y$gpa.lat.end) 

# Tow validity:
y$valid <- ifelse(tolower(y$valid) == "good", 1, 0)

# Some observed start and end times must be corrected by hand:
y[which(y$start.time.logbook == "        "), ]

# Add variables to be filled-in later:
y$start.time        <- "        "
y$end.time          <- "        "
y$swept.area        <- as.numeric(NA)
y$swept.area.method <- as.numeric(NA)
y$groundfish.sample	<- 0
y$water.sample      <- as.numeric(NA)
y$longitude         <- as.numeric(NA)
y$latitude          <- as.numeric(NA)

# Add groundfish sampling station flag:
tmp <- read.csv("U:/Snow Crab/Stock Assessment 2019/Fish Sampling Stations 2019.csv", stringsAsFactors = FALSE)
index <- which((substr(y$tow.id,2,2) == "C") & (y$valid == 1) & substr(y$tow.id,3,5) %in% substr(tmp$tow.id, 3, 5))
y$groundfish.sample[index] <- 1
index <- which((substr(y$tow.id,2,2) != "C") & (y$valid == 1) & substr(y$tow.id,3,5) %in% substr(tmp$tow.id, 3, 5))
index <- setdiff(index, which(substr(y$tow.id,3,5) %in% substr(y$tow.id,3,5)[y$groundfish.sample == 1]))
y$groundfish.sample[index] <- 1

# Remove irrelevant variables:
vars <- c("year", "month", "day", "zone", "tow.number", "tow.id", 
          "start.time.logbook", "end.time.logbook", "start.time", "end.time", "valid",
          "longitude", "latitude", "longitude.start.logbook", "longitude.end.logbook", 
          "latitude.start.logbook", "latitude.end.logbook",
          "depth", "bottom.temperature", "warp", "swept.area", "swept.area.method", "groundfish.sample", "water.sample", "comment")
y <- y[vars]    

class(y) <- c("scset", "gulf.set", "ascii", "data.frame")
attr(y, "converted") <- TRUE
                         
# Import touchdown times:
flags <- read.csv("U:/Snow Crab/Stock Assessment 2019/Touchdown and Swept Area Results/Marcel Touchdown Flags 2019.csv", stringsAsFactors = FALSE)
tmp <- read.csv("U:/Snow Crab/Stock Assessment 2019/Touchdown and Swept Area Results/Touchdown and Liftoff - Headline 2019.csv", stringsAsFactors = FALSE)
touchdown.headline <- tmp$touchdown
names(touchdown.headline) <- tmp$tow.id
v <- touchdown.headline[setdiff(names(touchdown.headline), flags$tow.id[flags$source == "logbook"])]
y$start.time[match(names(v), y$tow.id)] <- as.character(v)
y$start.time[match(flags$tow.id[flags$source == "logbook"], y$tow.id)] <- y$start.time.logbook[match(flags$tow.id[flags$source == "logbook"], y$tow.id)]
y$start.time[y$tow.id == "GC309F"] <- "11:21:30"
y$start.time[y$tow.id == "GC272F"] <- "06:59:01"

# Import swept area method:
method <- read.csv("U:/Snow Crab/Stock Assessment 2019/Touchdown and Swept Area Results/Marcel Swept Area Average Flags 2019.csv", stringsAsFactors = FALSE)
y$swept.area.method[y$valid == 1] <- "model"
y$swept.area.method[y$tow.id %in% method$tow.id] <- "average"

# Update longitude-latitude from eSonar files:
for (i in 1:nrow(y)){
   cat(paste0(i, ") ", y$tow.id[i], "\n"))
   if (y$valid[i] == 1){
      e <- read.esonar(year = year, tow.id = y$tow.id[i])
      if (!is.null(e)){
         e$time <- time2min(time(e), start.time(y[i, ]))
         stop.time <- time2min(end.time(y[i, ]), start.time(y[i, ]))
         index <- (e$time >= 0) & (e$time <= stop.time)
         y$longitude[i] <- mean(e$longitude[index], na.rm = TRUE)
         y$latitude[i] <- mean(e$latitude[index], na.rm = TRUE)
      }
   }
}
                
# Bottom temperatures:
tmp <- y$bottom.temperature
y$bottom.temperature <- NA
source("C:/gulf package/gulf/R/star.oddi.path.str.R")
source("C:/gulf package/gulf/R/star.oddi.file.str.R")
source("C:/gulf package/gulf/R/read.star.oddi.R")
for (i in 1:nrow(y)){
   cat(paste0(i, ") ", y$tow.id[i], "\n"))
   if (y$valid[i] == 1){
      h <- read.star.oddi(year = year, tow.id = y$tow.id[i], type = "depth") 
      if (!is.null(h)){
         h$time <- time2min(time(h), start.time(y[i, ]))
         stop.time <- time2min(end.time(y[i, ]), start.time(y[i, ]))
         y$bottom.temperature[i] <- mean(h$temperature[h$time > (stop.time-2) & h$time <= stop.time], na.rm = TRUE)
      }
   }
}
y$bottom.temperature <- round(y$bottom.temperature, 2)
#y$bottom.temperature[y$valid == 1 & is.na(y$bottom.temperature)] <- tmp[y$valid == 1 & is.na(y$bottom.temperature)]

# Import swept area estimates:
z <- read.table("U:/Snow Crab/Stock Assessment 2019/Touchdown and Swept Area Results/Swept Area Estimates 2019.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
y$swept.area <- z$swept.area[match(y$tow.id, z$tow.id)]

y$swept.area[y$tow.id == "GP244F"] <- round(3033.5)
y$swept.area[y$tow.id == "GP003F"] <- round(2920.7)
y$swept.area[y$tow.id == "GP165F"] <- round(3397.3)

write(y, file = "W:/Crab/Offshore Crab Common/Fishing Year 2019/Trawl Data/South Western Gulf/Tow Data/Tows 2019.txt")
       