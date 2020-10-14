library(gulf)

# Prepare data export for industry and partners:
year <- 2019

# Read tow data:
s <- read.scset(year = year, valid = 1)
s$longitude <- longitude(s)
s$latitude <- latitude(s)
s <- s[substr(s$tow.id,2,2) != "C", ]
s$end.time <- s$end.time.logbook

# Read biological data:
b <- read.scbio(year = year)
b$weight <- weight(b, hard.shelled = TRUE, units = "kg")
b <- b[substr(b$tow.id,2,2) != "C", ]

# Tabulate results:
res <- aggregate(list(male.legal.count = is.category(b, "MGE95")),  by = b["tow.id"], sum, na.rm = TRUE)
res$commercial.count <- aggregate(list(count = is.category(b, "COM")),  by = b["tow.id"],  sum, na.rm = TRUE)$count
res$commercial.weight <- round(aggregate(list(weight = b$weight * is.category(b, "COM")),  by = b["tow.id"], sum, na.rm = TRUE)$weight, 3)
res$commercial345.count <- aggregate(list(count = is.category(b, "COM SC345")),  by = b["tow.id"],  sum, na.rm = TRUE)$count
res$commercial345.weight <- round(aggregate(list(weight = b$weight * is.category(b, "COM SC345")),  by = b["tow.id"], sum, na.rm = TRUE)$weight, 3) 
res$male.legal.immature.count <- aggregate(list(count = is.category(b, "MIGE95")),  by = b["tow.id"],  sum, na.rm = TRUE)$count
res$male.count <- aggregate(list(count = is.category(b, "TM")),  by = b["tow.id"],  sum, na.rm = TRUE)$count
res$female.count <- aggregate(list(count = is.category(b, "TF")),  by = b["tow.id"],  sum, na.rm = TRUE)$count
res$total.count <- aggregate(list(count = is.category(b, "T")),  by = b["tow.id"],  sum, na.rm = TRUE)$count

# Merge variables:
vars <- setdiff(names(res), "tow.id")
s[vars] <- 0
index <- match(res$tow.id, s$tow.id)
tmp <- res[vars]
s[index, vars] <- tmp

# Convert depth to meters:
s$depth.meters <- round(1.8288 * s$depth, 1)
s$depth.meters <- round(s$depth.meters, 1)

# Define tow type:
s$tow.type <- ""
s$tow.type[substr(s$tow.id, 6, 6) == "F"] <- "Primary"
s$tow.type[substr(s$tow.id, 6, 6) == "F" & substr(s$tow.id, 7, 7) == "R" ] <- "Redone"
s$tow.type[substr(s$tow.id, 6, 6) == "A"] <- "Alternate"
index <- substr(s$tow.id, 6, 6) == "A"
s$tow.type[index] <- paste("Alternate", substr(s$tow.id[index], 7, 7))

s$vessel <- "Avalon Voyager II"

vars <- c("year", "month", "day", "zone", "tow.number", "tow.id", 
          "swept.area", "swept.area.method", "tow.type", 
          "start.time", "end.time", 
          "latitude", "longitude", 
          "depth.meters", "bottom.temperature", "vessel", 
          "commercial.count", "commercial.weight",  
          "commercial345.count", "commercial345.weight", 
          "male.legal.count", "male.legal.immature.count", 
          "male.count", "female.count", "total.count")
          
# Sort data:
s <- sort(s, by = c("year", "month", "day", "tow.number"))          

s <- s[vars]          

write.table(s, file = "U:/Snow Crab/Stock Assessment 2019/file01_snow_crab_survey_summary_2019.txt", sep = "\t", row.names = FALSE)  

# Survey summary document export:
vars <- c("year", "month", "day", "zone", "tow.number", "latitude", "longitude", "swept.area", "depth.meters", 
          "bottom.temperature", "commercial.count", "commercial.weight", "commercial345.count",  
          "commercial345.weight", "tow.type")
tmp <- s[vars]
tmp$depth.meters <- round(tmp$depth.meters)
tmp$commercial.weight <- round(tmp$commercial.weight, 1)
tmp$commercial345.weight <- round(tmp$commercial345.weight, 1)
tmp$tow.type <- gsub("Primary", "P", tmp$tow.type)
tmp$tow.type <- gsub("lternate ", "", tmp$tow.type) 
z <- fishing.zone(tmp$longitude, tmp$latitude, species = 2526)
tmp$zone[which(tmp$zone != z)] <- z[which(tmp$zone != z)]
rownames(tmp) <- NULL

head(tmp)

excel(tmp)

