library(gulf.data)
library(gulf.spatial)

# Prepare data export for industry and partners:
year <- 2020
language <- "french"

# Read tow data:
s <- read.scsset(year = year, valid = 1, survey = "regular")
s$longitude <- lon(s)
s$latitude <- lat(s)
s$stop.time <- s$stop.time.logbook

# Read biological data:
b <- read.scsbio(year = year, survey = "regular")
b$weight <- weight(b, hard.shelled = TRUE, units = "kg")

res <- catch(b, category = c("COM", "COM SC345"), weight = TRUE, hard.shelled = TRUE, units = "kg")
names(res) <- c("date", "tow.id", "commercial.weight", "commercial345.weight")
import(s, fill = 0) <- res
category <- c("COM", "COM SC345", "MIGE95", "M", "F", "T")
names <- c("commercial.count", "commercial345.count", "male.legal.immature.count", "male.count", "female.count", "total.count")
res <- catch(b, category = category)
names(res) <- c("date", "tow.id", names)
import(s, fill = 0) <- res

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

vars <- c("date", "zone", "tow.number", "tow.id",
          "swept.area", "swept.area.method", "tow.type",
          "touchdown.time", "stop.time",
          "latitude", "longitude",
          "depth.meters", "bottom.temperature", "vessel",
          "commercial.count", "commercial.weight",
          "commercial345.count", "commercial345.weight",
          "male.legal.immature.count",
          "male.count", "female.count", "total.count")

# Sort data:
s <- sort(s, by = c("date", "tow.number"))
s <- s[vars]

write.table(s, file = paste0("data/export/snow crab survey summary ", year, ".csv"), sep = ",", row.names = FALSE)

# Survey summary for RAP document:
vars <- c("date", "zone", "tow.number", "latitude", "longitude", "swept.area", "depth.meters",
          "bottom.temperature", "commercial.count", "commercial.weight", "commercial345.count",
          "commercial345.weight", "tow.type")
tmp <- s[vars]
tmp$depth.meters <- round(tmp$depth.meters)
tmp$commercial.weight <- round(tmp$commercial.weight, 1)
tmp$commercial345.weight <- round(tmp$commercial345.weight, 1)
tmp$tow.type <- gsub("Primary", "P", tmp$tow.type)
tmp$tow.type <- gsub("lternate ", "", tmp$tow.type)
write.table(tmp, file = paste0("results/tables/english/scs.tow.summary.", year, ".csv"), sep = ",", row.names = FALSE)

