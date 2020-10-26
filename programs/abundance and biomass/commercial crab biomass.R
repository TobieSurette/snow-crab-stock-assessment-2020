library(gulf.data)
library(gulf.stats)
library(gulf.graphics)
library(gulf.spatial)

# Survey year:
year <- 2020

# Read three years of data (for variogram averaging):
s <- read.scsset(year = (year-2):year, survey = "regular", valid = 1) # Tow data.
b <- read.scsbio(year = (year-2):year, survey = "regular")            # Biological data.

# Import catch data:
categories <- c("COM", "COMSC12", "COMSC345") # Define catch categories.
import(s, fill = 0) <- catch(b, category = categories, weight = TRUE, hard.shelled = TRUE, units = "t") # Merge catches.
s[categories] <- 1000000 * s[categories] / repvec(s$swept.area, ncol = length(categories))   # Convert to tonnes per km2.

# Perform kriging:
m <- ked(s, variables = categories, variogram.average = 3, grid = c(100, 100), max.distance = 75)

# Load kriging polygons:
p <- read.gulf.spatial("kriging polygons revised")
p <- p[c("gulf", "zone12", "zone19", "zoneE", "zoneF", "zoneEF_unassigned", "zone19_F_buffer", "zone19_12_buffer")]

# Calculate abundance or biomass:
res <- summary.ked(m, polygon = p)

# Write results:
write.table(res, file = "results/tables/english/commercial.biomass.2020.csv", row.names = FALSE, sep = ",")

