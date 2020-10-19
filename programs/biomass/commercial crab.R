library(gulf.data)
library(gulf.stats)
library(gulf.spatial)

year <- 2020

# Read data:
s <- read.scsset(year = year, valid = 1) # Tow data.
b <- read.scsbio(year = year)            # Catch data.

# Define catch categories:
categories <- c("COM", "COMSC12", "COMSC345")
import(s) <- catch(b, category = categories, weight = TRUE, hard.shelled = TRUE, units = "t") # Merge catches.
s[categories] <- 1000000 * s[categories] / repvec(s$swept.area, ncol = length(categories))   # Convert to tonnes per km2.

# Read previous two years for variogram averaging:
ss <- read.scsset(year = (year-2):(year-1), valid = 1)
ss <- ss[substr(ss$tow.id,2,2) != "C", ]
bb <- read.scsbio(year = (year-2):(year-1))
bb <- bb[substr(bb$tow.id,2,2) != "C", ]
import(ss) <- catch(bb, category = categories, weight = TRUE, hard.shelled = TRUE, units = "t")
ss[categories] <- 1000000 * ss[categories] / repvec(ss$swept.area, ncol = length(categories))

# Merge the three years together:
s <- rbind(ss, s[names(ss)])

# Perform kriging:
m <- ked(s, variables = categories, variogram.average = 3, grid = c(100, 100), bug = FALSE, max.distance = 150)

# Load kriging polygons:
p <- locate.gulf.spatial(keywords = c("kriging", "polygons"))
p <- p[c("gulf", "zone12", "zone19", "zoneE", "zoneF", "zoneEF_unassigned", "zone19_F_buffer", "zone19_12_buffer",
         "static_closure_2018", "static_closure_inside_2018")]

# Calculate abundance or biomass:
res <- summary.ked(m, polygon = p)


