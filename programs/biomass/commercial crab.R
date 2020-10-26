library(gulf.data)
library(gulf.stats)
library(gulf.spatial)

year <- 2020 # Survey year.

# Read data:
s <- read.scsset(year = year, survey = "regular", valid = 1) # Tow data.
b <- read.scsbio(year = year, survey = "regular")            # Biolgical data.

# Define catch categories:
categories <- c("COM", "COMSC12", "COMSC345")
import(s, fill = 0) <- catch(b, category = categories, weight = TRUE, hard.shelled = TRUE, units = "t") # Merge catches.
s[categories] <- 1000000 * s[categories] / repvec(s$swept.area, ncol = length(categories))   # Convert to tonnes per km2.

# Read previous two years for variogram averaging:
ss <- read.scsset(year = (year-2):(year-1), survey = "regular", valid = 1)
bb <- read.scsbio(year = (year-2):(year-1), survey = "regular")
import(ss, fill = 0) <- catch(bb, category = categories, weight = TRUE, hard.shelled = TRUE, units = "t")
ss[categories] <- 1000000 * ss[categories] / repvec(ss$swept.area, ncol = length(categories))

# Determine kriging coordinates:

# Merge the three years together:
s[setdiff(names(ss), names(s))] <- NA
ss[setdiff(names(s), names(ss))] <- NA
s <- rbind(ss[names(s)], s)

# Perform kriging:
m <- ked(s, variables = categories, variogram.average = 3, grid = c(100, 100), bug = FALSE, max.distance = 150)

# Load kriging polygons:
p <- locate.gulf.spatial(keywords = c("kriging", "polygons"))
p <- p[c("gulf", "zone12", "zone19", "zoneE", "zoneF", "zoneEF_unassigned", "zone19_F_buffer", "zone19_12_buffer",
         "static_closure_2018", "static_closure_inside_2018")]

# Calculate abundance or biomass:
res <- summary.ked(m, polygon = p)


