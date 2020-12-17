library(gulf.data)
library(gulf.stats)
library(gulf.graphics)
library(gulf.spatial)

# Survey year:
year <- 2020
output <- "results/tables/"
categories <- c("COM", "COMSC12", "COMSC345") # Define catch categories.

# Load kriging polygons:
p <- read.gulf.spatial("kriging polygons revised")
p <- p[c("gulf", "zone12_expanded", "zone12", "zone19", "zoneE", "zoneF", "zoneEF_unassigned", "zone19_F_buffer", "zone19_12_buffer")]

# Read three years of data (for variogram averaging):
s <- read.scsset(year = (year-2):year, survey = "regular", valid = 1) # Tow data.
b <- read.scsbio(year = (year-2):year, survey = "regular")            # Biological data.

# Import catch data:
import(s, fill = 0) <- catch(b, category = categories, weight = TRUE, hard.shelled = TRUE, units = "t") # Merge catches.
s[categories] <- 1000000 * s[categories] / repvec(s$swept.area, ncol = length(categories))   # Convert to tonnes per km2.

# Perform kriging:
m <- ked(s, variables = categories, variogram.average = 3, lag = 3, max.distance = 75)

# Calculate abundance or biomass:
res <- summary(m, polygon = p)

# Add description:
res <- cbind(res["variable"],
             data.frame(description = category(res$variable, language = "english")),
             res[setdiff(names(res), "variable")])

# Output:
vars <- c("area", "mean.sample", "sd.sample", "mean", "sd", "lowerCI", "upperCI", "mean.CV", "mad.CV", "sd.CV")
res[vars] <- round(res[vars], 1)

# English:
write.table(res, file = paste0(output, "english/", "commercial.biomass.", year, ".csv"), row.names = FALSE, sep = ",")

# French:
names <- names(res)
names <- gsub("polygon", "polygone", names)
names <- gsub("mean", "moyenne", names)
names <- gsub("sample", "echantillon", names)
names <- gsub("sd", "ecart.type", names)
names <- gsub("mad", "deviation.absolue", names)
names <- gsub("lowerCI", "int.conf.bas", names)
names <- gsub("upperCI", "int.conf.haut", names)
names(res) <- names
res$description <- category(res$variable, language = "french")
write.table(res, file = paste0(output, "français/", "biomasse.commerciale.", year, ".csv"), row.names = FALSE, sep = ",")
