library(gulf.data)
library(gulf.stats)
library(gulf.graphics)
library(gulf.spatial)

# Survey year:
year <- 2020
output <- "results/tables/"
categories <- c("MIGE34L45",                                           # Instar VIII recruitment.
                "MMGE95SC12", "MMGE95SC3", "MMGE95SC4", "MMGE95SC5",   # Commercial by shell condition.
                "MM", "MMGE95", "MML95",                               # Adult males.
                "FI", "FIGNO", "FM", "FP", "FMULT",                    # Females.
                "MIGE56L69", "MIGE69L83", "MIGE83", "MIGE83L98",       # R-4, R-3, R-2, R-1 fishery recruitment.
                "MIGE83L98SC345")                                      # Skip-moulters.

# Load kriging polygons:
p <- read.gulf.spatial("kriging polygons revised")
p <- p[c("gulf", "zone12", "zone19", "zoneE", "zoneF", "zoneEF_unassigned", "zone19_F_buffer", "zone19_12_buffer")]

# Read three years of data (for variogram averaging):
s <- read.scsset(year = (year-2):year, survey = "regular", valid = 1) # Tow data.
b <- read.scsbio(year = (year-2):year, survey = "regular")            # Biological data.

# Import catch data:
import(s, fill = 0) <- catch(b, category = categories) # Merge catches.
s[categories] <- s[categories] / repvec(s$swept.area, ncol = length(categories)) # Standardize by swept area.

# Perform kriging with external drift:
m <- ked(s, variables = categories, grid = c(100, 100), variogram.average = 3, max.distance = 75)

# Calculate abundance:
res <- summary.ked(m, polygon = p)

# Add description:
res <- cbind(res["variable"],
             data.frame(description = category(res$variable, language = "english")),
             res[setdiff(names(res), "variable")])

# Output:

# English:
write.table(res, file = paste0(output, "english/", "abundance.", year, ".csv"), row.names = FALSE, sep = ",")

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
write.table(res, file = paste0(output, "français/", "abondance.", year, ".csv"), row.names = FALSE, sep = ",")
