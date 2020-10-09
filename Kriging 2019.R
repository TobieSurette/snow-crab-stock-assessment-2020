library(gulf)

source("C:/gulf package/gulf/R/summary.scset.R")
source("C:/gulf package/gulf/R/ked.scset.R")

load("C:/gulf package/gulf/data/kriging.polygons.revised.rda")
p <- kriging.polygons
poly.str <- c("gulf", "zone12", "zone19", "zoneE", "zoneF", "zoneEF_unassigned", "zone19_F_buffer", "zone19_12_buffer", "static_closure_2018", "static_closure_inside_2018")
p <- p[poly.str]

s <- read.scset(year = 2019, valid = 1)
# s$swept.area <- swept.area
s <- s[substr(s$tow.id, 2, 2) != "C", ]

vars <- c("COM", "COMSC12", "COMSC345")
s <- summary(s, category = vars, weight = TRUE, hard.shelled = TRUE, units = "t")
s[vars] <- 1000000 * s[vars] / repvec(s$swept.area, ncol = length(vars))

# Add 2016 and 2017 for variogram averaging:
ss <- read.scset(year = 2017:2018, valid = 1)
ss <- summary(ss, category = vars, weight = TRUE, hard.shelled = TRUE, units = "t")
ss[vars] <- 1000000 * ss[vars] / repvec(ss$swept.area, ncol = length(vars))

s <- rbind(ss, s[names(ss)])

m <- ked.scset(s, variables = vars, variogram.average = 3, grid = c(100, 100), bug = FALSE, max.distance = 150)

# Corrected results:
res <- summary.ked(m, polygon = p["gulf"])


 