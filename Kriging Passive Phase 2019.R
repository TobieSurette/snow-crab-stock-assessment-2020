library(gulf)

source("C:/gulf package/gulf/R/summary.scset.R")
source("C:/gulf package/gulf/R/ked.scset.R")

load("C:/gulf package/gulf/data/kriging.polygons.revised.rda")
p <- kriging.polygons
poly.str <- c("gulf", "zone12", "zone19", "zoneE", "zoneF", "zoneEF_unassigned", "zone19_F_buffer", "zone19_12_buffer", "static_closure_2018", "static_closure_inside_2018")
p <- p[poly.str]

s <- read.scset(year = 2017:2019, valid = 1)
# s$swept.area <- swept.area
s <- s[substr(s$tow.id, 2, 2) != "C", ]

vars <- c("COM", "COMSC12", "COMSC345")
s <- summary(s, category = vars, weight = TRUE, hard.shelled = TRUE, units = "t")

s$station <- substr(s$tow.id, 3, 5) 

# Load passive swept areas:
tmp <- read.csv("U:/Snow Crab/Stock Assessment 2019/Touchdown and Swept Area Results/Passive Swept Area 2017-2019.csv", header = TRUE, stringsAsFactors = TRUE)
tmp$station <- substr(tmp$tow.id, 3, 5) 
index <- match(s[c("year", "station")], tmp[c("year", "station")])
s$passive.swept.area <- tmp$passive.swept.area[index]
s$swept.area <- s$swept.area + s$passive.swept.area

s[vars] <- 1000000 * s[vars] / repvec(s$swept.area, ncol = length(vars))

m <- ked.scset(s, variables = vars, variogram.average = 3, grid = c(100, 100), bug = FALSE, max.distance = 150)
m2 <- ked.scset(s[s$year %in% 2017:2018, ], variables = vars, variogram.average = 2, grid = c(100, 100), bug = FALSE, max.distance = 150)
m3 <- ked.scset(s[s$year %in% 2017, ], variables = vars, variogram.average = 1, grid = c(100, 100), bug = FALSE, max.distance = 150)

# Corrected results:
res <- summary.ked(m, polygon = p["gulf"])
res2 <- summary.ked(m2, polygon = p["gulf"])
res3 <- summary.ked(m3, polygon = p["gulf"])

 