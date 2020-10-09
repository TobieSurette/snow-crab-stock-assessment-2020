source("C:/gulf package/gulf/R/summary.scbio.R")
source("C:/gulf package/gulf/R/summary.scset.R")

s <- read.scset(year = 2018, valid = 1)
s <- s[substr(s$tow.id, 2, 2) != "C", ]
vars <- c("F", "FM", "M", "MI", "MM", "COM", "COMSC12", "COMSC345")
s <- summary(s, category = vars)
s[vars] <- 1000000 * s[vars] / repvec(s$swept.area, ncol = length(vars))

clg()
scale <- 0.025
gulf.map(land = FALSE, sea = TRUE)
map.fishing.zones(species = 2526)
coastline()
v <- s[, "COM"]
index <- v > 0
points(longitude(s)[index], latitude(s)[index], cex = scale * sqrt(v[index]), pch = 21, bg = "grey")
points(longitude(s)[!index], latitude(s)[!index], cex = 1, pch = "x")
mtext(2018, 3, 1, cex = 1.5)
box()

values <- pretty(v)
legend("bottomleft", legend = values, pt.cex = c(1, scale * sqrt(values[-1])), pch = c(4, rep(21, length(values)-1)), pt.bg = "grey", bg = "white")


s <- read.scset(year = 2019, valid = 1)
s <- s[substr(s$tow.id, 2, 2) != "C", ]
s <- summary(s, category = vars)
s[vars] <- 1000000 * s[vars] / repvec(s$swept.area, ncol = length(vars))

windows()
gulf.map(land = FALSE, sea = TRUE)
map.fishing.zones(species = 2526)
coastline()
v <- s[, "COM"]
index <- v > 0
points(longitude(s)[index], latitude(s)[index], cex = scale * sqrt(v[index]), pch = 21, bg = "grey")
points(longitude(s)[!index], latitude(s)[!index], cex = 1, pch = "x")
mtext(2019, 3, 1, cex = 1.5)
box()
legend("bottomleft", legend = values, pt.cex = c(1, scale * sqrt(values[-1])), pch = c(4, rep(21, length(values)-1)), pt.bg = "grey", bg = "white")


