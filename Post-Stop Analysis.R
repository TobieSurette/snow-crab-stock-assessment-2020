
p <- 70
w <- 3 * p
v <- 1
d <- function(t) return(2*sqrt(p*(2*p-((3/4)*v*t))))
s <- function(t) return((v*(w-v*t))/(sqrt((w-v*t)^2 - p^2)))
a <- function(t) return(57.2958 * asin(p / (w-v*t)))

t <- seq(0, 140, len = 1000)

plot(t, d(t))
plot(t, s(t))
plot(t, a(t))


# Read tow data:
x <- read.scset(year = 2017, valid = 1)

# Read touchdown and liftoff data:
tmp <- read.csv(file = "U:/Snow Crab/Stock Assessment 2019/Touchdown and Swept Area Results/Touchdown and Liftoff - Tilt 2017.csv", header = TRUE, stringsAsFactors = FALSE)
touchdown <- tmp$touchdown
names(touchdown) <- tmp$tow.id
liftoff <- tmp$liftoff
names(liftoff) <- tmp$tow.id

# Import:
x$start.time <- touchdown[x$tow.id]
x$liftoff.time <- liftoff[x$tow.id]

delta <- as.numeric(difftime(liftoff(x), end.time(x), units = "secs"))

windows(); plot(x$depth, delta)

windows()
gulf.map(sea = TRUE)
v <- delta - mean(delta)
index <- which(v >= 0)
points(longitude(x)[index], latitude(x)[index], pch = 21, bg = "red", cex = 0.18 * sqrt(v[index]))
index <- which(v < 0)
points(longitude(x)[index], latitude(x)[index], pch = 21, bg = "grey", cex = 0.18 * sqrt(-v[index]))

 
windows()
gulf.map(sea = TRUE)
points(longitude(x), latitude(x), pch = 21, bg = "grey", cex = 0.25 * sqrt(delta))


index <- which(v < 0)
points(longitude(x)[index], latitude(x)[index], pch = 21, bg = "grey", cex = 0.18 * sqrt(-v[index]))