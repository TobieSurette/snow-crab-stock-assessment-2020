library(gulf.data)
library(gulf.graphics)
library(gulf.spatial)

var <- "FM"

years <- 2013:2020
x <- read.scsset(years, valid = 1, survey = "regular")
x$grid <- scs.survey.grid(x)
x$station <- station(lon(x), lat(x))

y <- read.scsbio(years, valid = 1, survey = "regular")
y$tow.id <- tow.id(y)
import(x, fill = 0) <- catch(y, category = var)
x$n <- x[, var]

# Summary of 2013 to 2020 changes:
n <- as.numeric(substr(x$tow.id, 7, 7))
n[is.na(n)] <- 0
grid <- matrix(0, nrow = 355, ncol = length(years))
for (i in 1:length(years)){
   ix <- which((year(x) == years[i]) & !is.na(x$grid))
   grid[x$grid[ix], i] <- n[ix]
}
grid <- t(apply(grid, 1, cumsum))

r <- NULL
for (i in 0:16)
r <- cbind(r, t(t(apply(grid, 2, function(x) sum(x == i)))))

# Remove 2013 alternate stations from the analysis:
remove <- x$grid[(year(x) == 2013) & (substr(x$tow.id, 6, 6) == "A")]
x <- x[!(x$grid %in% remove), ]

# Grids which have moved stations in 2020:
moved <- x$grid[x$station %in% setdiff(x$station[year(x) == 2020], x$station[year(x) == 2013])]

x$density <- 1000 * x$n / x$swept.area
x$year <- year(x)

# Compare 2013 and 2020:
x$moved <- x$grid %in% moved
res <- aggregate(x$density[!x$moved], by = x[!x$moved, c("year", "moved")], mean)
res$n <- aggregate(x$density[x$moved], by = x[x$moved, c("year", "moved")], mean)[, 3]

clg()
gbarplot(res$n / res$x, years, grid = TRUE)
mtext("Ratio moved / stationary", 2, 2.5)
mtext("Year", 1, 2.5)
box()

# Read survey grids:
mif <- scs.survey.grids()
for (i in 1:length(mif)){
   tmp <- km2deg(mif[[i]]$x, mif[[i]]$y)
   mif[[i]]$longitude <- tmp[,1]
   mif[[i]]$latitude <- tmp[,2]
}

clg()
xx <- x[year(x) == 2013, ]
map.new()
for (i in 1:nrow(xx)){
   if (x$moved[i]) polygon(mif[[xx$grid[i]]]$longitude, mif[[xx$grid[i]]]$latitude, col = "grey50", border = "grey80")
   if (!x$moved[i]) polygon(mif[[xx$grid[i]]]$longitude, mif[[xx$grid[i]]]$latitude, col = "grey95", border = "grey80")
}
for (i in 1:length(remove)) polygon(mif[[remove[i]]]$longitude, mif[[remove[i]]]$latitude, col = NA, border = "grey80")
map("coast", col = "floralwhite", border = "saddlebrown", lwd = 0.4)
box()
# Fishing zones:
v <- read.gulf.spatial("fishing zone vertices shp", species = 2526, region = "gulf", label = c("12", "12E", "12F", "19"))
v <- subset(v, label %in% c("12", "12E", "12F", "19"))
plot(v, add = TRUE)
map.axis(1:2)


