var <- "FI"

years <- 2013:2020
x <- read.scsset(years, valid = 1, survey = "regular")
x$grid <- scs.survey.grid(x)
x$station <- station(lon(x), lat(x))

y <- read.scsbio(years, valid = 1, survey = "regular")
y$tow.id <- tow.id(y)
import(x, fill = 0) <- catch(y, category = var)
x$n <- x[, var]

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



