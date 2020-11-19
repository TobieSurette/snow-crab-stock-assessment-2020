library(gulf.data)
library(gulf.graphics)

# Read set and catch data:
x <- read.scsset(2015:2020, valid = 1, survey = "regular")
y <- read.scscat(2015:2020, species = c(2527))

# Import catches into set data:
vars <- c("number.caught", "weight.caught")
import(x, var = vars, fill = 0) <- y

# Standarize units:
ix <- year(x) <= 2017
x$weight.caught[ix] <- x$weight.caught[ix] / 1000 # Convert to kg.

# Standardize by swept area:
x[c("number.per.km2", "kg.per.km2")] <- 1000000 * x[vars] / repvec(x$swept.area, ncol = length(vars))

# Calculate trawlable abundance and biomass:
r <- aggregate(list(number.per.tow = x$number.caught, weight.per.tow = x$weight.caught), by = list(year = year(x)), mean)
r[c("number.per.km2", "kg.per.km2")] <- aggregate(x[c("number.per.km2", "kg.per.km2")], by = list(year = year(x)), mean)[, 2:3]
r[c("abundance(millions)", "biomass(MT)")] <- 57842.8 * r[c("number.per.km2", "kg.per.km2")]
r$"abundance(millions)" <- r$"abundance(millions)" / 1000000
r$"biomass(MT)" <- r$"biomass(MT)" / 1000
r[, -1] <- round(r[, -1], 1)

boxplot(I(1000 * x$weight.caught / x$number.caught) ~ year(x), ylim = c(0, 175),
        xlab = "Year", ylab = "Individual mean weight (g)")
