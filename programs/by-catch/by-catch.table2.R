library(gulf.data)
library(gulf.graphics)

species <- c("Atlantic cod", "American plaice", "Brittle star", "Sea urchin", "Lesser toad crab", "Greater toad crab")

r <- NULL
for (i in 1:length(species)){
   # Read set and catch data:
   x <- read.scsset(2016:2020, valid = 1, survey = "regular")
   y <- read.scscat(2016:2020, species = species(species[i])[1])

   #y <- read.scscat(2016:2020, species = species("sea urchin"))

   # Import catches into set data:
   vars <- c("number.caught", "weight.caught")
   import(x, var = vars, fill = 0) <- y

   # Standarize units:
   x$weight.caught[year(x) <= 2017] <- x$weight.caught[year(x) <= 2017] / 1000 # Convert to kg.

   # Standardize by swept area:
   x[c("number.per.km2", "kg.per.km2")] <- 1000000 * x[vars] / repvec(x$swept.area, ncol = length(vars))
   x$species <- species(unique(y$species))

   # Calculate trawlable abundance and biomass:
   r <- rbind(r, aggregate(list(number.per.tow = x$number.caught, weight.per.tow = x$weight.caught), by = list(year = year(x), species = x$species), mean))
}
r$number.per.tow[r$species == "Brittle star"] <- NA
r[c("number.per.tow", "weight.per.tow")]  <- round(r[c("number.per.tow", "weight.per.tow")], 2)
