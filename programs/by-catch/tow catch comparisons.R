library(gulf.data)
library(gulf.graphics)

var <- "weight.caught"
z <- list()
years <- 2013:2019
species <- c(6400)

#6300 # Basketstars
#6400 # Sea urchins
#c(6100, 6110:6119, 6121, 6123, 6129, 6135) # Sea stars
#c(6600, 6611) # Sea cucumbers
#2521 # Lesser
#2527 # Greater
c(2520, 2521, 2527)

for (year in years){
   y <- read.scscat(year:(year+1), species = species, survey = "regular")
   x <- read.scsset(year:(year+1), valid = 1, survey = "regular")
   y <- aggregate(y[c("number.caught", "weight.caught")], by = y[c("date", "tow.number", "tow.id")], sum)

   import(x, fill = 0, var = var) <- y

   if (var == "weight.caught"){
      ix <- which(year(x) < 2018)
      x[ix, var] <- x[ix, var] / 1000
   }

   x$density <- 1000000 * x[, var] / x$swept.area
   r <- matrix(NA, nrow = 355, ncol = 2)
   ix <- year(x) == year
   xx <- x[ix, ]
   r[as.numeric(substr(xx$tow.id, 3, 5)), 1] <- xx$density

   ix <- year(x) == (year + 1)
   xx <- x[ix, ]
   r[as.numeric(substr(xx$tow.id, 3, 5)), 2] <- xx$density

   ix <- !is.na(r[,1]) & !is.na(r[,2]) & r[,1] > 0 & r[,2] > 0
   r <- r[ix, ]
   lr1 <- log(r[,1])
   lr2 <- log(r[,2])

   clg()
   r <- lr2-lr1
   z[[year-years[1]+1]] <- r
   plot(r, ylim = c(-3, 3))
   grid()
   abline(0, 0, col = "blue", lwd = 2)

   hline(mean(r), col = "red")
   hline(mean(r) + 1.96 * sd(r) / sqrt(length(r)), col = "red", lty = "dashed")
   hline(mean(r) - 1.96 * sd(r) / sqrt(length(r)), col = "red", lty = "dashed")

   #print(year)
   #print(round(100  * (exp(mean(r))-1), 1))
   print(round(100 * sum(x[year(x) == year, var] != 0) / sum(year(x) == year),1))
}

# Compile results:
results <- data.frame(mu = unlist(lapply(z, mean)),
                      sigma = unlist(lapply(z, sd)),
                      n = unlist(lapply(z, length)))
results$lci <- results$mu - 1.96 * results$sigma / sqrt(results$n)
results$uci <- results$mu + 1.96 * results$sigma / sqrt(results$n)
results$mu <- 100*(exp(results$mu)-1)
results$lci <- 100*(exp(results$lci)-1)
results$uci <- 100*(exp(results$uci)-1)

ylim <- c(-70, 70)
if (all(species == 40)) ylim <- c(-45, 45)
gbarplot(results$mu, years, grid = TRUE, las = 2, ylim = ylim)
hline(0, col = "red", lwd = 2)
error.bar(years, lower = results$lci, upper = results$uci)
mtext("Year-to-year difference (%)", 2, 2.5, cex = 1.25)
mtext("Initial survey year", 1, 3.5, cex = 1.25)
