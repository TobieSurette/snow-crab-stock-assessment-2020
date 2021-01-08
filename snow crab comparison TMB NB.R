library(gulf.data)
library(gulf.graphics)
library(gulf.spatial)
library(glmmTMB)

format <- "pdf"
years <- 2006:2020
var <- c("FI", "FM", "MI")

# Read survey grids:
mif <- scs.survey.grids()
for (i in 1:length(mif)){
   tmp <- km2deg(mif[[i]]$x, mif[[i]]$y)
   mif[[i]]$longitude <- tmp[,1]
   mif[[i]]$latitude <- tmp[,2]
}

clg()
k <- kronecker(matrix(1:(2*length(var)), ncol = 2, byrow = TRUE), matrix(1, ncol = 5, nrow = 5))
k <- rbind(0,cbind(0,0,k,0,0,0),0,0)
layout(k)
par(mar = c(0,0,0,0))

for (j in 1:length(var)){
   x <- read.scsset(years, valid = 1, survey = "regular")
   y <- read.scsbio(years, survey = "regular")
   y$tow.id <- tow.id(y)
   import(x, var = var[j], fill = 0) <- catch(y, category = var[j])
   x$grid <- scs.survey.grid(x)

   # Fix NA grids:
   ix <- which(is.na(x$grid))
   fun <- function(x) return(c(mean(x$longitude[1:4]), mean(x$latitude[1:4])))
   centroids <- lapply(mif, fun)
   lon <- unlist(lapply(centroids, function(x) x[1]))
   lat <- unlist(lapply(centroids, function(x) x[2]))
   d <- distance(lon(x[ix,]), lat(x[ix,]), lon, lat)
   x$grid[ix] <- apply(d, 1, which.min)

   # Prepare data for model:
   x$n <- x[, var[j]]
   x$year<- as.factor(year(x))
   x$grid <- as.factor(x$grid)

   #m <- glmmTMB(n ~ (1|year) + (1| grid) + offset(log(swept.area) - log(1000000)), data = x, family = nbinom2)

   m <- list()
   for (i in 1:length(years)){
      print(years[i])
      m[[i]] <- glmmTMB(n ~ (1| grid) + offset(log(swept.area) - log(1000000)),
                        data = x[year(x) == years[i], ],
                        family = nbinom1,
                        zi = ~1)
   }

   fun <- function(x) summary(x)$coef$cond[1]

   mu <- function(x) return(mean(coef(x)[[1]]$grid[,1]))
   sigma <- function(x){
      z <- coef(x)[[1]]$grid[,1]
      return(sd(z) / sqrt(length(z)))
   }

   v <- unlist(lapply(m, mu))
   names(v) <- years
   s <- unlist(lapply(m, sigma))
   names(s) <- years
   r <- aggregate(1000000* x$n / x$swept.area, by = x["year"], mean)[, 2]
   names(r) <- years

   gbarplot(exp(v), years, xaxt = "n", grid = TRUE, )
   lines(years, 0.8 * r , col = "blue", lwd = 2)
   error.bar(years, lower = exp(v - 1.96 * s), upper = exp(v + 1.96 * s))
   if (j == 3) axis(1, las = 2)
   mtext(category(var[j]), 2, 2.5, cex = 1.0)
   if (j == 1) mtext("Density(#/km2)", 3, 0.5, cex = 1.0)

   delta <- diff(v)
   ds <- sqrt(s[-1]^2 + s[-length(s)] ^2)
   gbarplot(100*(exp(delta)-1), years[-length(years)], xaxt = "n", yaxt = "n", ylim = c(-75, 75), grid = TRUE, yaxs = "i")
   hline(0, col = "red")
   error.bar(years[-length(s)], lower = 100*(exp(delta - 1.96 * ds)-1), upper = 100*(exp(delta + 1.96 * ds)-1))
   axis(4)
   if (j == 3) axis(1, at = years[-length(years)], labels = paste0(years[-length(years)], "-", years[-1]), las = 2)
   if (j == 2) mtext("Percentage", 4, 2.75, cex = 1.0)
   if (j == 1) mtext("Year-to-year increase (%)", 3, 0.5, cex = 1.0)
}

