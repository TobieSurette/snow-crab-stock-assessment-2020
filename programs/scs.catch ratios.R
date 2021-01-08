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
file <- paste0(getwd(), "/results/figures/english/scs.catch ratios.", min(years),  "-", max(years))
gdevice(format, file = file, height = 11, width = 8.5)
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
   #lines(years, 0.8 * r , col = "blue", lwd = 2)
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
dev.off()

# Draw map:
clg()
for (j in 1:length(var)){
   file <- paste0(getwd(), "/results/figures/english/maps/catch ratios ", var[j], ".", min(years),  "-", max(years))
   gdevice(format, file = file, height = 11, width = 8.5)
   m <- kronecker(matrix(1:15, ncol = 3), matrix(1, nrow = 5, ncol = 5))
   m <- rbind(0,0,cbind(0, 0, m, 0),0,0)
   layout(m)
   par(mar = c(0,0,0,0))
   for (i in 1:(length(years)-1)){
      m <- models[[j]][[i]]
      mu <- coef(m)[[1]]$grid[, 1]
      grid <- as.numeric(rownames(coef(m)[[1]]$grid))
      r <- data.frame(grid = 1:355)
      r$x <- NA
      r$x[match(grid, r$grid)] <- mu
      m <- models[[j]][[i+1]]
      mu <- coef(m)[[1]]$grid[, 1]
      grid <- as.numeric(rownames(coef(m)[[1]]$grid))
      r$y <- NA
      r$y[match(grid, r$grid)] <- mu

      map.new()
      cols <- colorRampPalette(c("red", "white", "blue"))(1000)

      for (k in 1:nrow(r)){
         if (!is.na(r$y[k]-r$x[k])){
            p <- km2deg(mif[[k]]$x, mif[[k]]$y)
            p <- as.polygon(p$longitude, p$latitude)

            delta <- r$y[k] - r$x[k]
            delta <- delta / 2.0
            delta[delta <= -1] <- -1
            delta[delta >= 1] <- 1

            scale <- 1
            mx <- mean(p[[1]]$x[1:4])
            my <- mean(p[[1]]$y[1:4])
            p[[1]]$x <- sqrt(scale) * (p[[1]]$x - mx) + mx
            p[[1]]$y <- sqrt(scale) * (p[[1]]$y - my) + my

            plot(p, col = cols[round((delta + 1)/ 2 * length(cols))], border = "grey60", lwd = 0.4)
         }
      }
      map("coast", col = "floralwhite", border = "saddlebrown", lwd = 0.4)
      box()

      # Fishing zones:
      v <- read.gulf.spatial("fishing zone vertices shp", species = 2526, region = "gulf", label = c("12", "12E", "12F", "19"))
      v <- subset(v, label %in% c("12", "12E", "12F", "19"))
      plot(v, add = TRUE)

      # Draw legend:
      rr <- ((log(c(0.2, 0.5, 0.66666, 1, 1.5, 2, 5)) / 2.0)+1)/2
      legend("bottomleft",
             legend = c("5:1", "2:1", "3:2", "1:1", "2:3", "1:2", "1:5"),
             pch = 22,  pt.cex = 2.5,
             pt.bg = cols[round(rr * 1000)],
             bg = "white", cex = 0.75, lwd = 0.4, col = "grey60",
             title = expression(paste("catch ratio(#)")))
      text(-61.1, 48.9, paste0(years[i], "-", (years[i]+1)), cex = 1.25)
      box()

      if (i == 5) map.axis(1:2)
      if (i == 11) map.axis(3:4)
   }
   dev.off()
}


