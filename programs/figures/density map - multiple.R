library(gulf.data)
library(gulf.stats)
library(gulf.graphics)
library(gulf.spatial)

# Load kriging polygons:
p <- read.gulf.spatial("kriging polygons revised")["gulf"]

var <- "MIGE56" # "COM" # "MIGE56" #"COMSC345" #
output <- "pdf" # jpeg <- TRUE
weight <- FALSE

# Uncorrected variables:
res <- list()
m <- list()
years <- 2010:2020
for (i in 1:length(years)){
   print(years[i])
   # Read three years of data (for variogram averaging):
   s <- read.scsset(year = (years[i]-2):years[i], survey = "regular", valid = 1) # Tow data.
   b <- read.scsbio(year = (years[i]-2):years[i], survey = "regular")            # Biological data.
   b$tow.id <- tow.id(b)

   # Import catch data:
   import(s, fill = 0) <- catch(b, category = var, weight = weight, as.hard.shelled = TRUE, units = "t") # Merge catches.
   s[var] <- 1000000 * s[var] / repvec(s$swept.area, ncol = length(var))   # Convert to tonnes per km2.

   m[[i]] <- ked(s, variables = var, variogram.average = 3)

  # # Perform kriging with external drift using three-year variogram averaging:
#   if ((var == "COM") | ((var == "COMSC345") & (years[i] != 2018))){
#      m[[i]] <- ked.scset(year = years[i], variables = var,  weight = weight, as.hard.shelled = TRUE, variogram.average = 3, units = "t")
#      m <- ked(s, variables = categories, variogram.average = 3, lag = 3, max.distance = 75)
#   }

 #  if (var == "MIGE56") m[[i]] <- ked(year = years[i], variables = var, variogram.average = 3, weight = weight)
 #  if ((var == "COMSC345") & (years[i] == 2018)){
 #     # Shell condition 2 corrective probabilities by survey week for 2018:
 #     pnew <- c(0.33, 0.20, 0.02, 0.04, 0.02, 0.11, 0.40, 0.04, 0.60)
 #     names(pnew) <- 1:length(pnew)#
 #
 #     s <- read.scset(year = 2018, valid = 1)
 #     s$week <- week(date(s))
 #     s$week <- s$week - min(s$week) + 1
 #
 #     s <- summary(s, category = c("COMSC2", "COMSC345"), weight = weight, as.hard.shelled = TRUE, units = "t")
 #      s$COMSC345 <- s$COMSC345 + s$COMSC2 * (1-pnew[s$week])
 #     s$COMSC345 <- 1000000 * s$COMSC345 / s$swept.area

  #    # Add 2016 and 2017 for variogram averaging:
#      ss <- read.scset(year = 2016:2017, valid = 1)
 #     ss <- summary(ss, category = c("COMSC345"), weight = weight, as.hard.shelled = TRUE, units = "t")
 #     ss$COMSC345 <- 1000000 * ss$COMSC345 / s$swept.area
 #     s <- rbind(ss, s[names(ss)])
 #
 #     m[[i]] <- ked.scset(s, variables = "COMSC345", variogram.average = 3, grid = c(100, 100), bug = FALSE, max.distance = 150)
 #  }

   res[[i]] <- summary(m[[i]], polygon = p)
}
names(res) <- years

# Display density maps:
library(akima)

# Prepare plot:
gdevice(output, file = paste0("density.map.", var, ".", years[1], "-", years[length(years)]), height = 9, width = 7.5)
k <- kronecker(matrix(1:12, ncol = 3, byrow = TRUE), matrix(1, nrow = 3, ncol = 5))
k <- rbind(0, cbind(0, k, 0), 0)
layout(k)
par(mar = c(0, 0, 0, 0))
for (i in 1:length(years)){
   if (i %in% c(1))   axis.side <- c(2,3)
   if (i %in% c(10))  axis.side <- c(1,2)
   if (i %in% c(11))  axis.side <- c(1)
   if (i %in% c(4,7)) axis.side <- c(2)
   if (i %in% c(2))   axis.side <- c(3)
   if (i %in% c(3))   axis.side <- c(3,4)
   if (i %in% c(5,8)) axis.side <- NULL
   if (i %in% c(6,9)) axis.side <- c(4)

   # Background map:
   map.new(xlim = c(-66.5, -60-1/6), ylim = c(45.5, 49+1/6))
   map("coast")

   mu <- m[[i]]$map[,,1]
   lon <- m[[i]]$map.longitude
   lat <- m[[i]]$map.latitude
   index <- !is.na(lon) & !is.na(lat) & !is.na(mu)
   mu <- mu[index]
   lon <- lon[index]
   lat <- lat[index]

   xx <- seq(-66.5, -60, len = 400)
   yy <- seq(45, 49, len = 400)
   zz <- interp(x = lon, y = lat, z = mu, xo = xx, yo = yy,  linear = TRUE, extrap = TRUE, duplicate = "mean")$z

   xxx <- repvec(xx, nrow = length(yy))
   yyy <- repvec(yy, ncol = ncol(xxx))
   xxx <- t(xxx)
   yyy <- t(yyy)
   index <- in.polygon(as.polygon(p$gulf$longitude, p$gulf$latitude), xxx, yyy)
   dim(index) <- dim(zz)
   zz[!index] <- NA

   cols <- colorRampPalette(c("blue4", "blue", "mediumturquoise", "yellow", "orange", "red", "darkred"))

   #image(xx, yy, 1000 * zz, add = TRUE, col = cols(100), breaks = c(seq(0, 3000, len = 100), 10000))

   if (var == "COM"){
      breaks = c(seq(0, 3000, by = 300), 10000)
      zz <- 1000 * zz
   }
   if (var == "COMSC345"){
      breaks = c(seq(0, 2000, by = 200), 10000)
      zz <- 1000 * zz
   }
   if (var == "MIGE56"){
      breaks = c(seq(0, 15000, by = 1500), 100000)
   }

   image(xx, yy, zz, add = TRUE, col = cols(length(breaks)-1), breaks = breaks)

   # Fishing zones:
   v <- read.gulf.spatial("fishing zone vertices shp", species = 2526, region = "gulf", label = c("12", "12E", "12F", "19"))
   v <- subset(v, label %in% c("12", "12E", "12F", "19"))
   plot(v, add = TRUE)

   lines(p[[1]]$longitude, p[[1]]$latitude, lwd = 0.5)

   text(-61, 48.7, years[i], cex = 1.4)

   if (length(axis.side) > 0) map.axis(axis.side)

   box(lwd = 1)
}

#colors <- colorRampPalette(c("blue", "yellow", "orange", "red", "darkred"))
str <- paste0(breaks[1:(length(breaks)-1)], " - ", breaks[2:length(breaks)])
str[length(str)] <- paste(breaks[length(breaks)-1], "+")

plot(c(0, 1), c(0, 1), type = "n", axes = FALSE, xlab = "", ylab = "")
legend("center",
       legend = rev(str),
       pt.bg = rev(cols(length(breaks))),
       pch = 22,
       pt.cex = 3,
       cex = 0.95,
       bty = "n",
       title = ifelse(weight, "kg / km2", "# / km2"))

dev.off()
