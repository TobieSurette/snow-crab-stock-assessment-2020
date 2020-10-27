library(gulf.data)
library(gulf.stats)
library(gulf.graphics)
library(gulf.spatial)

# Survey year:
year <- 2020
output <- "results/tables/"
categories <- c("COM", "COMSC12", "COMSC345") # Define catch categories.
weight <- TRUE

# Load kriging polygons:
p <- read.gulf.spatial("kriging polygons revised")["gulf"]

# Read three years of data (for variogram averaging):
s <- read.scsset(year = (year-2):year, survey = "regular", valid = 1) # Tow data.
b <- read.scsbio(year = (year-2):year, survey = "regular")            # Biological data.

# Import catch data:
import(s, fill = 0) <- catch(b, category = categories, weight = weight, hard.shelled = TRUE, units = "t") # Merge catches.
s[categories] <- 1000000 * s[categories] / repvec(s$swept.area, ncol = length(categories))   # Convert to tonnes per km2.

# Perform kriging:
m <- ked(s, variables = categories, variogram.average = 3, lag = 3, max.distance = 75)

# Interpolation grid:
x <- seq(-66.5, -60, len = 400)
y <- seq(45, 49, len = 400)
for (i in 1:length(m$variables)){
   dev.new()

   # Background map:
   map.new(xlim = c(-66.5, -60-1/6), ylim = c(45.5-1/6, 48.5+5/6))
   bathymetry()

   # Interpolate:
   index <- !is.na(m$map.longitude) & !is.na(m$map.latitude) & !is.na(m$map[,,i])
   zz <- akima::interp(x = m$map.longitude[index],
                       y = m$map.latitude[index],
                       z = m$map[,,i][index],
                       xo = x, yo = y,
                       linear = TRUE, extrap = TRUE, duplicate = "mean")$z

   xx <- t(repvec(x, nrow = length(y)))
   yy <- t(repvec(y, ncol = length(x)))
   index <- in.polygon(as.polygon(p$gulf$longitude, p$gulf$latitude), xx, yy)
   dim(index) <- dim(zz)
   zz[!index] <- NA

   cols <- colorRampPalette(c("blue4", "blue", "mediumturquoise", "yellow", "orange", "red", "darkred"))

   if (weight) zz <- 1000 * zz

   if (m$variables[i] == "COM")      breaks = c(seq(0, 3000, by = 300), 10000)
   if (m$variables[i] == "COMSC12")  breaks = c(seq(0, 2000, by = 200), 10000)
   if (m$variables[i] == "COMSC345") breaks = c(seq(0, 2000, by = 200), 10000)
   if (m$variables[i] == "MIGE56")   breaks = c(seq(0, 15000, by = 1500), 100000)

   # Density:
   image(x, y, zz, add = TRUE, col = cols(length(breaks)-1), breaks = breaks)

   # Grid lines:
   vline(seq(-66.5, -59, by = 1/6), col = "grey75", lwd = 0.5)
   hline(seq(45, 49.5, by = 1/6), col = "grey75", lwd = 0.5)

   # Fishing zones:
   v <- read.gulf.spatial("fishing zone vertices shp", species = 2526, region = "gulf", label = c("12", "12E", "12F", "19"))
   v <- subset(v, label %in% c("12", "12E", "12F", "19"))
   plot(v, add = TRUE)

   # Erase corner:
  # plot.grid("GQ22", col = "white", border = "white")

#   str <- paste0("GQ", 23:59)
 #  plot.grid(str, col = "papayawhip")
 #  tmp <- grid.corners(str)
 #  text(apply(tmp[, c(1,3)], 1, mean), apply(tmp[, c(2,4)], 1, mean), substr(rownames(tmp), 3, 4), cex = 0.85)

 #  str <- paste0(c(paste0("G", LETTERS), paste0("H", LETTERS)), 22)
 #  str <- str[which(substr(str, 1, 2) == "GR"):which(substr(str, 1, 2) == "HN")]
 #  map.grid(str, col = "papayawhip")
 #  tmp <- grid.corners(str)
 #  text(apply(tmp[, c(1,3)], 1, mean), apply(tmp[, c(2,4)], 1, mean), substr(rownames(tmp), 1, 2), cex = 0.85)
 #
 #  rect(par("usr")[1] + 1/6, par("usr")[3], par("usr")[2], par("usr")[4]-1/6, border = "black", lwd = 1)
 #  map.axis(c(1, 4))

   # Legend:
   legend <- paste0(breaks[1:(length(breaks)-1)], " - ", breaks[2:length(breaks)])
   legend[length(legend)] <- paste(breaks[length(breaks)-1], "+")
   if (weight) title <- "kg / km2" else "n / km2"
   legend("bottomleft",
          inset = c(0.035, 0.01),
          legend = rev(legend),
          pt.bg = rev(cols(length(breaks))),
          pch = 22, pt.cex = 4,
          cex = 1.25,  bg = "white", title = title)

   coast()
   box(lwd = 1)

   if (output %in% c("pdf", "jpeg", "jpg")) dev.off()
}

