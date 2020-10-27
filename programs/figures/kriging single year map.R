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


for (i in 1:length(m$variables)){
   dev.new()

   map.new(xlim = c(-66.5, -60-1/6), ylim = c(45.5-1/6, 48.5+5/6))

   bathymetry()

   xx <- seq(-66.5, -60, len = 400)
   yy <- seq(45, 49, len = 400)
   zz <- akima::interp(x = m$map.longitude[index],
                       y = m$map.latitude[index],
                       z = m$map[,,i][index],
                       xo = xx, yo = yy,  linear = TRUE, extrap = TRUE, duplicate = "mean")$z

   xxx <- repvec(xx, nrow = length(yy))
   yyy <- repvec(yy, ncol = ncol(xxx))
   xxx <- t(xxx)
   yyy <- t(yyy)
   index <- in.polygon(as.polygon(p$gulf$longitude, p$gulf$latitude), xxx, yyy)
   dim(index) <- dim(zz)
   zz[!index] <- NA

   cols <- colorRampPalette(c("blue4", "blue", "mediumturquoise", "yellow", "orange", "red", "darkred"))

   if (weight) zz <- 1000 * zz

   if (m$variables[i] == "COM")      breaks = c(seq(0, 3000, by = 300), 10000)
   if (m$variables[i] == "COMSC12")  breaks = c(seq(0, 2000, by = 200), 10000)
   if (m$variables[i] == "COMSC345") breaks = c(seq(0, 2000, by = 200), 10000)
   if (m$variables[i] == "MIGE56")   breaks = c(seq(0, 15000, by = 1500), 100000)

   image(xx, yy, zz, add = TRUE, col = cols(length(breaks)-1), breaks = breaks)

   #map.fishing.zones(species = 2526, lwd = 1)
   coast()
   box(lwd = 1)
}

xx <- seq(-66.5 + 1/6, -60, by = 1/6)
yy <- seq(45, 49, by = 1/6)
for (i in 1:length(xx)) lines(c(xx[i], xx[i]), c(45, 49+1/6), col = "grey75", lwd = 0.5)
for (i in 1:length(yy)) lines(c(-66.5+1/6, -60), c(yy[i], yy[i]),  col = "grey75", lwd = 0.5)

# Draw fishing zone lines without drawing the shoreline:
data("fishing.zone.polygons")
p <- subset(fishing.zone.polygons, species = 2526)
for (i in 1:length(p)){
   for (j in 1:length(p[[i]])){
      if ("x" %in% names(p[[i]][[j]])){
         xx <- p[[i]][[j]]$x
         yy <- p[[i]][[j]]$y
         dd <- depth(xx, yy)
         index <- which(depth(xx, yy) < -30) # Threshold depth for crab zone lines.
         if (length(index) > 0){
            for (k in 1:length(index)){
               if (index[k] > 1) lines(xx[(index[k]-1):index[k]], yy[(index[k]-1):index[k]], col = "grey10", lwd = 1)
               if (index[k] < length(xx)) lines(xx[index[k]:(index[k]+1)], yy[index[k]:(index[k]+1)], col = "grey10", lwd = 1)
            }
         }
      }
   }
}

# Erase corner:
map.grid("GQ22", col = "white", border = "white")

str <- paste0("GQ", 23:59)
map.grid(str, col = "papayawhip")
tmp <- grid.corners(str)
text(apply(tmp[, c(1,3)], 1, mean), apply(tmp[, c(2,4)], 1, mean), substr(rownames(tmp), 3, 4), cex = 0.85)

str <- paste0(c(paste0("G", LETTERS), paste0("H", LETTERS)), 22)
str <- str[which(substr(str, 1, 2) == "GR"):which(substr(str, 1, 2) == "HN")]
map.grid(str, col = "papayawhip")
tmp <- grid.corners(str)
text(apply(tmp[, c(1,3)], 1, mean), apply(tmp[, c(2,4)], 1, mean), substr(rownames(tmp), 1, 2), cex = 0.85)

rect(par("usr")[1] + 1/6, par("usr")[3], par("usr")[2], par("usr")[4]-1/6, border = "black", lwd = 1)
map.axis(c(1, 4))

#str <- paste0(breaks[1:(length(breaks)-1)], " - ", breaks[2:length(breaks)])
#str[length(str)] <- paste(breaks[length(breaks)-1], "+")

#plot(c(0, 1), c(0, 1), type = "n", axes = FALSE, xlab = "", ylab = "")
#a <- legend("center",
#       legend = rev(str),
#       pt.bg = rev(cols(length(breaks))),
#       pch = 22,
#       pt.cex = 3,
#       cex = 0.95, bty = "n", plot = FALSE)
#legend("center",
#       legend = rev(str),
#       pt.bg = rev(cols(length(breaks))),
#       pch = 22,
#       pt.cex = 3,
#       cex = 0.95, bty = "n")
#
str <- paste0(breaks[1:(length(breaks)-1)], " - ", breaks[2:length(breaks)])
str[length(str)] <- paste(breaks[length(breaks)-1], "+")
#
legend("bottomleft",
       inset = c(0.035, 0.01),
       legend = rev(str),
       pt.bg = rev(cols(length(breaks))),
       pch = 22,
       pt.cex = 4,
       cex = 1.25,  bg = "white", title = "n / km2")

#points(a$rect$left + 0.5 * a$rect$w, a$rect$top - a$rect$h)
#str <- "# / km2"
#if (var %in% c("COM", "COMSC345")) str <- "kg / km2"
#text(a$rect$left + 0.5 * a$rect$w, a$rect$top - a$rect$h + 0.04, str, pos = 1, cex = 1.25)

if (output %in% c("pdf", "jpeg", "jpg")) dev.off()







