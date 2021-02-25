library(gulf.data)
library(gulf.stats)
library(gulf.graphics)
library(gulf.spatial)

# Program options:
year <- 2020                                 # Survey year.
output <- "pdf"                              # Output format.
language <- language("fr")                   # Output language.
categories <- c("COM","COMSC12","COMSC345")  # Define catch categories.
#categories <- c("MIGE34L45", "MIGE56L69", "MIGE69L83", "MIGE83", "MIGE83L98", "MIGE83L98SC345", "FI", "FIGNO", "FM", "FP", "FMULT")
categories <- "MIGE56"
weight <- FALSE                              # Whether to convert counts to weights.
path <- paste0("results/figures/", language, "/maps") # File output path.
if (weight & (language == "french"))   path <- paste0(path, "/biomasse/")
if (weight & (language == "english"))  path <- paste0(path, "/biomass/")
if (!weight & (language == "french"))  path <- paste0(path, "/abondance/")
if (!weight & (language == "english")) path <- paste0(path, "/abundance/")

# Load kriging polygons:
p <- read.gulf.spatial("kriging polygons revised")["gulf"]

# Read three years of data (for variogram averaging):
s <- read.scsset(year = (year-2):year, survey = "regular", valid = 1) # Tow data.
b <- read.scsbio(year = (year-2):year, survey = "regular")            # Biological data.

# Import catch data:
import(s, fill = 0) <- catch(b, category = categories, weight = weight, as.hard.shelled = TRUE, units = "t") # Merge catches.
s[categories] <- 1000000 * s[categories] / repvec(s$swept.area, ncol = length(categories))   # Convert to tonnes per km2.

# Perform kriging:
m <- ked(s, variables = categories, variogram.average = 3, lag = 3, max.distance = 75)

# Interpolation grid:
x <- seq(-66.5, -60, len = 400)
y <- seq(45, 49, len = 400)
for (i in 1:length(m$variables)){
   # Prepare output graphics device:
   file <- paste0(year, " - ", tolower(category(m$variables[i], language = language, symbols = FALSE)))
   file <- paste0(path,  file)
   gdevice(output, file = file, width = 11, height = 8.5)

   # Background map:
   map.new(xlim = c(-66.5, -60-1/6), ylim = c(45.5, 49+1/6))
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

   # Define colour palette:
   cols <- colorRampPalette(c("blue4", "blue", "mediumturquoise", "yellow", "orange", "red", "darkred"))

   # Rescale weights:
   if (weight) zz <- 1000 * zz

   # Define contour breaks:
   breaks = c(seq(0, 15000, by = 1500), 100000)
   if (weight & m$variables[i] == "COM")             breaks = c(seq(0, 3000, by = 300), 10000)
   if (!weight & m$variables[i] %in%  c("COM", "COMSC12", "COMSC345")) breaks = c(seq(0, 5000, by = 500), 10000)
   if (weight & (m$variables[i] %in% c("COMSC12", "COMSC345"))) breaks = c(seq(0, 2000, by = 200), 10000)
   if (substr(m$variables[i],1,2) == "MI")           breaks = c(seq(0, 15000, by = 1500), 100000)
   if (substr(m$variables[i],1,1) == "F")            breaks = c(seq(0, 30000, by = 3000), 100000)

   # Density:
   image(x, y, zz, add = TRUE, col = cols(length(breaks)-1), breaks = breaks)

   # Grid lines:
   vline(seq(-66.5, -59, by = 1/6), col = "grey75", lwd = 0.5)
   hline(seq(45, 49.5, by = 1/6), col = "grey75", lwd = 0.5)

   # Fishing zones:
   v <- read.gulf.spatial("fishing zone vertices shp", species = 2526, region = "gulf", label = c("12", "12E", "12F", "19"))
   v <- subset(v, label %in% c("12", "12E", "12F", "19"))
   plot(v, add = TRUE)

   # Coastline:
   coast(col = "grey75")

   # Erase corner:
   plot.grid("GR22", col = "white", border = "white") # Erase corner.
   str <- paste0("GR", 23:59)
   plot.grid(str, label = substr(str,3,4), col = "grey90", cex = 0.70)
   str <- paste0(c(paste0("G", LETTERS), paste0("H", LETTERS)), 22)
   str <- str[which(substr(str, 1, 2) == "GS"):which(substr(str, 1, 2) == "HN")]
   plot.grid(str, label = substr(str,1,2), col = "grey90", cex = 0.70)
   rect(par("usr")[1] + 1/6, par("usr")[3], par("usr")[2], par("usr")[4]-1/6, border = "black", lwd = 1)

   # Legend:
   legend <- paste0(breaks[1:(length(breaks)-1)], " - ", breaks[2:length(breaks)])
   legend[length(legend)] <- paste(breaks[length(breaks)-1], "+")
   if (weight) title <- "kg / km2" else title <- "n / km2"
   legend("bottomleft",
          inset = c(0.035, 0.01),
          legend = rev(legend),
          pt.bg = rev(cols(length(breaks))),
          pch = 22, pt.cex = 3,
          cex = 1.00,  bg = "white", title = title)

   # Draw axes:
   map.axis(c(1,4))

   # Title:
   mtext(category(m$variables[i], language = language, simplify = FALSE), 3, 1.00, cex = 1.25)

   # Info and stats:
   text(par("usr")[1] + 0.9 * diff(par("usr")[1:2]),
        par("usr")[3] + 0.9 * diff(par("usr")[3:4]),
        year, cex = 1.75)

   if (output %in% c("pdf", "jpeg", "jpg")) dev.off()
}

