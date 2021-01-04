library(gulf.data)
library(gulf.graphics)
library(gulf.spatial)

format <- "pdf"
years <- 2013:2020
fish <- TRUE
if (fish){
   species <- list(40, 23, 10, 41, 201, 42, c(12, 35), 30)
   species.en <- c("American plaice", "Redfish", "Atlantic cod", "Witch flounder", "Thorny skate", "Yellowtail flounder", "White hake", "Halibut")
   species.fr <- c("Plie canadienne", "Sébaste", "Morue franche", "Plie grise", "Raie épineuse", "Limande à queue jaune", "Merluche blanche", "Flétan Atlantique")
   group <- "fish"
   max <- c(30, 50, 20, 10, 10, 5, 10, 10)
}else{
   # Invertebrates:
   species <- list(6300, 6400, 6200, c(6100, 6110:6119, 6121, 6123, 6129, 6135), c(1823, 1827), 6600, 2521, 2527)
   species.en <- c("Basket stars", "Green sea urchins", "Brittle star", "Starfish", "Sea squirts", "Sea cucumbers", "Lesser toad crab", "Greater toad crab")
   species.fr <- c("Cornes de boeuf", "Oursin vert", "Ophuires", "Étoiles de mer", "Tuniciers",  "Cocombres de mer", "Hyas coarctatus", "Hyas araneus")
   group <- "invertebrates"
   max <- c(25, 15, 20, 20, 15, 40, 5, 5)
}

# Read survey grids:
mif <- read.gulf.spatial(c("scs", "mif"))
for (i in 1:length(mif)){
  tmp <- km2deg(mif[[i]]$x, mif[[i]]$y)
  mif[[i]]$longitude <- tmp$longitude
  mif[[i]]$latitude <- tmp$latitude
}

# Define function for assigning colours to catches:
colour <- function(x, maximum, col = c("white", "black")){
   if (missing(maximum)) x <- x / base::max(x, na.rm = TRUE)
   if (!missing(maximum)) x <- x / maximum
   x[x > 1] <- 1
   col <- colorRamp(col)(x) / 255
   col <- rgb(col[,1], col[,2], col[,3])
   return(col)
}

for(i in 1:length(species)){
   print(species[i])
   clg()
   gdevice(format, file = paste0(getwd(), "/results/figures/english/maps/by-catch/", species.en[i], ".", min(years),  "-", max(years)), height = 11, width = 8.5)
   m <- kronecker(matrix(1:8, ncol = 2), matrix(1, nrow = 5, ncol = 5))
   m <- rbind(0,0,cbind(0, 0, m, 0),0,0)
   layout(m)
   par(mar = c(0,0,0,0))
   for (j in 1:length(years)){
      print(years[j])
      x <- read.scsset(years[j], valid = 1, survey = "regular")
      y <- read.scscat(years[j], survey = "regular", species = species[[i]])
      y <- aggregate(y["weight.caught"], by = y[key(x)], sum)
      import(x, var = "weight.caught", fill = 0) <- y

      # Catch density (tonnes / km2):
      x$density <- x$weight.caught / x$swept.area
      if (years[j] > 2017) x$density <- 1000 * x$density

      x$colour <- colour(sqrt(x$density), max = sqrt(max[i])) # Define catch colours.
      map.new()
      for (k in 1:length(mif)){
         p <- as.polygon(mif[[k]]$longitude, mif[[k]]$latitude)
         index <- which(in.polygon(p, longitude(x), latitude(x)))
         col <- NA
         if (length(index) > 0) col <- x$colour[index]
         plot(p, col = col, border = "grey60", lwd = 0.4)
      }
      map("coast", col = "floralwhite", border = "saddlebrown", lwd = 0.4)

      # Fishing zones:
      v <- read.gulf.spatial("fishing zone vertices shp", species = 2526, region = "gulf", label = c("12", "12E", "12F", "19"))
      v <- subset(v, label %in% c("12", "12E", "12F", "19"))
      plot(v, add = TRUE)

      # Draw legend:
      legend <- seq(0, sqrt(max[i]), len = 6) ^ 2
      legend("bottomleft",
             legend = round(legend,1),
             pch = 22,  pt.cex = 2.5,
             pt.bg = colour(sqrt(legend), max = sqrt(max[i])),
             bg = "white", cex = 0.75, lwd = 0.4, col = "grey60",
             title = expression(paste("mt/km"^"2")))
      text(-60.9, 49, years[j], cex = 1.25)
      box()

      if (j == 4){
         mtext("Longitude", 2, 2.0, cex = 1.25)
         mtext("Latitude", 1, 2.0, cex = 1.25)
         map.axis(1:2)
      }
      if (j == 5) map.axis(3:4)
   }
   dev.off()
}


