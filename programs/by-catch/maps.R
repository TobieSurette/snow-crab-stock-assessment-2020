library(gulf.data)
library(gulf.graphics)
library(gulf.spatial)

format <- "pdf"
year <- 2020
fish <- TRUE
if (fish){
   species <- list("American plaice", "redfish unsp", "cod", "Witch flounder", "Thorny skate", "Yellowtail flounder", "white hake", "Halibut")
   species.en <- gsub(" unsp", "", species(unlist(lapply(species(species), function(x) x[1]))))
   species.fr <- gsub(" non spécifié", "", species(unlist(lapply(species(species), function(x) x[1])), language = "fr"))
   species.fr <- paste0(toupper(substr(species.fr, 1, 1)), substr(species.fr, 2, nchar(species.fr)))
   group <- "fish"
}else{
   # Invertebrates:
   species <- list(6300, 6400, 6200, c(6100, 6110:6119, 6121, 6123, 6129, 6135), c(1823, 1827), 6600, 2521, 2527)
   species.en <- c("Basket stars", "Brittle star", "Sea squirts", "Lesser toad crab", "Green sea urchins", "Starfish", "Sea cucumbers", "Greater toad crab")
   species.fr <- c("Cornes de boeuf", "Ophuires", "Tuniciers", "Hyas coarctatus", "Oursin vert", "Étoiles de mer", "Cocombres de mer", "Hyas araneus")
   group <- "invertebrates"
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
   col <- colorRamp(col)(x) / 255
   col <- rgb(col[,1], col[,2], col[,3])
   return(col)
}

clg()
gdevice(format, file = paste0(getwd(), "/results/figures/", year, " - survey.by-catch.", group),  height = 11, width = 8.5)
m <- kronecker(matrix(1:8, ncol = 2), matrix(1, nrow = 5, ncol = 5))
m <- rbind(0,0,cbind(0, 0, m, 0),0,0)
layout(m)
par(mar = c(0,0,0,0))
for (i in 1:length(species)){
   x <- read.scsset(year, valid = 1, survey = "regular")
   y <- read.scscat(year, survey = "regular", species = species[[i]])
   y <- aggregate(y["weight.caught"], by = y[key(x)], sum)
   import(x, var = "weight.caught", fill = 0) <- y

   # Catch density (tonnes / km2):
   x$density <- x$weight.caught / x$swept.area
   if (year > 2017) x$density <- 1000 * x$density

   x$colour <- colour(sqrt(x$density)) # Define catch colours.
   map.new()
   for (j in 1:length(mif)){
      p <- as.polygon(mif[[j]]$longitude, mif[[j]]$latitude)
      index <- which(in.polygon(p, longitude(x), latitude(x)))
      col <- NA
      if (length(index) > 0) col <- x$colour[index]
      plot(p, col = col, border = "black")
   }
   map("coast", col = "floralwhite", border = "saddlebrown", lwd = 0.4)

   legend <- seq(0, sqrt(max(x$density)), len = 6) ^ 2
   legend("bottomleft",
          legend = round(legend,1),
          pch = 22,  pt.cex = 3,
          pt.bg = colour(sqrt(legend), max = max(sqrt(x$density))),
          bg = "white", cex = 1,
          title = expression(paste("mt/km"^"2")))
   text(-61.4, 48.9, species.en[i], cex = 1.1)
   text(-61.4, 48.67, species.fr[i], cex = 1.1)
   box()

   if (i == 4){
      mtext("Longitude", 2, 2.0, cex = 1.25)
      mtext("Latitude", 1, 2.0, cex = 1.25)
      map.axis(1:2)
   }
   if (i == 5) map.axis(3:4)
}
dev.off()
