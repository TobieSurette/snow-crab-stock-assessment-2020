library(gulf.data)
library(gulf.graphics)
library(gulf.spatial)

# Do JF's periodic survey map update:
year <- 2020
format <- pdf
trip <- NULL

# Read tow data:
x <-  read.scsset(year = year)
x$trip <- trip(x)

# Determine which are abandonned tows:
x$id <- substr(x$tow.id, 3, 5)
tmp <- aggregate(x["valid"], by = x["id"], sum)
tmp$trip <- aggregate(x["trip"], by = x["id"], max)$trip
abandonned <- tmp[which(tmp$valid == 0), c("id", "trip")]
abandonned$id <- as.numeric(abandonned$id)

# Keep only valid or abandonned tows:
x <- x[(x$valid == 1) | (as.numeric(substr(x$tow.id, 3, 5)) %in% abandonned$id), ]

# Subset of the specified trip:
if (!is.null(trip)){
   x <- x[x$trip == trip, ]
   abandonned <- abandonned[abandonned$trip == trip, ]
}

# Load survey grids:
grids <- read.gulf.spatial("scs grids")
names <- rep(NA, length(grids))
for (i in 1:length(grids)){
   tmp <- km2deg(grids[[i]]$x, grids[[i]]$y)
   grids[[i]]$x <- tmp$longitude
   grids[[i]]$y <- tmp$latitude
}
names(grids) <- unlist(lapply(grids, function(x) x$tow.id))

map.new()

cols <- c("yellow", "deepskyblue2", "pink", "green", "black")
for (i in 1:length(grids)){
   index <- which(as.numeric(names(grids)[i]) == as.numeric(substr(x$tow.id, 3, 5)))
   grids[[i]]$tow.id == substr(x$tow.id, 3, 5)

   col <- NA
   if (length(index) == 1){
      if (substr(x$tow.id[index], 6, 8) %in% c("F", "FR")) col <- cols[1]
      if (substr(x$tow.id[index], 6, 8) %in% c("A1")) col <- cols[2]
      if (substr(x$tow.id[index], 6, 8) %in% c("A2")) col <- cols[3]
      if (substr(x$tow.id[index], 6, 8) %in% c("A3")) col <- cols[4]
   }
   if (nrow(abandonned) > 0) if (as.numeric(names(grids)[i]) %in% abandonned$id) col <- "black"

   polygon(grids[[i]]$x,  grids[[i]]$y, col = col)
}

coast()

points(lon(x), lat(x), pch = 21, bg = "red", cex = 1.0)

for (i in 1:length(grids)){
   col <- "black"
   if (is.null(trip) & (as.numeric(names(grids)[i]) %in% abandonned$id)) col <- "white"
   if (!is.null(trip)){
      if (nrow(abandonned) > 0) if ((as.numeric(names(grids)[i]) %in% abandonned$id) & (abandonned$trip == trip)) col = "white"
   }
   text(mean(grids[[i]]$x[1:4]), mean(grids[[i]]$y[1:4]), as.numeric(names(grids)[i], 3, 5), cex = 0.65, col = col)
}

#map.place.names(language = "both", sea = FALSE)
#scale.bar(len = 80)
#wind.rose(-60.45, 48.25)
legend("topright",
       legend = c("Primary Valid / Primaire Valide",
                  "First Alternate Valid / Prermière Alternative Valide",
                  "Second Alternate Valid / Deuxième Alternative Valide",
                  "Third Alternate Valid / Troisième Alternative Valide",
                  "Abandonned / Abandonn?e"),
       pch = 22, pt.cex = 3, pt.bg = cols, bg = "grey95")

box()
mtext(c(paste0(year, " Snow Crab Survey\n"),
        paste0("Relevè du Crab des Neiges ", year)),
      3, 1.2, cex = 1.25)
if (pdf) dev.off()

