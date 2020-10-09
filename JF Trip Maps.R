# Do JF's periodic survey map update:
pdf <- TRUE
trip <- 6

# Read survey tows:
#x <- read.table("U:/Snow Crab/Stock Assessment 2019/Tows 2019.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

#names(x) <- tolower(names(x))
#x$day <- as.numeric(substr(x$date, 1, 2))
#x$month <- as.numeric(substr(x$date, 4, 5))
#x$year <- as.numeric(substr(x$date, 7, 10))
#x$tow.id <- toupper(x$gpnumber)

x <-  read.scset(year = 2019)



#x$longitude <- -(dmm2deg(x$gpa_lon_start) + dmm2deg(x$gpa_lon_end))/2
#x$latitude <- (dmm2deg(x$gpa_lat_start) + dmm2deg(x$gpa_lat_end))/2
#x$valid <- as.numeric(x$tow.quality == "Good")

# Identify Trip:
index <- rep(FALSE, nrow(x))
index[c(1, which(diff(julian(date(x))) > 2)+1)] <- TRUE
x$trip <- cumsum(index) 

# Determine which are abandonned tows:
x$id <- substr(x$tow.id, 3, 5) 
tmp <- aggregate(x["valid"], by = x["id"], sum)
tmp$trip <- aggregate(x["trip"], by = x["id"], max)$trip
abandonned <- tmp[which(tmp$valid == 0), c("id", "trip")]
abandonned$id <- as.numeric(abandonned$id)

x <- x[(x$valid == 1) | (as.numeric(substr(x$tow.id, 3, 5)) %in% abandonned$id), ]

if (!is.null(trip)){
   x <- x[x$trip == trip, ]
   abandonned <- abandonned[abandonned$trip == trip, ]
}

tows <- read.table("U:/Snow Crab/Stock Assessment 2019/Survey Station Generation/Snow Crab Survey Stations 2019.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
mif <- read.mif("U:/Snow Crab/Stock Assessment 2018/Survey Station Generation/grids2017.mif", mid = FALSE)
names <- rep(NA, length(mif))
for (i in 1:length(mif)){
   tmp <- km2deg(mif[[i]]$x, mif[[i]]$y)
   mif[[i]]$x <- tmp$longitude
   mif[[i]]$y <- tmp$latitude
   
   index <- which(in.polygon(mif[i], tows$longitude, tows$latitude)[,1])
   names[i] <- as.numeric(substr(tows$tow.id[index], 3, 5))
}
names(mif) <- names
  
# Plot base map:
clg()

if (pdf){
   if (!is.null(trip)) 
      file <- paste0("Snow Crab Survey ", unique(x$year), " - Trip Number ", trip)
   else 
      file <- paste0("Snow Crab Survey ", unique(x$year), " - Total", trip)
   pdf(paste0("U:/Snow Crab/Stock Assessment 2019/", file, ".pdf"), width = 11, height = 8.5)
}else{
   windows(width = 13, height = 11)
}
gulf.map(sea = TRUE, land = FALSE, xlim = c(-66, -60), ylim = c(45+40/60, 49 + 8/60)) 

cols <- c("yellow", "deepskyblue2", "pink", "green", "black") 
for (i in 1:length(mif)){
   index <- which(as.numeric(names(mif)[i]) == as.numeric(substr(x$tow.id, 3, 5)))
   col <- NA
   if (length(index) == 1){
      if (substr(x$tow.id[index], 6, 8) %in% c("F", "FR")) col <- cols[1]
      if (substr(x$tow.id[index], 6, 8) %in% c("A1")) col <- cols[2]
      if (substr(x$tow.id[index], 6, 8) %in% c("A2")) col <- cols[3]
      if (substr(x$tow.id[index], 6, 8) %in% c("A3")) col <- cols[4]
   }
   if (nrow(abandonned) > 0) if (as.numeric(names(mif)[i]) %in% abandonned$id) col <- "black"

   polygon(mif[[i]]$x,  mif[[i]]$y, col = col)
}
map.fishing.zones(species = 2526, border = "black", lwd = 2)
coastline(border = "grey50", col = "grey80")

#if (!is.null(trip)) lines(x$longitude, x$latitude, col = "red", lwd = 2)
points(x$longitude, x$latitude, pch = 21, bg = "red", cex = 1.0)

for (i in 1:length(mif)){
   col <- "black"
   if (is.null(trip) & (as.numeric(names(mif)[i]) %in% abandonned$id)) col <- "white"
   if (!is.null(trip)){
      if (nrow(abandonned) > 0) if ((as.numeric(names(mif)[i]) %in% abandonned$id) & (abandonned$trip == trip)) col = "white"   
   }
   text(mean(mif[[i]]$x[1:4]), mean(mif[[i]]$y[1:4]), as.numeric(names(mif)[i], 3, 5), cex = 0.65, col = col)
}

map.place.names(language = "both", sea = FALSE)
scale.bar(len = 80)
wind.rose(-60.45, 48.25)
legend("topright", 
       legend = c("Primary Valid / Primaire Valide",
                  "First Alternate Valid / Prermière Alternative Valide",
                  "Second Alternate Valid / Deuxième Alternative Valide",
                  "Third Alternate Valid / Troisième Alternative Valide",
                  "Abandonned / Abandonnée"),
       pch = 22, pt.cex = 3, pt.bg = cols, bg = "grey95")

box()
mtext(c("2019 Snow Crab Survey\n", "Relevé du Crab des Neiges 2019"), 3, 1.2, cex = 1.25)
if (pdf) dev.off()

