library(gulf.data)
library(jpeg)

year <- 2020
labels <- TRUE

# Get photo file names and paths:
files <- locate(keywords = "photo", file = "^GP")

# Load tow data:
x <- read.scsset(year = year, valid = 1)

# Fix fishing zones:
z <- fishing.zone(longitude(x), latitude(x), species = 2526)
x$zone[which(z != x$zone)] <- z[which(z != x$zone)]
x$zone[is.na(z)] <- "buffer"

# Define image width in degrees:
thin <- 8 # Image thinning factor (large numbers = more thinning, 1 = no thinning)

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
windows(width = 13, height = 11)
gulf.map(sea = TRUE, land = FALSE, xlim = c(-66, -60), ylim = c(45+40/60, 49 + 8/60))
coastline(border = "black" )
text(-65.35, 48.65272, "QC", cex = 1.5)
text(-65.5, 46.84847, "NB / N.-B.", cex = 1.5)
text(-63.65, 45.75880, "NS / N.-?.", cex = 1.5)
text(-63.22722, 46.33590, "PE / ?.-P.-?.", cex = 1.25)
mtext(paste0("Snow Crab Survey ", year), 3, 2.5, cex = 1.65)
mtext(paste0("Relev? du crabe des neiges en ", year), 3, 1.15, cex = 1.65)

# Loop over all grids:
crop <- 0.05
for (i in 1:length(mif)){
   index <- which(as.numeric(names(mif)[i]) == as.numeric(substr(x$tow.id, 3, 5)))

   if (length(index) > 1) cat(paste0("Grid number ", i, " has multiple tows: '", paste(x$tow.id[index], collapse = "', '"), "' inside it.\n"))
   if (length(index) == 0) cat(paste0("Grid number ", i, " has no tows inside it.\n"))
   if (length(index) == 1){
      # Get photo file name:
      file <- files[grep(x$tow.id[index], files)]

      if (length(file) == 1){
         # Load photo:
         p <- readJPEG(file, native = FALSE)

         # Thin-out image:
         if (prod(dim(p)[1:2]) < 10000000){
            # Photos taken via iPad:
            p <- p[seq(1, nrow(p), by = 5), seq(1, ncol(p), by = 5), ]
            tmp <- array(NA, dim = c(dim(p)[2], dim(p)[1], dim(p)[3]))
            for (j in 1:3) tmp[,,j] <- t(p[,,j])
            p <- tmp
         }else{
            p <- p[seq(1, nrow(p), by = thin), seq(1, ncol(p), by = thin), ]
         }

         # Crop image border:
         p <- p[round(crop * dim(p)[1]):round((1-crop) * dim(p)[1]), round(crop * dim(p)[2]):round((1-crop) * dim(p)[2]), ]

         # Display image:
         rx <- c(min(mif[[i]]$x), max(mif[[i]]$x))
         ry <- c(min(mif[[i]]$y), max(mif[[i]]$y))
         rasterImage(p, rx[1], ry[1], rx[2], ry[2])

         # Add tow ID label:
         if (labels){
            str <- paste0("", as.numeric(substr(x$tow.id[index], 3, 5)))
            text(rx[1] + 0.5 * diff(rx), ry[1] + 0.85 * diff(ry), str, col = "grey70", font = 2, cex = 0.25)
         }
         mif[[i]]$zone <- x$zone[index]
         ii <- match(mif[[i]]$zone, c("12", "E", "F", "19", "buffer"))
         cols <- c("blue", "red", "green", "yellow", "grey")
         lines(mif[[i]]$x, mif[[i]]$y, lwd = 0.9, col = cols[ii])
      }
   }
}
wind.rose()
box()
legend("bottomleft",
       legend = c("Area 12 / Zone 12", "Area 12E / Zone 12E", "Area 12F / Zone 12F", "Area 19 / Zone 19", "Buffer Zone / Zone Tampon"),
       pch = 22, pt.bg = "cornsilk", col = cols, lwd = NA, pt.cex = 3, pt.lwd = 3,
       bg = "cornsilk", cex = 1.25)

