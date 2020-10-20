library(gulf.data)
library(gulf.spatial)
library(jpeg)
clg()

year <- 2020
labels <- TRUE
pdf <- TRUE

# Get photo file names and paths:
files <- locate(keywords = "photo", file = "^GP")

# Load tow data:
x <- read.scsset(year = year, valid = 1)

# Fix fishing zones:
#z <- fishing.zone(longitude(x), latitude(x), species = 2526)
#x$zone[which(z != x$zone)] <- z[which(z != x$zone)]
#x$zone[is.na(z)] <- "buffer"

# Define image width in degrees:
thin <- 8 # Image thinning factor (large numbers = more thinning, 1 = no thinning)

# Load survey grid file:
mif <- read.gulf.spatial("grids", survey = "scs")
for (i in 1:length(mif)){
   tmp <- km2deg(mif[[i]]$x, mif[[i]]$y)
   mif[[i]]$x <- tmp$longitude
   mif[[i]]$y <- tmp$latitude
}

# Plot base map:
#dev.new(width = 13, height = 11)
if (pdf){
   pdf(file = paste0("results/figures/survey photo map/survey photo map ", year, ".pdf"), width = 13, height = 11)
}else{
   quartz()
}
plot(c(-66, -60), c(45+40/60, 49 + 8/60), xaxs = "i", yaxs = "i", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
coast()

text(-65.35, 48.65272, "QC", cex = 1.5)
text(-65.5, 46.84847, "NB / N.-B.", cex = 1.5)
text(-63.65, 45.75880, "NS / N.-É.", cex = 1.5)
text(-63.22722, 46.33590, "PE / Î.-P.-É.", cex = 1.25)
mtext(paste0("Snow Crab Survey ", year), 3, 2.5, cex = 1.65)
mtext(paste0("Relevé du crabe des neiges en ", year), 3, 1.05, cex = 1.65)

# Loop over all grids:
crop <- 0.05
for (i in 1:length(mif)){
   j <- which(mif[[i]]$tow.id == as.numeric(substr(x$tow.id, 3, 5)))
   print(x$tow.id[j])
   if (length(j) > 1) cat(paste0("Grid number ", i, " has multiple tows: '", paste(x$tow.id[j], collapse = "', '"), "' inside it.\n"))
   if (length(j) == 0) cat(paste0("Grid number ", i, " has no tows inside it.\n"))
   if (length(j) == 1){
      # Get photo file name:
      file <- files[grep(x$tow.id[j], files)]

      if (length(file) == 1){
         # Load photo:
         p <- readJPEG(file, native = FALSE)

         # Thin-out image:
         if (prod(dim(p)[1:2]) < 10000000){
            # Photos taken via iPad:
            p <- p[seq(1, nrow(p), by = 5), seq(1, ncol(p), by = 5), ]
            tmp <- array(NA, dim = c(dim(p)[2], dim(p)[1], dim(p)[3]))
            for (k in 1:3) tmp[,,k] <- t(p[,,k])
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
         if (labels) text(rx[1] + 0.5 * diff(rx), ry[1] + 0.85 * diff(ry), mif[[i]]$tow.id, col = "grey70", font = 2, cex = 0.25)

         col <- c("blue", "red", "green", "yellow", "grey")[match(x$zone[j], c("12", "E", "F", "19", "buffer"))]
         lines(mif[[i]]$x, mif[[i]]$y, lwd = 0.9, col = col)
      }
   }
}
#wind.rose()
box()
legend("bottomleft",
       legend = c("Area 12 / Zone 12", "Area 12E / Zone 12E", "Area 12F / Zone 12F", "Area 19 / Zone 19", "Buffer Zone / Zone Tampon"),
       pch = 22, col = cols, lwd = NA, pt.cex = 3, pt.lwd = 3,
       bg = "grey90", cex = 1.25)

if (pdf) dev.off()
