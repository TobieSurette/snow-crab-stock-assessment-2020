library(gulf)

source("C:/gulf package/gulf/R/summary.scset.R")
source("C:/gulf package/gulf/R/ked.scset.R")

load("C:/gulf package/gulf/data/kriging.polygons.revised.rda")
p <- kriging.polygons
p <- p["gulf"]

var <-  "MIGE56" #"COMSC345" # "MIGE56"   "COM"
output <- "pdf" # jpeg 

# Uncorrected variables:
res <- list()
m <- list()
years <- 2019:2019
for (i in 1:length(years)){
   # Perform kriging with external drift using three-year variogram averaging:
   if ((var == "COM") | ((var == "COMSC345") & (years[i] != 2018))){
      m[[i]] <- ked.scset(year = years[i], variables = var, grid = c(100, 100), weight = TRUE, hard.shelled = TRUE, variogram.average = 3, units = "t", bug = FALSE)
   }
   if (var == "MIGE56") m[[i]] <- ked.scset(year = years[i], variables = var, grid = c(100, 100), variogram.average = 3, bug = FALSE)
   if ((var == "COMSC345") & (years[i] == 2018)){
      # Shell condition 2 corrective probabilities by survey week for 2018:
      pnew <- c(0.33, 0.20, 0.02, 0.04, 0.02, 0.11, 0.40, 0.04, 0.60)
      names(pnew) <- 1:length(pnew)

      s <- read.scset(year = 2018, valid = 1)
      s$week <- week(date(s))
      s$week <- s$week - min(s$week) + 1

      s <- summary(s, category = c("COMSC2", "COMSC345"), weight = TRUE, hard.shelled = TRUE, units = "t")
      s$COMSC345 <- s$COMSC345 + s$COMSC2 * (1-pnew[s$week]) 
      s$COMSC345 <- 1000000 * s$COMSC345 / s$swept.area

      # Add 2016 and 2017 for variogram averaging:
      ss <- read.scset(year = 2016:2017, valid = 1)
      ss <- summary(ss, category = c("COMSC345"), weight = TRUE, hard.shelled = TRUE, units = "t")
      ss$COMSC345 <- 1000000 * ss$COMSC345 / s$swept.area
      s <- rbind(ss, s[names(ss)])

      m[[i]] <- ked.scset(s, variables = "COMSC345", variogram.average = 3, grid = c(100, 100), bug = FALSE, max.distance = 150)
   }
      
   res[[i]] <- summary.ked(m[[i]], polygon = p)
}
names(res) <- years

# Display density maps:
library(akima)

# Prepare plot:
k <- matrix(1, ncol = 8, nrow = 7)
#k <- kronecker(matrix(1:9, ncol = 3, byrow = TRUE), matrix(1, nrow = 3, ncol = 5))
k <- rbind(0, cbind(0, k, 0), 0)

if (output == "pdf") grDevices::pdf(file = paste0("U:/Snow Crab/Stock Assessment 2019/Density Map ", var, " ", unique(years), ".pdf"), width = 11, height = 8.5)
if (output %in% c("jpg", "jpeg")){
   if (length(years) == 1) str <- years else str <-  paste0(min(years), "-", max(years))
   jpeg(file = paste0("U:/Snow Crab/Stock Assessment 2019/Density Map ", var, " ", str, ".jpg"), width = 7 * 480, height = 7 * 480, res = 7 * 75)
}
          
if (is.null(dev.list())) windows(height = 8, width = 11.5)

layout(k)
par(mar = c(0,0,0,0))

xlim <- c(-66.5, -60-1/6)
ylim <- c(45.5-1/6, 48.5+5/6)
ratio <- distance(xlim[1], ylim[1], xlim[1], ylim[2]) / distance(xlim[1], ylim[1], xlim[2], ylim[1])
width <- 9
#windows(height = ratio * width, width = width);
plot(xlim, ylim, type = "n", xlab = "", ylab = "", xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n")

bathymetry(sea = TRUE)

i = 1  
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
map.fishing.zones(species = 2526, lwd = 1)
coastline(col = "grey80", border = "grey40", lwd = 1)
box(lwd = 1)

coastline()
      
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
 
 
 
 
 
 
 
 