library(gulf)

source("C:/gulf package/gulf/R/summary.scset.R")
source("C:/gulf package/gulf/R/ked.scset.R")

load("C:/gulf package/gulf/data/kriging.polygons.revised.rda")
p <- kriging.polygons
p <- p["gulf"]

var <- "COM" # "MIGE56" #"COMSC345" # "MIGE56" 
output <- "pdf", # jpeg <- TRUE

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
k <- kronecker(matrix(1:12, ncol = 3, byrow = TRUE), matrix(1, nrow = 3, ncol = 5))
#k <- kronecker(matrix(1:9, ncol = 3, byrow = TRUE), matrix(1, nrow = 3, ncol = 5))
k <- rbind(0, cbind(0, k, 0), 0)

if (output == "pdf") pdf(file = paste0("U:/Snow Crab/Stock Assessment 2019/Density Map ", var, " ", str, ".pdf"), width = 11, height = 8.5)

if (output %in% c("jpg", "jpeg")){
   if (length(years) == 1) str <- years else str <-  paste0(min(years), "-", max(years))
   jpeg(file = paste0("U:/Snow Crab/Stock Assessment 2019/Density Map ", var, " ", str, ".jpg"), width = 7 * 480, height = 7 * 480, res = 7 * 75)
}
          
if (is.null(dev.list())) windows(height = 9, width = 7.5)

layout(k)
par(mar = c(0, 0, 0, 0))
for (i in 1:11){
   if (i %in% c(1)) axis.side <- c(2,3)
   if (i %in% c(10)) axis.side <- c(1,2)
   if (i %in% c(11)) axis.side <- c(1)
   if (i %in% c(4,7)) axis.side <- c(2)
   if (i %in% c(2)) axis.side <- c(3)
   if (i %in% c(3)) axis.side <- c(3,4)
   if (i %in% c(5,8)) axis.side <- NULL
   if (i %in% c(6,9)) axis.side <- c(4)
   
   gulf.map(land = FALSE, xlim = c(-66, -60), ylim = c(45.6, 49.1), axis.side = axis.side)
   # gulf.map(land = FALSE, xlim = c(-66, -60), ylim = c(45.6, 49.1))
   
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
   #contour(xx, yy, 1000 * zz, add = TRUE, levels = c(seq(0, 3000, by = 300), 10000), labels = "")
   #filled.contour(xx, yy, 1000 * zz, add = TRUE, levels = c(seq(0, 3000, by = 300), 10000))

   #wind.rose()

   map.fishing.zones(species = 2526, lwd = 1)
   coastline(col = "grey80", border = "grey40", lwd = 1)
   box(lwd = 1)
       
   # Display cross-validated residuals:
   #r <- m[[i]]$data[, m[[i]]$variables[1]] - m[[i]]$cross.validation[, 1]
   #index <- r >=0
   #points(longitude.scset(m[[i]]$data)[index], latitude.scset(m[[i]]$data)[index], cex = 0.05 * sqrt(1000*r[index]), col = "red", lwd = 2)
   #points(longitude.scset(m[[i]]$data)[!index], latitude.scset(m[[i]]$data)[!index], cex = 0.05 * sqrt(-1000*r[!index]), col = "black", lwd = 2)
    
   for (j in 1:1) lines(p[[j]]$longitude, p[[j]]$latitude)  
   
   text(-61, 48.7, years[i], cex = 1.4)   
}

#colors <- colorRampPalette(c("blue", "yellow", "orange", "red", "darkred"))
str <- paste0(breaks[1:(length(breaks)-1)], " - ", breaks[2:length(breaks)])
str[length(str)] <- paste(breaks[length(breaks)-1], "+")

plot(c(0, 1), c(0, 1), type = "n", axes = FALSE, xlab = "", ylab = "")
a <- legend("center", 
       legend = rev(str),
       pt.bg = rev(cols(length(breaks))),
       pch = 22,
       pt.cex = 3,
       cex = 0.95, bty = "n", plot = FALSE)
legend("center", 
       legend = rev(str),
       pt.bg = rev(cols(length(breaks))),
       pch = 22,
       pt.cex = 3,
       cex = 0.95, bty = "n")       

str <- paste0(breaks[1:(length(breaks)-1)], " - ", breaks[2:length(breaks)])
str[length(str)] <- paste(breaks[length(breaks)-1], "+")
      
legend("bottomleft", 
       legend = rev(str),
       pt.bg = rev(cols(length(breaks))),
       pch = 22,
       pt.cex = 2.6,
       cex = 0.85,  bg = "white", title = "n / km2") 
       

#points(a$rect$left + 0.5 * a$rect$w, a$rect$top - a$rect$h)
str <- "# / km2"
if (var %in% c("COM", "COMSC345")) str <- "kg / km2"
text(a$rect$left + 0.5 * a$rect$w, a$rect$top - a$rect$h + 0.04, str, pos = 1, cex = 1.25)        

if (jpeg) dev.off()
 