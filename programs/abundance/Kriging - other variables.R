library(gulf)

source("C:/gulf package/gulf/R/summary.scset.R")
source("C:/gulf package/gulf/R/ked.scset.R")

load("C:/gulf package/gulf/data/kriging.polygons.revised.rda")
p <- kriging.polygons
poly.str <- c("gulf", "zone12", "zone19", "zoneE", "zoneF", "zoneEF_unassigned", "zone19_F_buffer", "zone19_12_buffer", "static_closure_2018", "static_closure_inside_2018")
p <- p[poly.str]

vars <- category.str(sex = 1)
vars <- c("COMLT102SC3", "COMLT102SC5", "COMLT102SC4", "COMLT102SC1", "COMLT102ML", "TMMGE102SC5", "TMMGE95SC5")     # Problem categories
vars <- c("MMGE95SC12", "MMGE95SC3", "MMGE95SC4", "MMGE95SC5")
vars <- "MIGE34L45"  # Instar VIII recruitment.
vars <- c("MM", "MMGE95", "MML95") # Adult males.
vars <- c("FI", "FIGNO", "FM", "FP", "FMULT") # Females.
vars <- c("MIGE56L69", "MIGE69L83", "MIGE83", "MIGE83L98", "MMGE95SC12") # R-4, R-3, R-2 male recruitment. 
vars <- c( "MMGE95SC12") # Elmer's R-2 and R-1 male recruitment. 
vars <- c( "MIGE83L98SC345") # Skip-moulters.

vars <- c("MMGE95SC12", "MMGE95SC3", "MMGE95SC4", "MMGE95SC5", "MIGE34L45", "MM", "MMGE95", "MML95", 
          "FM", "FIGNO", "FP", "FMULT", "MIGE56L69", "MIGE69L83", "MIGE83", "MIGE83L98", "MMGE95SC12", "MIGE83L98SC345")
vars <- unique(vars)
          

x <- read.scset(year = 2019, valid = 1)
x <- x[substr(x$tow.id, 2, 2) != "C", ]
x <- summary(x, category = "FM")
v <- gulf::variogram(x, variable = "FM", lag = 3, max.distance = 75, fit = TRUE, inits = list(range = 20))
plot(v)

# Uncorrected variables:
res <- list()
years <- 2019
for (i in 1:length(years)){
   # Perform kriging with external drift using three-year variogram averaging:
   #m <- ked.scset(year = years[i], variables = vars, grid = c(100, 100), weight = TRUE, hard.shelled = TRUE, variogram.average = 3, units = "t", bug = FALSE)
   
   # Use range of 125 km for sparse categories SC4 and SC5:
   m <- ked.scset(year = years[i], variables = vars, grid = c(100, 100), variogram.average = 3, bug = FALSE, max.distance = 75)
   print(c(years[i], nrow(m$data)))

   res[[i]] <- summary.ked(m, polygon = p["gulf"])
}

r <- NULL
for (i in 1:length(res)){
   r <- rbind(r, cbind(list(year = as.numeric(names(res)[i])), res[[i]]))
}

# Fill-in 
index <- is.na(r$sd)

slog = sqrt(log((r$sd.sample[index]^2)/(r$mean[index]^2)+1));                                                                            
mlog = log(r$mean[index])-(slog^2)/2;                                                                                       
cilog = exp(cbind(mlog - 1.959964 * slog, mlog + 1.959964 *slog)); 
            
r$lowerCI[index] <- r$mean[index] - 1.96 * r$sd.sample[index] / sqrt(r$n.sample[index])
r$upperCI[index] <- r$mean[index] + 1.96 * r$sd.sample[index] / sqrt(r$n.sample[index])


# Instar VIII recruitment figure:
clg()
windows(width = 8.5, height = 5)
plot(c(1997, 2018), c(0, 450), type = "n", xlab = "", ylab = "", yaxs = "i")
grid()
lines(r$year, r$mean / 1000000, lwd = 2, col = "black")
points(r$year, r$mean / 1000000, pch = 21, bg = "black", cex = 1.25)
for (i in 1:nrow(r)){
   lines(rep(r$year[i], 2), c(r$lowerCI[i], r$upperCI[i]) / 1000000 )
   lines(c(r$year[i] - 0.15, r$year[i] + 0.15), rep(r$lowerCI[i], 2) / 1000000 )
   lines(c(r$year[i] - 0.15, r$year[i] + 0.15), rep(r$upperCI[i], 2) / 1000000 )
}
mtext("Abundance (millions)", 2, 2.5, cex = 1.25)
mtext("Year", 1, 2.5, cex = 1.25)
box()

# Mature male plots:
clg()
windows(width = 8.5, height = 6)
plot(c(1997, 2018), c(0, 500), type = "n", xlab = "", ylab = "", yaxs = "i", xaxt = "n")
grid()
vars <- c("MM", "MMGE95", "MML95")
pch = c(21, 22, 24)
lty = c("solid", "dashed", "dotted")
col <- c("grey20", "grey50", "grey90")
for (j in 1:length(vars)){
   rr <- r[r$variable == vars[j], ]
   lines(rr$year, rr$mean / 1000000, lwd = 2, col = "black", lty = lty[j])
   for (i in 1:nrow(r)){
      lines(rep(rr$year[i], 2), c(rr$lowerCI[i], rr$upperCI[i]) / 1000000 )
      lines(c(rr$year[i] - 0.15, rr$year[i] + 0.15), rep(rr$lowerCI[i], 2) / 1000000 )
      lines(c(rr$year[i] - 0.15, rr$year[i] + 0.15), rep(rr$upperCI[i], 2) / 1000000 )
   }
   points(rr$year, rr$mean / 1000000, pch = pch[j], bg = col[j], cex = 1.25)
}
axis(1, at = seq(1997, 2018, by = 4))
axis(1, at = seq(1999, 2018, by = 4))
mtext("Abundance (millions)", 2, 2.5, cex = 1.25)
mtext("Year", 1, 2.5, cex = 1.25)
box()
legend("topright", 
       legend = c("Total adult male", "Adult male >= 95mm", "Adult male < 95mm"),
       pch = pch,
       lty = lty,
       pt.bg = col,
       pt.cex = 1.25,
       cex = 1.25,
       bg = "white")

# Mature and pubescent females:
language <- "french"
clg()
windows(width = 8.5, height = 6)
plot(c(1997, 2018), c(0, 1050), type = "n", xlab = "", ylab = "", yaxs = "i", xaxt = "n")
grid()
vars <- c("FM", "FIGNO")
pch = c(21, 22)
lty = c("solid", "dashed")
col <- c("grey20", "grey90")
for (j in 1:length(vars)){
   rr <- r[r$variable == vars[j], ]
   lines(rr$year, rr$mean / 1000000, lwd = 2, col = "black", lty = lty[j])
   for (i in 1:nrow(r)){
      lines(rep(rr$year[i], 2), c(rr$lowerCI[i], rr$upperCI[i]) / 1000000 )
      lines(c(rr$year[i] - 0.15, rr$year[i] + 0.15), rep(rr$lowerCI[i], 2) / 1000000 )
      lines(c(rr$year[i] - 0.15, rr$year[i] + 0.15), rep(rr$upperCI[i], 2) / 1000000 )
   }
   points(rr$year, rr$mean / 1000000, pch = pch[j], bg = col[j], cex = 1.25)
}
axis(1, at = seq(1997, 2018, by = 4))
axis(1, at = seq(1999, 2018, by = 4))
if (language == "french") mtext("Abondance (millions)", 2, 2.5, cex = 1.25)
if (language == "english") mtext("Abundance (millions)", 2, 2.5, cex = 1.25)
if (language == "french") mtext("Année", 1, 2.5, cex = 1.25)
if (language == "english") mtext("Year", 1, 2.5, cex = 1.25)
if (language == "french") str <- c("Femelle mature", "Femelle pubère")
if (language == "english") str <- c("Mature female", "Pubescent female")
legend("topright", 
       legend = str,
       pch = pch,
       lty = lty,
       pt.bg = col,
       pt.cex = 1.25,
       cex = 1.25,
       bg = "white")
box()  
  
# Primiparous and multiparous females:
language <- "french"
clg()
windows(width = 8.5, height = 6)
plot(c(1997, 2018), c(0, 800), type = "n", xlab = "", ylab = "", yaxs = "i", xaxt = "n")
grid()
vars <- c("FP", "FMULT")
pch = c(21, 22)
lty = c("solid", "dashed")
col <- c("grey20", "grey90")
for (j in 1:length(vars)){
   rr <- r[r$variable == vars[j], ]
   lines(rr$year, rr$mean / 1000000, lwd = 2, col = "black", lty = lty[j])
   for (i in 1:nrow(r)){
      lines(rep(rr$year[i], 2), c(rr$lowerCI[i], rr$upperCI[i]) / 1000000 )
      lines(c(rr$year[i] - 0.15, rr$year[i] + 0.15), rep(rr$lowerCI[i], 2) / 1000000 )
      lines(c(rr$year[i] - 0.15, rr$year[i] + 0.15), rep(rr$upperCI[i], 2) / 1000000 )
   }
   points(rr$year, rr$mean / 1000000, pch = pch[j], bg = col[j], cex = 1.25)
}
axis(1, at = seq(1997, 2018, by = 4))
axis(1, at = seq(1999, 2018, by = 4))
if (language == "french") mtext("Abondance (millions)", 2, 2.5, cex = 1.25)
if (language == "english") mtext("Abundance (millions)", 2, 2.5, cex = 1.25)
if (language == "french") mtext("Année", 1, 2.5, cex = 1.25)
if (language == "english") mtext("Year", 1, 2.5, cex = 1.25)
if (language == "french") str <- c("Femelle primipare", "Femelle multipare")
if (language == "english") str <- c("Primiparous female", "Multiparous female")
box()
legend("topright", 
       legend = rev(str),
       pch = rev(pch),
       lty = rev(lty),
       pt.bg = rev(col),
       pt.cex = 1.25,
       cex = 1.25,
       bg = "white")
       
              
area <- function(x){
   tmp <- deg2km(x$longitude, x$latitude, long.ref = -66, lat.ref = 45.5, method = "ellipsoid")
   return(area.polygon(as.polygon(tmp$x, tmp$y)))
}

# Extract polygon info:
polygons <- res[[1]]$polygon
biomass <- matrix(NA, nrow = length(res), ncol = length(polygons))
for (i in 1:length(res)){
   for (j in 1:length(polygons)){
      biomass[i,j] <- res[[i]]$mean[res[[i]]$polygon == polygons[j]]
   }
}
rownames(biomass) <- years
colnames(biomass) <- polygons
biomass <- round(biomass, 1)

mu <- unlist(lapply(res, function(x) x$mean[x$polygon == "gulf"] / 1000))
lowerCI <- unlist(lapply(res, function(x) x$lowerCI[x$polygon == "gulf"] / 1000))
upperCI <- unlist(lapply(res, function(x) x$upperCI[x$polygon == "gulf"] / 1000))
dbarplot(mu, years, width = 1, ylim = c(0, 130000))
for (i in 1:length(years)){
   lines(c(years[i], years[i]), c(lowerCI[i], upperCI[i]), lwd = 2)
   lines(c(years[i]-0.2, years[i]+0.2), c(lowerCI[i], lowerCI[i]), lwd = 2)
   lines(c(years[i]-0.2, years[i]+0.2), c(upperCI[i], upperCI[i]), lwd = 2)
}

reported <- c(35795, 31681, 40291, 
              63162, 55965, 71022, 
              74997, 65822, 85086, 
              65868, 56283, 76610, 
              67534, 60994, 74579, 
              58808, 52754, 65466, 
              99145, 87749, 111600,
              66021, 57456, 75495)
mu.reported <- reported[seq(1, length(reported), by = 3)]
lowerCI.reported <- reported[seq(2, length(reported), by = 3)]
upperCI.reported <- reported[seq(3, length(reported), by = 3)]
lines(years, mu.reported, col = "red", lwd = 2)
for (i in 1:length(years)){
   lines(c(years[i], years[i]), c(lowerCI.reported[i], upperCI.reported[i]), lwd = 2, col = "red")
   lines(c(years[i]-0.2, years[i]+0.2), c(lowerCI.reported[i], lowerCI.reported[i]), lwd = 2, col = "red")
   lines(c(years[i]-0.2, years[i]+0.2), c(upperCI.reported[i], upperCI.reported[i]), lwd = 2, col = "red")
}

# Plot variograms:
for (i in 1:length(m$variogram)){
   windows()
   plot(m$variogram[[i]])
}

# Display density maps:
library(akima)
for (i in 1:1){
   mu <- m$map[,,i]
   lon <- m$map.longitude 
   lat <- m$map.latitude
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
   
   cols <- colorRampPalette(c("blue", "yellow", "orange", "red", "darkred"))
   windows()
   gulf.map(land = FALSE, xlim = c(-66, -60), ylim = c(45.6, 49.1))
   image(xx, yy, zz, add = TRUE, col = cols(100), breaks = c(seq(0, 3000, len = 100), 10000))
   coastline(col = "grey80", border = "grey40")
   wind.rose()
   box()
   lines(scz$scz$longitude, scz$scz$latitude, lwd = 2, col = "black")
    
   # Display cross-validated residuals:
   r <- m$data[, m$variables[i]] - m$cross.validation[, i]
   index <- r >=0
   points(longitude.scset(m$data)[index], latitude.scset(m$data)[index], cex = 0.05 * sqrt(r[index]), col = "red", lwd = 2)
   points(longitude.scset(m$data)[!index], latitude.scset(m$data)[!index], cex = 0.05 * sqrt(-r[!index]), col = "black", lwd = 2)
    
   for (j in 1:8) lines(p[[j]]$longitude, p[[j]]$latitude)
}

cat <- read.rvcat(year = 2000:2017, species = 2526)
set <- read.rvset(year = 2000:2017, experiment = 1)
set <- merge.catch(set, cat)
points(longitude(set), latitude(set), cex = 1 * sqrt(set$weight.caught), col = "blue", lwd = 2)
index <- set$weight.caught == 0
points(longitude(set)[index], latitude(set)[index], cex = 1, pch = "x", col = "black", lwd = 2)
len <- read.rvlen(year = 2000:2017, species = 2526)

b <- read.scbio(year = 2017)
b <- b[b$sex  == 1, ]
s <- read.scset(year = 2017)
index <- match(b$tow.id, s$tow.id)
s$depth <- -depth(longitude(s), latitude(s))
s$depth10 <- round(s$depth / 10) * 10

b$depth <- s$depth10[index]

a <- kronecker(matrix(1, nrow = 1, ncol = 5), t(t(1:7)))
layout(a)
par(mar = c(0, 0, 0, 0))
for (i in 3:9){
   dbarplot(table(round(b$carapace.width[b$depth == i * 10])), xlim = c(10, 130), width = 1)
}




 