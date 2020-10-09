library(gulf)
library(mgcv)

language <- "english"
jpeg <- FALSE

# Load data:
s <- read.scset(year = 2019, valid = 1)
s <- s[substr(s$tow.id,2,2) != "C",]

#b <- read.star.oddi(s)

# Calculate bottom temperatures:
#r <- summary(b)

# Import Star Oddi bottom temperatures:
s$temperature <- s$bottom.temperature
                                                                   
if (language == "english"){
   xlab <- "Depth (meters)" 
   ylab <- "Temperature (°C)"
}else{
   xlab <- "Profondeur (mètres)" 
   ylab <- "Température (°C)"
}

if (jpeg){
   jpeg(filename = paste0("U:/Snow Crab/Stock Assessment 2019/Survey Summary/Survey Temperature Plot - ", language, ".jpeg"), width = 600 * 8.5, height = 600 * 8.5, res = 75 * 8)
}else{
   clg()
   windows(width = 8.5, height = 8.5)
}

index <- s$tow.id %in% setdiff(s$tow.id, c("GP348F", "GP355F"))
dd <- -depth(longitude(s), latitude(s))[index]
tt <- s$temperature[index]

d <- round(dd / 50)
cols <- grey.colors(max(d))[d]

plot(dd, tt, pch = 21, bg = cols, cex = 1.25, xlab = xlab, ylab = "", cex.lab = 1.5)    

round(-depth(longitude(s), latitude(s)) / 25)

grid()
mtext(ylab, 2, 2.75, cex = 1.5)
#text(dd, tt, s$tow.id)

m <- gam(tt[index] ~ s(log(dd))) 
xx <- seq(par("usr")[1], par("usr")[2], len = 1000)  
pp <- predict(m, newdata = list(dd = xx))
lines(xx, pp, col = "black", lwd = 2)

index <- s$tow.id %in% c("GP348F", "GP355F")
points(-depth(longitude(s), latitude(s))[index], s$temperature[index], pch = 22, bg = "red", cex = 2)
lines(c(40, 40), par("usr")[3:4], lwd = 2, lty = "dashed")
lines(c(150, 150), par("usr")[3:4], lwd = 2, lty = "dashed")


if (jpeg) dev.off()

if (language == "english"){
   xlab <- "Depth (meters)" 
   ylab <- "Temperature (°C)"
}else{
   xlab <- "Profondeur (mètres)" 
   ylab <- "Température (°C)"
}

if (jpeg){
   jpeg(filename = paste0("U:/Snow Crab/Survey 2018/Survey Temperature Map - ", language, ".jpeg"), width = 600 * 8.5, height = 600 * 8.5, res = 75 * 8)
}else{
   clg()
   windows(width = 8.5, height = 8.5)
}

gulf.map(sea = TRUE, land = FALSE)

dd <- -depth(longitude(s), latitude(s))
tt <- s$temperature

map.fishing.zones(species = 2526, lwd = 2)
index <- tt > 0
scale <- 1.1 
points(longitude(s)[index], latitude(s)[index], pch = 21, bg = "grey", cex = scale * sqrt(abs(tt[index])))
points(longitude(s)[!index], latitude(s)[!index], pch = 21, bg = "black", cex = scale * sqrt(abs(tt[!index])))
index <- s$tow.id %in% c("GP348F", "GP355F")
points(longitude(s)[index], latitude(s)[index], pch = 22, cex = scale * sqrt(abs(tt[index])), bg = "red")

coastline(lwd = 2)
map.place.names(sea = FALSE, language = language)

legend("bottomleft", 
       legend = c("< 0", "0", "2", "4", "6"), 
       pt.cex = scale * sqrt(c(0.5, 0.1, 2, 4, 6)), 
       pch = 21,
       pt.bg = c("black", "grey", "grey", "grey", "grey"),
       bg = "white", 
       cex = 1.2, 
       title = "T(°C)")

box()

if (jpeg) dev.off()