library(gulf)
library(mgcv)

language <- "english"
jpeg <- TRUE
year <- 2018

# Load data:
s <- read.scset(year = year, valid = 1)
s$temperature <- s$bottom.temperature
s <- s[substr(s$tow.id,2,2) != "C", ]                                                                 

if (language == "english"){
   xlab <- "Depth (meters)" 
   ylab <- "Temperature (°C)"
}else{
   xlab <- "Profondeur (mètres)" 
   ylab <- "Température (°C)"
}

if (jpeg){
   jpeg(filename = paste0("U:/Snow Crab/Stock Assessment 2019/Survey Temperature Plot ", year, " - ", language, ".jpeg"), width = 600 * 8.5, height = 600 * 8.5, res = 75 * 8)
}else{
   clg()
   windows(width = 8.5, height = 8.5)
}

dd <- -depth(longitude(s), latitude(s))
tt <- s$temperature

plot(dd, tt, type = "n", pch = 21, bg = cols, ylim = c(-1, 7.5), 
     yaxs = "i", xlim = c(0, 400), xaxs = "i", cex = 1.25, xlab = xlab, ylab = "", yaxt = "n", cex.lab = 1.5)    
grid()
points(dd, tt,  pch = 21, bg = "grey", cex = 1.25)  
mtext(ylab, 2, 2.75, cex = 1.5)
axis(2, at = -1:7)

dd <- round(-depth(longitude(s), latitude(s))/20)*20
res <- aggregate(list(mean = s$temperature), by = list(depth = dd), mean, na.rm = TRUE) 
res$median <- aggregate(list(median = s$temperature), by = list(depth = dd), median, na.rm = TRUE)$median 
res$n <- aggregate(list(x = s$temperature), by = list(depth = dd), length)$x 
res$sd <- aggregate(list(x = s$temperature), by = list(depth = dd), sd, na.rm = TRUE)$x 
res$lci <- res$mean - 1.96 * res$sd / sqrt(res$n)
res$uci <- res$mean + 1.96 * res$sd / sqrt(res$n)
for (i in 1:nrow(res)){
   lines(rep(res$depth[i], 2), res[i, c("lci", "uci")], lwd = 2, col = "red")
   lines(c(res$depth[i]-4, res$depth[i]+4), rep(res$lci[i], 2), lwd = 2, col = "red")
   lines(c(res$depth[i]-4, res$depth[i]+4), rep(res$uci[i], 2), lwd = 2, col = "red")
}
points(res$depth, res$mean, pch = 21, bg = "red", cex = 1.25)
box()


if (jpeg) dev.off()

if (language == "english"){
   xlab <- "Depth (meters)" 
   ylab <- "Temperature (°C)"
}else{
   xlab <- "Profondeur (mètres)" 
   ylab <- "Température (°C)"
}

if (jpeg){
   jpeg(filename = paste0("U:/Snow Crab/Stock Assessment 2019/Survey Temperature Map ", year, " - ", language, ".jpeg"), width = 600 * 8.5, height = 600 * 8.5, res = 75 * 8)
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

coastline(lwd = 1)
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