library(gulf.data)
library(gulf.spatial)
library(mgcv)

year <- 2020
language <- "french"
output <- "pdf"
tables <- TRUE
var.str <- ifelse(language == "french", "température", "temperature")
language.str <- ifelse(language == "french", "français", "english")
path.name <- paste0("/", language.str, "/", var.str)
if (language == "english"){
   xlab <- "Depth (meters)"
   ylab <- "Temperature (°C)"
}else{
   xlab <- "Profondeur (mètres)"
   ylab <- "Température (°C)"
}

# Load data:
s <- read.scsset(year = year, valid = 1)
s <- s[survey(s) == "regular", ]

if (output != ""){
   if (language == "english") file.name <- paste0("bottom temperature depth profile ", year)
   if (language == "french")  file.name <- paste0("température versus profondeur ", year)
   path <- paste0("results/figures", path.name)
   if (!file.exists(path)) dir.create(path)
}
if (output == "jpeg") jpeg(file = paste0(path, "/", file.name, ".jpg") , width = 600 * 8.5, height = 600 * 8.5, res = 75 * 8)
if (output == "pdf")  pdf(file = paste0(path, "/", file.name, ".pdf") , width = 8.5, height = 8.5)
if (output == ""){
   clg()
   dev.new(width = 8.5, height = 8.5)
}

# Depth profile plot:
x <- depth(lon(s), lat(s))
y <- s$bottom.temperature
plot(x, y, type = "n", pch = 21, ylim = c(-1, 7.5),
     yaxs = "i", xlim = c(0, 400), xaxs = "i", cex = 1.25,
     xlab = "", ylab = "", yaxt = "n", cex.lab = 1.5)
grid()
points(x, y,  pch = 21, bg = "grey", cex = 1.25)
mtext(xlab, 1, 2.5, cex = 1.5)
mtext(ylab, 2, 2.5, cex = 1.5)
axis(2, at = -1:7)

x <- round(depth(lon(s), lat(s))/20)*20
res <- aggregate(list(mean = y), by = list(depth = x), mean, na.rm = TRUE)
res$median <- aggregate(list(median = y), by = list(depth = x), median, na.rm = TRUE)$median
res$n <- aggregate(list(x = y), by = list(depth = x), length)$x
res$sd <- aggregate(list(x = y), by = list(depth = x), sd, na.rm = TRUE)$x
res$lci <- res$mean - 1.96 * res$sd / sqrt(res$n)
res$uci <- res$mean + 1.96 * res$sd / sqrt(res$n)
for (i in 1:nrow(res)){
   lines(rep(res$depth[i], 2), res[i, c("lci", "uci")], lwd = 2, col = "red")
   lines(c(res$depth[i]-4, res$depth[i]+4), rep(res$lci[i], 2), lwd = 2, col = "red")
   lines(c(res$depth[i]-4, res$depth[i]+4), rep(res$uci[i], 2), lwd = 2, col = "red")
}
points(res$depth, res$mean, pch = 21, bg = "red", cex = 1.25)
box()
if (output != "") dev.off()

# Temperature map:
if (output != ""){
   if (language == "english") file.name <- paste0("bottom temperature map ", year)
   if (language == "french")  file.name <- paste0("carte de température du fond ", year)
   path <- paste0("results/figures", path.name)
   if (!file.exists(path)) dir.create(path)
}
if (output == "jpeg") jpeg(file = paste0(path, "/", file.name, ".jpg"), width = 600 * 8.5, height = 600 * 8.5, res = 75 * 8)
if (output == "pdf")  pdf(file = paste0(path, "/", file.name, ".pdf"), width = 8.5, height = 8.5)
if (output == ""){
   clg()
   dev.new(width = 8.5, height = 8.5)
}

map()
bathymetry()
coast()

#map.fishing.zones(species = 2526, lwd = 2)
index <- y > 0
scale <- 1.1
points(lon(s)[index], lat(s)[index], pch = 21, bg = "grey", cex = scale * sqrt(abs(y[index])))
points(lon(s)[!index], lat(s)[!index], pch = 21, bg = "black", cex = scale * sqrt(abs(y[!index])))

legend("bottomleft",
       legend = c("< 0", "0", "2", "4", "6"),
       pt.cex = scale * sqrt(c(0.5, 0.1, 2, 4, 6)),
       pch = 21,
       pt.bg = c("black", "grey", "grey", "grey", "grey"),
       bg = "white",
       cex = 1.2,
       title = "T(°C)")

box()

if (output != "") dev.off()
