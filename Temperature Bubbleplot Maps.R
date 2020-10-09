library(gulf)

jpeg <- TRUE

years <- c(2018, 2019)
y <- read.scset(year = years[2], valid = 1)
y <- y[substr(y$tow.id, 2, 2) != "C", ]

x <- read.scset(year = years[1], valid = 1)
index <- match(substr(y$tow.id, 3, 5), substr(x$tow.id, 3, 5))
y$temperature <- NA
y$temperature <- x$bottom.temperature[index]


if (jpeg) jpeg(file = paste0("U:/Snow Crab/Stock Assessment 2019/Bottom Temperature Differences ", years[1], "-", years[2], ".jpg"),
               width = 8*480, height = 8*480, res = 7*75)
delta <- (y$bottom.temperature - y$temperature)
index <- delta >= 0
gulf.map(sea = TRUE)
points(longitude(y)[index], latitude(y)[index], cex = 1.5 * sqrt(delta[index]), pch = 21, bg = "red")
points(longitude(y)[!index], latitude(y)[!index], cex = 1.5 * sqrt(-delta[!index]), pch = 21, bg = "grey")
values <- seq(-3, 3, by = 1)
values.str <- c("-3°C", "-2°C", "-1°C", "+0°C", "+1°C", "+2°C", "+3°C")
legend("bottomleft", 
       legend = values.str,
       pch = 21, pt.cex = 1.5 * sqrt(abs(values)),
       pt.bg = c("grey", "grey", "grey", "grey", "red", "red", "red"),
       cex = 1.25,
       bg = "white")
if (jpeg) dev.off()

if (jpeg) jpeg(file = paste0("U:/Snow Crab/Stock Assessment 2019/Bottom Temperatures ", years[2], ".jpg"),
               width = 8*480, height = 8*480, res = 7*75)
delta <- y$bottom.temperature
index <- delta >= 0
gulf.map(sea = TRUE)
points(longitude(y)[index], latitude(y)[index], cex = 1.0 * sqrt(delta[index]), pch = 21, bg = "red")
points(longitude(y)[!index], latitude(y)[!index], cex = 1.0 * sqrt(-delta[!index]), pch = 21, bg = "grey")
values <- seq(-1, 6, by = 1)
values.str <- c("-1°C", "0°C", "+1°C", "+2°C", "+3°C", "+4°C", "+5°C", "+6°C")
legend("bottomleft", 
       legend = values.str,
       pch = 21, pt.cex = 1.5 * sqrt(abs(values)),
       pt.bg = c("grey", rep("red", 7)),
       cex = 1.25,
       bg = "white")
if (jpeg) dev.off()
