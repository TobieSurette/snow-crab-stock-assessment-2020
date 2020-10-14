library(gulf)

jpeg <- TRUE

years <- c(2018, 2019)
y <- read.scset(year = years[2], valid = 1)
y <- y[substr(y$tow.id, 2, 2) != "C", ]
z <- read.csv(paste0("U:/Snow Crab/Stock Assessment 2019/Touchdown and Swept Area Results/Touchdown and Liftoff - Tilt ", years[2], ".csv"), stringsAsFactors = FALSE)
y$liftoff <- z$liftoff[match(y$tow.id, z$tow.id)]
y$passive.duration <- as.numeric(difftime(time.default(paste0(date(y), " ", y$liftoff, " AST")), time.default(paste0(date(y), " ", y$end.time.logbook, " AST")), units = "secs"))


x <- read.scset(year = years[1], valid = 1)
z <- read.csv(paste0("U:/Snow Crab/Stock Assessment 2019/Touchdown and Swept Area Results/Touchdown and Liftoff - Tilt ", years[1], ".csv"), stringsAsFactors = FALSE)
x$liftoff <- z$liftoff[match(x$tow.id, z$tow.id)]
x$passive.duration <- as.numeric(difftime(time.default(paste0(date(x), " ", x$liftoff, " AST")), time.default(paste0(date(x), " ", x$end.time.logbook, " AST")), units = "secs"))

index <- match(substr(y$tow.id, 3, 5), substr(x$tow.id, 3, 5))
y$passive.duration2 <- x$passive.duration[index]


plot(y$passive.duration - y$passive.duration2, ylab = "Time Difference (s)")
grid()
abline(0, 0, lwd = 2, col = "red")


if (jpeg) jpeg(file = paste0("U:/Snow Crab/Stock Assessment 2019/Passive Phase Duration Differences ", years[1], "-", years[2], ".jpg"),
               width = 8*480, height = 8*480, res = 7*75)
delta <- (y$passive.duration - y$passive.duration2)
index <- delta >= 0
gulf.map(sea = TRUE)
points(longitude(y)[index], latitude(y)[index], cex = 0.2 * sqrt(delta[index]), pch = 21, bg = "red")
points(longitude(y)[!index], latitude(y)[!index], cex = 0.2 * sqrt(-delta[!index]), pch = 21, bg = "grey")
values <- 60*seq(-3, 4, by = 1)
values.str <- c("-3 min", "-2 min", "-1 min", "+0 min", "+1 min", "+2 min", "+3 min", "+4 min")
legend("bottomleft", 
       legend = values.str,
       pch = 21, pt.cex = 0.2 * sqrt(abs(values)),
       pt.bg = c("grey", "grey", "grey", rep("red", 5)),
       cex = 1.10,
       bg = "white")
if (jpeg) dev.off()

if (jpeg) jpeg(file = paste0("U:/Snow Crab/Stock Assessment 2019/Passive Phase Duration ", years[2], ".jpg"),
               width = 8*480, height = 8*480, res = 7*75)
delta <- y$passive.duration
gulf.map(sea = TRUE)
points(longitude(y), latitude(y), cex = 0.2 * sqrt(delta), pch = 21, bg = "grey")
values <- 60*seq(0, 6, by = 1)
values.str <- c("+0 min", "+1 min", "+2 min", "+3 min", "+4 min", "+5 min", "+6 min")
legend("bottomleft", 
       legend = values.str,
       pch = 21, pt.cex = 0.2 * sqrt(abs(values)),
       pt.bg = "grey",
       cex = 1.10,
       bg = "white")
if (jpeg) dev.off()
