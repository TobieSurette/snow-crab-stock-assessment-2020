library(gulf.data)
library(gulf.graphics)

year <- 2020
language <- "french"
jpeg <- TRUE

# Read tow data:
s <- read.scsset(year = year, valid = 1)

# Load and process sunrise and sunset times:
x <- read.csv(paste0("data/sunrise and sunset/scs.dawn.dusk.times.", year, ".csv"), header = TRUE, stringsAsFactors = FALSE)

x$sunrise <- as.numeric(as.POSIXct(paste(x$date, x$dawn.civil)) - date(x))
x$sunset <- as.numeric(as.POSIXct(paste(x$date, x$dusk.civil)) - date(x))
x$julian <- round(julian(date(x)))

s$julian <- round(julian(date(s)))
s$time <- as.numeric(time(s, "touchdown") - date(s))

clg()
if (!jpeg) dev.new() else jpeg(filename = paste0("results/figures/dawn and dusk ", year, " - ", language, ".jpeg"), width = 7*480, height = 7*480, res = 7*75)

xlim <- c(date("2020/07/10"), date(year = 2020, month = 9, day = 20))
xlim <- round(time2day(xlim, date("2020/01/01")))
plot(xlim, c(4, 23), type = "n", xlab = "", ylab = "", xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n")

# Day:
polygon(c(par("usr")[1], x$julian, rep(par("usr")[2], 2), rev(x$julian), rep(par("usr")[1], 2)),
        c(x$sunrise[1], x$sunrise, x$sunrise[nrow(x)], x$sunset[nrow(x)], rev(x$sunset), x$sunset[1], x$sunrise[1]),
        col = "khaki1", border = NA)

# Morning:
polygon(c(par("usr")[1], x$julian, rep(par("usr")[2], 2), rep(par("usr")[1], 2)),
        c(x$sunset[1], x$sunset, x$sunset[nrow(x)], rep(par("usr")[4],2), x$sunset[1]),
        col = "lightblue2", border = "lightblue3")

# Night:
polygon(c(par("usr")[1], x$julian, rep(par("usr")[2], 2), rep(par("usr")[1], 2)),
        c(x$sunrise[1], x$sunrise, x$sunrise[nrow(x)], rep(par("usr")[3], 2), x$sunrise[1]),
        col = "lightblue2", border = "lightblue3")

# Draw tow times:
points(s$julian + s$time / 24, s$time, pch = 21, bg = "grey")

# Draw month separators:
pos <- c(round(julian(date("2020/08/01"))) - 0.5, round(julian(date("2020/09/01"))) - 0.5)
vline(pos, col = "black", lty = "dashed")

pos <- c((par("usr")[1] + pos[1]) / 2, mean(pos), (pos[2] + par("usr")[2])/2)
if (language == "english"){
   month.name <- c(rep("", 6), "July", "August", "September")
   xlab <- "Day of survey"
   ylab <- "Time of day (hour)"
}else{
   month.name <- c(rep("", 6), "Juillet", "Août", "Septembre")
   ylab <- "Heure"
   xlab <- "Jour du relevé"
}
axis(2, at = seq(4, 22, by = 4))

mtext(ylab, 2, 2.5, cex = 1.5)
mtext(xlab, 1, 2.5, cex = 1.5)
for (i in 1:length(pos)) text(pos[i], par("usr")[3] + 0.96*diff(par("usr")[3:4]), month.name[i+6], cex = 1.5)

labels <- seq(0, 60, by = 10)
at <- round(julian(min(date(s)) + 24*60*60*labels))

axis(1, at = at, labels = labels)

box()

if (jpeg) dev.off()

