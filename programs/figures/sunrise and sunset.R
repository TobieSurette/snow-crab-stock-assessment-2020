library(gulf)

year <- 2019
language <- "french"
jpeg <- TRUE

# Convenience functions:
hline <- function(x, col = "red", lty = "solid", ...) for (i in 1:length(x)) lines(par("usr")[1:2], c(x[i], x[i]), lty = lty, col = col, ...)
vline <- function(x, col = "red", lty = "solid", ...) for (i in 1:length(x)) lines(c(x[i], x[i]), par("usr")[3:4], lty = lty, col = col, ...)

s <- read.scset(year = year, valid = 1)

# Load and process sunrise and sunset times:
times <- read.csv(paste0("U:/Snow Crab/Stock Assessment 2019/Sunrise and Sunset Times ", year, ".csv"), header = TRUE, stringsAsFactors = FALSE)
names(times) <- c("year", "month", "day", "sunrise", "sunset", "length", "difference", "astronomical.start", "astronomical.end",
                  "nautical.start", "nautical.end", "civil.start", "civil.end", "time", "mil..km")
 
times$sunrise <- unlist(lapply(strsplit(times$civil.start, " "), function(x) x[1]))
times$sunset <- unlist(lapply(strsplit(times$civil.end, " "), function(x) x[1]))

times$sunrise <- paste0("0", unlist(lapply(strsplit(times$sunrise, ":"), function(x) x[1])), ":",
                        unlist(lapply(strsplit(times$sunrise, ":"), function(x) x[2])), ":00")
times$sunset <- paste0(12 + as.numeric(unlist(lapply(strsplit(times$sunset, ":"), function(x) x[1]))), ":",
                        unlist(lapply(strsplit(times$sunset, ":"), function(x) x[2])), ":00")

times$sunrise <- as.numeric(difftime(time.default(paste0(as.character(date(times)), " ", times$sunrise, " AST")), date(times), units = "hours"))
times$sunset <- as.numeric(difftime(time.default(paste0(as.character(date(times)), " ", times$sunset, " AST")), date(times), units = "hours"))

times$julian <- julian(date(times))

s$time <- julian(date(s)) + as.numeric(difftime(start.time(s), date(s), units = "hours"))/24

clg()
if (!jpeg) windows() else jpeg(filename = paste0("U:/Snow Crab/Stock Assessment 2019/Sunrise and Sunset Figure ", year, " - ", language, ".jpeg"), width = 7*480, height = 7*480, res = 7*75)

if (year == 2018) xlim <- c(0, 65) else xlim = c(0, 80)
plot(xlim, c(4, 23), type = "n", xlab = "", ylab = "", xaxs = "i", yaxs = "i", yaxt = "n")

polygon(c(par("usr")[1], times$julian - min(julian(date(s))) + 1, par("usr")[2], par("usr")[2], rev(times$julian - min(julian(date(s))) + 1),  par("usr")[1],  par("usr")[1]),
        c(times$sunrise[1], times$sunrise, times$sunrise[nrow(times)], times$sunset[nrow(times)], rev(times$sunset), times$sunset[1], times$sunrise[1]), col = "khaki1", border = NA) 

polygon(c(par("usr")[1], times$julian - min(julian(date(s))) + 1, par("usr")[2], par("usr")[2], par("usr")[1], par("usr")[1]),
        c(times$sunset[1], times$sunset, times$sunset[nrow(times)], par("usr")[4], par("usr")[4], times$sunset[1]), col = "lightblue2", border = "lightblue3") 

polygon(c(par("usr")[1], times$julian - min(julian(date(s))) + 1, par("usr")[2], par("usr")[2], par("usr")[1], par("usr")[1]),
        c(times$sunrise[1], times$sunrise, times$sunrise[nrow(times)], par("usr")[3], par("usr")[3], times$sunrise[1]), col = "lightblue2", border = "lightblue3") 
     
points(julian(date(s)) - min(julian(date(s))) + 1, 
       as.numeric(difftime(start.time(s), date(s), units = "hours")), pch = 21, bg = "grey")

vline(max(times$julian[times$month == 7]) - min(julian(date(s))) + 1, col = "black", lty = "dashed")
vline(max(times$julian[times$month == 8]) - min(julian(date(s))) + 1, col = "black", lty = "dashed")
vline(max(times$julian[times$month == 9]) - min(julian(date(s))) + 1, col = "black", lty = "dashed")

if (year == 2018) pos <- c(6, 28, 54)  
if (year == 2019) pos <- c(10, 35, 66) 
axis(2, at = seq(4, 22, by = 4))
if (language == "english"){
   mtext("Time of day (hour)", 2, 2.5, cex = 1.5)
   mtext("Survey day", 1, 2.5, cex = 1.5)
   text(pos[1], par("usr")[3] + 0.96*diff(par("usr")[3:4]), "July", cex = 1.5)
   text(pos[2], par("usr")[3] + 0.96*diff(par("usr")[3:4]), "August", cex = 1.5)
   text(pos[3], par("usr")[3] + 0.96*diff(par("usr")[3:4]), "September", cex = 1.5)
}
if (language == "french"){
   mtext("Heure", 2, 2.5, cex = 1.5)
   mtext("Jour du relevé", 1, 2.5, cex = 1.5)
   text(pos[1], par("usr")[3] + 0.96*diff(par("usr")[3:4]), "Juillet", cex = 1.5)
   text(pos[2], par("usr")[3] + 0.96*diff(par("usr")[3:4]), "Août", cex = 1.5)
   text(pos[3], par("usr")[3] + 0.96*diff(par("usr")[3:4]), "Septembre", cex = 1.5)
}
box()

if (jpeg) dev.off()

