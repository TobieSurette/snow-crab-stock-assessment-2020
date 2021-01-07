library(gulf.data)
library(gulf.graphics)
library(gulf.spatial)

output <- "pdf"
years <- 2017:2020
language <- language("en")

xlab <- "x(meters)"
ylab <- "y(meters)"
if (language == "french"){
   xlab <- "x(mètres)"
   ylab <- "y(mètres)"
}

clg()
gdevice(output, file = paste0("results/figures/", language, "/vessel.tracks.", min(years), "-", max(years)))
m <- kronecker(matrix(1:4, ncol = 2), matrix(1, ncol = 5, nrow = 5))
m <- rbind(0, cbind(0, 0, m, 0), 0, 0)
par(mar = c(0,0,0,0))
layout(m)
for (i in 1:length(years)){
   print(years[i])
   s <- read.scsset(years[i], valid = 1, survey = "regular")
   #s <- s[s$tow.id != "GP276F", ]

   plot(c(-500, 500), c(-500, 500), type = "n", xlab = "", ylab = "", xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n")
   grid()

   if (i == 1) mtext(ylab, 2, 2.5, cex = 1.25, at = par("usr")[3])
   if (i == 4) mtext(xlab, 1, 2.75, cex = 1.25, at = par("usr")[1])
   if (i %in% 1:2) axis(2)
   if (i %in% c(2,4)) axis(1)

   for (j in 276:nrow(s)){
      print(j)
      e <- read.esonar(s[j, ])
      if (length(e) > 0){
         e <- trim(e, range = c(time(s[j, ], "touchdown"), time(s[j, ], "liftoff")))
         if (nrow(e) > 0){
            y <- 1000*deg2km(lon(e), lat(e))
            y[,1] <- y[,1] - y[1,1]
            y[,2] <- y[,2] - y[1,2]
            t <- time2min(time(e), time(s[j, ], "touchdown"))
            stop <- time2min(time(s[j, ], "stop"), time(s[j, ], "touchdown"))
            index <- t <= stop
            lines(y[index,1], y[index,2], col = "grey")
            lines(y[!index,1], y[!index,2], col = "red")
         }
      }
   }
   points(0, 0, pch = 21, bg = "grey50")

   text(-380, 400, years[i], cex = 1.4)

   # Draw reference circles:
   theta <- seq(0, 2 * pi, len = 1000)
   lines(300 * cos(theta), 300 * sin(theta), lty = "dashed", col = "grey20")

   box()
}
dev.off()
