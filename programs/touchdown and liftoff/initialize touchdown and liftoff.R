library(gulf.data)
library(gulf.spatial)

x <- read.scsset(year = 2020, valid = 1)

res <- data.frame(date = x$date, tow.id = x$tow.id, touchdown = NA, liftoff = NA)

clg()
windows(width = 15, height = 8.5)
m <- kronecker(matrix(1:2), matrix(1, nrow = 4, ncol = 5))
m <- cbind(0, m, 0)
m <- rbind(0, m, 0)
layout(m)
par(mar = c(0, 0, 0, 0))
tows <- which(is.na(res$touchdown) | is.na(res$liftoff) | res$touchdown == "        " | res$liftoff == "        ")
for (i in tows){
   print(x$tow.id[i])
   s <- read.star.oddi(x[i, ], probe = "footrope")
   if (!("tilt-x" %in% names(s))) s <- NULL
   h <- read.star.oddi(x[i, ], probe = "headline")
   stop.time <- time2min(stop.time(x[i,]), start.time(x[i,]))
   depth <- depth(lon(x[i,]), lat(x[i,]))

   xlim <- NULL

   # Footrope Star Oddi:
   if (!is.null(s)){
      s$time <- time2min(time(s), start.time(x[i,]))
      s$depth <- depth * s$pressure / mean(s$pressure[(s$time > 0) & (s$time < stop.time)])
      xlim <- s$time[range(which(s$depth > (depth-30)))]
   }

   # Headline Star Oddi:
   if (!is.null(h)){
      h$time <- time2min(time(h), start.time(x[i,]))
      h$depth <- depth * h$pressure / mean(h$pressure[(h$time > 0) & (h$time < stop.time)])
      if (is.null(xlim)) xlim <- h$time[range(which(h$depth > (depth-30)))]
   }


   if (!is.null(s) | !is.null(h)){
      plot(xlim, c(depth-30, depth+10), type = "n", xaxs = "i", xlab = "", ylab = "", xaxt = "n")
      grid()
      if (!is.null(s)) lines(s$time, s$depth, col = "blue", lwd = 2)
      if (!is.null(h)) lines(h$time, h$depth, col = "green", lwd = 2)

      mtext(paste0(i, ") ", x$tow.id[i]), 3, 2.5, cex = 1.5)
      mtext("Depth (meters)", 2, 2.5, cex = 1.2)

      # Angle plots:
      if (!is.null(s)){
         vars <- c("tilt-x", "tilt-y", "tilt-z")
         index <- s$time >= xlim[1] & s$time <= xlim[2]
         plot(xlim, c(min(s[index, vars], na.rm = TRUE), max(s[index, vars], na.rm = TRUE)), type = "n", xaxs = "i", xlab = "", ylab = "")
         grid()
         lines(s$time, s$"tilt-x", col = "blue", lwd = 2)
         lines(s$time, s$"tilt-y", col = "red", lwd = 2)
         lines(s$time, s$"tilt-z", col = "green", lwd = 2)
         mtext("Time (min)", 1, 2.5, cex = 1.2)
         mtext("Angle (degrees)", 2, 2.5, cex = 1.2)
      }
      p <- draw.points()
      print(p)
      p <- as.numeric(p[1:2, 1])
      res$touchdown[i] <- p[1]
      res$liftoff[i] <- p[2]
   }
}

save(res, file = "Temp.Rdata")

#tmp <- res

# Convert to real times:
index <- !is.na(res$touchdown)
res$touchdown.str <- "        "
res$touchdown.str[index] <- unlist(lapply(strsplit(as.character(start.time(x[index, ]) + round(60 * res$touchdown[index])), " "), function(x) x[2]))
index <- !is.na(res$liftoff)
res$liftoff.str <- "        "
res$liftoff.str[index] <- unlist(lapply(strsplit(as.character(start.time(x[index, ]) + round(60 * res$liftoff[index])), " "), function(x) x[2]))
res$touchdown <- res$touchdown.str
res$liftoff <- res$liftoff.str

# Write results:
file <- paste0("data/raw/touchdown and liftoff - inits ", 2020, ".csv")
write.csv(res[c("date", "tow.id", "touchdown", "liftoff")], file = file, row.names = FALSE)

