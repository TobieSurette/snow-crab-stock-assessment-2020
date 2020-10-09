library(gulf)

year <- 2017
x <- read.scset(year = year, valid = 1)

res <- data.frame(year = year, tow.id = x$tow.id, touchdown = NA, liftoff = NA)

clg()
windows(width = 11, height = 8.5)
m <- kronecker(matrix(1:2), matrix(1, nrow = 4, ncol = 5))
m <- cbind(0, m, 0)
m <- rbind(0, m, 0)
layout(m)
par(mar = c(0, 0, 0, 0))
tows <- which(is.na(res$touchdown) | is.na(res$liftoff) | res$touchdown == "        " | res$liftoff == "        ")
for (i in tows){
   s <- read.star.oddi(year = year, tow.id = x$tow.id[i], type = "tilt")
   if (!("tilt.x" %in% names(s))) s <- NULL
   end.time <- time2min(end.time(x[i,]), start.time(x[i,]))
   depth <- -depth(longitude(x[i,]), latitude(x[i,])) 
   if (sum(s$pressure > 0) < 300){
      print("Reading headline Star Oddi.") 
      h <- read.star.oddi(year = year, tow.id = x$tow.id[i])
      h$time <- time2min(time(h), start.time(x[i,]))      
      h$depth <- depth * h$pressure / mean(h$pressure[(h$time > 0) & (h$time < end.time)])
      xlim <- h$time[range(which(h$depth > (depth-30)))]
      plot(xlim, c(depth-30, depth+10), type = "n", xaxs = "i", xlab = "", ylab = "", xaxt = "n")
      grid()
      lines(h$time, h$depth, col = "blue", lwd = 2)
   }else{
      s$time <- time2min(time(s), start.time(x[i,]))
      s$depth <- depth * s$pressure / mean(s$pressure[(s$time > 0) & (s$time < end.time)])
      xlim <- s$time[range(which(s$depth > (depth-30)))]

      # Depth plots:
      plot(xlim, c(depth-30, depth+10), type = "n", xaxs = "i", xlab = "", ylab = "", xaxt = "n")
      grid()
      lines(s$time, s$depth, col = "blue", lwd = 2)
   }
   mtext(paste0(i, ") ", x$tow.id[i]), 3, 2.5, cex = 1.5)
   mtext("Depth (meters)", 2, 2.5, cex = 1.2)   
    
   # Angle plots:
   if (!is.null(s)){
      vars <- c("tilt.x", "tilt.y", "tilt.z")
      index <- s$time >= xlim[1] & s$time <= xlim[2]
      plot(xlim, c(min(s[index, vars], na.rm = TRUE), max(s[index, vars], na.rm = TRUE)), type = "n", xaxs = "i", xlab = "", ylab = "")
      grid()
      lines(s$time, s$tilt.x, col = "blue", lwd = 2)
      lines(s$time, s$tilt.y, col = "red", lwd = 2)
      lines(s$time, s$tilt.z, col = "green", lwd = 2) 
      mtext("Time (min)", 1, 2.5, cex = 1.2)
      mtext("Angle (degrees)", 2, 2.5, cex = 1.2)
   }
   p <- draw.points()
   print(p)
   p <- as.numeric(p[1:2, 1])
   res$touchdown[i] <- p[1]
   res$liftoff[i] <- p[2]
}

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
file <- paste0("U:/Snow Crab/Stock Assessment 2019/Touchdown and Swept Area Results/Touchdown and Liftoff - Inits ", year, ".csv")
write.csv(res[c("year", "tow.id", "touchdown", "liftoff")], file = file, row.names = FALSE)


