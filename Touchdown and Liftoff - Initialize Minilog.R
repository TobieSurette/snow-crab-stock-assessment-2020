library(gulf)

year <- 2013
x <- read.scset(year = year, valid = 1)

res <- data.frame(year = year, tow.id = x$tow.id, touchdown = NA, liftoff = NA)

clg()
windows(width = 11, height = 8.5)
m <- kronecker(matrix(1), matrix(1, nrow = 4, ncol = 5))
m <- cbind(0, m, 0)
m <- rbind(0, m, 0)
layout(m)
par(mar = c(0, 0, 0, 0))
tows <- which(is.na(res$touchdown) | is.na(res$liftoff) | res$touchdown == "        " | res$liftoff == "        ")
for (i in setdiff(tows, 1:284)){
   s <- read.minilog(year = year, tow.id = x$tow.id[i])
   end.time <- time2min(end.time(x[i,]), start.time(x[i,]))
   depth <- -depth(longitude(x[i,]), latitude(x[i,])) 
   s$time <- time2min(time(s), start.time(x[i,]))
   s$depth <- depth * s$depth / mean(s$depth[(s$time > 0) & (s$time < end.time)])
   xlim <- s$time[range(which(s$depth > (depth-30)))]

   # Depth plots:
   plot(xlim, range(s$depth[which(s$depth > (depth-30))]), type = "n", xaxs = "i", xlab = "", ylab = "", xaxt = "n")
   grid()
   lines(s$time, s$depth, col = "blue", lwd = 2)

   mtext(paste0(i, ") ", x$tow.id[i]), 3, 2.5, cex = 1.5)
   mtext("Depth (meters)", 2, 2.5, cex = 1.2)   
    
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


