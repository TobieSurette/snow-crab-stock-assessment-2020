library(gulf)
library(TMB)
setwd("U:/Snow Crab/Stock Assessment 2019")
 
year <- 2019
   
source("C:/gulf package/gulf/R/liftoff.R")
source("C:/gulf package/gulf/R/liftoff.scset.R")
source("C:/gulf package/gulf/R/read.flowmeter.R")

# These are relative to the LOGBOOK start times:
x <- read.scset(year = year, valid = 1)
times <- read.csv(file = "U:/Snow Crab/Stock Assessment 2019/Touchdown and Liftoff Times 2019.csv", header = TRUE, stringsAsFactors = FALSE)

tmp <- x$start.time
x$start.time <- x$start.time.logbook 
x$end.time <- x$end.time.logbook
x$liftoff.time <- "        "
index <- match(x$tow.id, times$tow.id)
x$liftoff.time <- substr(start.time(x) + times$liftoff[index] * 60, 12, 20)
x$start.time <- tmp


tows <- substr(x$tow.id[substr(x$tow.id,2,2) == "C"], 3, 5)

clg()
plot(c(-1200, 1200), c(-1000, 1800), type = "n", xlab = "", ylab = "", cex.lab = 1.25, xaxs = "i", yaxs = "i")
mtext("x (meters)", 1, 2.25, cex = 1.25)
mtext("y (meters)", 2, 2.25, cex = 1.25) 

grid()
distance <- rep(NA, length(tows))
names(distance) <- tows
delta <- distance
duration <- NULL

for (i in 1:length(tows)){
   tow.id <- x$tow.id[substr(x$tow.id, 3, 5) %in% tows[i]][1]
   e <- read.esonar(year = year, tow.id = tow.id)
   e$time <- time2min(time(e), start.time(x[x$tow.id == tow.id, ]))
   stop.time <- time2min(end.time(x[x$tow.id == tow.id, ]), start.time(x[x$tow.id == tow.id, ]))
   liftoff.time <- time2min(liftoff(x[x$tow.id == tow.id, ]), start.time(x[x$tow.id == tow.id, ]))
   
   index <- which((e$time >= 0) & (e$time <= stop.time))

   tmp <- deg2km(e$longitude[index], e$latitude[index])
   xx <- tmp$x[round(nrow(tmp)/2)]
   yy <- tmp$y[round(nrow(tmp)/2)]
   tmp$x <- 1000*(tmp$x - xx)
   tmp$y <- 1000*(tmp$y - yy)
   tmp$time <- round(60 * e$time[index])
   
   theta <- mean(atan2(diff(predict(lm(y~time,data=tmp))), diff(predict(lm(x~time,data=tmp))))) + pi / 2 
   #theta <- median(atan2(diff(tmp$y), diff(tmp$x))) + theta + pi / 2
   
   R <- matrix(c(cos(-theta), sin(-theta), -sin(-theta), cos(-theta)), nrow = 2, byrow = TRUE)
   res <- as.matrix(tmp[c("x", "y")]) %*% R
  
   lines(-res[, 1], res[, 2], col = "red", lwd = 2)
   
   tow.id <- x$tow.id[substr(x$tow.id, 3, 5) %in% tows[i]][2]
   e <- read.esonar(year = year, tow.id = tow.id)
   e$time <- time2min(time(e), start.time(x[x$tow.id == tow.id, ]))
   stop.time <- time2min(end.time(x[x$tow.id == tow.id, ]), start.time(x[x$tow.id == tow.id, ]))
   liftoff.time <- time2min(liftoff(x[x$tow.id == tow.id, ]), start.time(x[x$tow.id == tow.id, ]))
   
   index <- which((e$time >= 0) & (e$time <= stop.time))

   tmp <- deg2km(e$longitude[index], e$latitude[index])
   tmp$x <- 1000*(tmp$x - xx)
   tmp$y <- 1000*(tmp$y - yy)

   res <- as.matrix(tmp[c("x", "y")]) %*% R
      
   lines(-res[, 1], res[, 2], col = "grey40", lwd = 2)
   text(mean(-res[, 1]), mean(res[, 2]), substr(tow.id,3,5), pos = 2, cex = 0.65)
   if (e$day[1] >= 24) points(mean(-res[, 1]), mean(res[, 2]), pch = 21, bg = "grey")

   distance[i] <- sqrt(mean(-res[, 1])^2  + mean(res[, 2])^2)
   
   delta[i] <- as.numeric(difftime(start.time(x[x$tow.id == x$tow.id[substr(x$tow.id, 3, 5) %in% tows[i]][1], ]),
                                   start.time(x[x$tow.id == x$tow.id[substr(x$tow.id, 3, 5) %in% tows[i]][2], ]), units = "sec"))  
                                   
   duration <- rbind(duration, c(60* duration(x[x$tow.id == x$tow.id[substr(x$tow.id, 3, 5) %in% tows[i]][1], ]),
                                 60* duration(x[x$tow.id == x$tow.id[substr(x$tow.id, 3, 5) %in% tows[i]][2], ]))) 
                                                             
}
points(0,0, pch = 21, bg = "red", cex = 1.25)
box()

#==============================================================================================================
clg()
plot(c(-1200, 1200), c(-1000, 1800), type = "n", xlab = "", ylab = "", cex.lab = 1.25, xaxs = "i", yaxs = "i")
mtext("x (meters)", 1, 2.25, cex = 1.25)
mtext("y (meters)", 2, 2.25, cex = 1.25) 

grid()
distance <- rep(NA, length(tows))
names(distance) <- tows
delta <- distance

# End of tow stats:

clg()
bottom.time <- NULL
speed <- NULL
length <- NULL
length.bottom <- NULL
flow <- matrix(NA, nrow = length(tows), ncol = 2)
flow.bottom <- matrix(NA, nrow = length(tows), ncol = 2)
speed.bottom <- matrix(NA, nrow = length(tows), ncol = 2)
plot(c(-2, 2), c(0, 2.5), type = "n", xlab = "", ylab = "", xaxs = "i", yaxs = "i")
for (i in 1:length(tows)){
   tow.id <- x$tow.id[substr(x$tow.id, 3, 5) %in% tows[i]]
   e1 <- read.esonar(year = year, tow.id = tow.id[1])
   e2 <- read.esonar(year = year, tow.id = tow.id[2])
   
   e1$time <- time2min(time(e1), start.time(x[x$tow.id == tow.id[1], ]))
   e2$time <- time2min(time(e2), start.time(x[x$tow.id == tow.id[1], ]))
   
   start.time1 <- 0
   start.time2 <- time2min(start.time(x[x$tow.id == tow.id[2], ]), start.time(x[x$tow.id == tow.id[1], ]))
   
   stop.time1 <- time2min(end.time(x[x$tow.id == tow.id[1], ]), start.time(x[x$tow.id == tow.id[1], ]))
   stop.time2 <- time2min(end.time(x[x$tow.id == tow.id[2], ]), start.time(x[x$tow.id == tow.id[1], ]))

   liftoff.time1 <- time2min(liftoff(x[x$tow.id == tow.id[1], ]), start.time(x[x$tow.id == tow.id[1], ]))
   liftoff.time2 <- time2min(liftoff(x[x$tow.id == tow.id[2], ]), start.time(x[x$tow.id == tow.id[1], ]))

   bottom.time <- rbind(bottom.time, c(liftoff.time2 - stop.time2, liftoff.time1 - stop.time1))
   
   speed <- rbind(speed, c(mean(e1$speed[(e1$time >= start.time1) & (e1$time <= stop.time1)], na.rm = TRUE),
                           mean(e2$speed[(e2$time >= start.time2) & (e2$time <= stop.time2)], na.rm = TRUE)))  
   
   
   index <- which((e1$time >= start.time1) & (e1$time <= stop.time1))
   tmp1 <- deg2km(e1$longitude[index], e1$latitude[index])
   index <- which((e2$time >= start.time2) & (e2$time <= stop.time2))
   tmp2 <- deg2km(e2$longitude[index], e2$latitude[index])
   
   length <- rbind(length, c(1000 *sum(sqrt(diff(tmp1$x)^2 + diff(tmp1$y)^2)), 1000 *sum(sqrt(diff(tmp2$x)^2 + diff(tmp2$y)^2))))
   
   index <- which((e1$time >= stop.time1) & (e1$time <= liftoff.time1))
   tmp1 <- deg2km(e1$longitude[index], e1$latitude[index])
   index <- which((e2$time >= stop.time2) & (e2$time <= liftoff.time2))
   tmp2 <- deg2km(e2$longitude[index], e2$latitude[index])
   
   length.bottom <- rbind(length.bottom, c(1000 *sum(sqrt(diff(tmp1$x)^2 + diff(tmp1$y)^2)), 1000 *sum(sqrt(diff(tmp2$x)^2 + diff(tmp2$y)^2))))   
   
   f1 <- read.flowmeter(year = 2019, tow.id = tow.id[1])
   f2 <- read.flowmeter(year = 2019, tow.id = tow.id[2])
   if (!is.null(f1)){
      f1$time <- time2min(time(f1), start.time(x[x$tow.id == tow.id[1], ]))
      index <- which((f1$time >= start.time1) & (f1$time <= stop.time1))
      flow[i,1] <- mean(f1$"Speed.(m/s)"[index])
               
      index <- which((f1$time >= stop.time1) & (f1$time <= liftoff.time1))
      flow.bottom[i,1] <- mean(f1$"Speed.(m/s)"[index])
      
      index <- which((f1$time >= start.time1) & (f1$time <= liftoff.time1))
      lines(f1$time[index] - stop.time1, 1 + f1$"Speed.(m/s)"[index] - mean(f1$"Speed.(m/s)"[which((f1$time >= start.time1) & (f1$time <= stop.time1))])) 
   } 
   if (!is.null(f2)){
      f2$time <- time2min(time(f2), start.time(x[x$tow.id == tow.id[2], ]))
      index <- which((f2$time >= start.time2) & (f2$time <= stop.time2))
      flow[i,2] <- mean(f2$"Speed.(m/s)"[index])

      
      lines(f2$time[index] - stop.time1, f2$"Speed.(m/s)"[index])
      
      index <- which((f2$time >= stop.time2) & (f2$time <= liftoff.time2))
      flow.bottom[i,2] <- mean(f2$"Speed.(m/s)"[index])  
      
      index <- which((f2$time >= start.time2) & (f2$time <= liftoff.time2))
      lines(f2$time[index] - stop.time2, 1 + f2$"Speed.(m/s)"[index] - mean(f2$"Speed.(m/s)"[which((f2$time >= start.time2) & (f2$time <= stop.time2))]))    
   }   
}

speed.bottom <- length.bottom / (60*bottom.time)

# Calculate passive speed:
plot(as.numeric(0.514444 * speed * flow.bottom  / flow))

# Calculate passive distance:
plot(as.numeric(60 * bottom.time * 0.514444 * speed * flow.bottom  / flow))

