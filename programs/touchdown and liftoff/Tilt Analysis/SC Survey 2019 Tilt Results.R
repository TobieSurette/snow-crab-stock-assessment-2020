library(gulf)

source("C:/gulf package/gulf/R/liftoff.scset.R")

year <- 2019 

# Read revised start-end times:
res <- read.table("U:/Snow Crab/Stock Assessment 2019/Touchdown and Liftoff Times 2019.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

# Read set data and import revised times:
s <- read.scset(year = year, valid = 1)
s <- s[substr(s$tow.id, 2, 2) != "C", ]

tmp <- s$start.time
s$start.time <- s$start.time.logbook 
s$end.time <- s$end.time.logbook
s$liftoff.time <- "        "

index <- match(s$tow.id, res$tow.id)
s$liftoff.time <- substr(start.time(s) + res$liftoff[index] * 60, 12, 20)
s$start.time <- tmp

# Bottom temperatures:
s$temperature <- NA
for (i in 1:nrow(s)){
   cat(paste0(i, ") ", s$tow.id[i], "\n"))
   h <- read.star.oddi(year = year, tow.id = s$tow.id[i], type = "depth") 
   h$time <- time2min(time(h), start.time(s[i, ]))
   stop.time <- time2min(end.time(s[i, ]), start.time(s[i, ]))
   s$temperature[i] <- mean(h$temperature[h$time > (stop.time-2) & h$time <= stop.time], na.rm = TRUE)
}

# Bottom temperatures:
s$temperature <- NA
for (i in 1:nrow(s)){
   cat(paste0(i, ") ", s$tow.id[i], "\n"))
   h <- read.star.oddi(year = year, tow.id = s$tow.id[i], type = "depth") 
   h$time <- time2min(time(h), start.time(s[i, ]))
   stop.time <- time2min(end.time(s[i, ]), start.time(s[i, ]))
   s$temperature[i] <- mean(h$temperature[h$time > (stop.time-2) & h$time <= stop.time], na.rm = TRUE)
}


h <- read.star.oddi(year = year, tow.id = "GP111F", type = "depth")
h$time <- time2min(time(h), start.time(s[s$tow.id == "GP111F", ]))
plot(h$time, h$temperature, xlim = c(0, 6), ylim = c(-1, 3))

# Plot wing spread and warp ratio:
clg()
plot(c(2.3, 3.5), c(0, 15), type = "n", xaxs = "i", yaxs = "i", xlab = "Warp/Depth ratio", ylab = "Wing sspread(m)")
grid()
for (i in 1:nrow(s)){
   e <- read.esonar(year = year, tow.id = s$tow.id[i], type = "depth") 
   e$time <- time2min(time(e), start.time(s[i, ]))
   
   stop.time <- time2min(end.time(s[i, ]), start.time(s[i, ]))
   
   w <- e$doormaster[e$time >= 0 & e$time <= stop.time]


   depth <- (median(e$depth[e$time >= 0 & e$time <= stop.time], na.rm = TRUE) / 1.829) + 1
   print(depth)
   
   points(rnorm(length(w), s$warp[i] / depth, 0.01), w, pch = 21, bg = "grey", cex = 0.3)
}
box()

# Plot wing spread and depth:
clg()
plot(c(30, 120), c(0, 15), type = "n", xaxs = "i", yaxs = "i", xlab = "Depth(m)", ylab = "Wing spread(m)")
grid()
for (i in 1:nrow(s)){
   e <- read.esonar(year = year, tow.id = s$tow.id[i], type = "depth") 
   e$time <- time2min(time(e), start.time(s[i, ]))
   e <- e[!is.na(e$doormaster), ]
   e$depth <- -depth(e$longitude, e$latitude)
   
   
   stop.time <- time2min(end.time(s[i, ]), start.time(s[i, ]))
   depth <- mean(-depth(e$longitude[e$time >= 0 & e$time <= stop.time], e$latitude[e$time >= 0 & e$time <= stop.time]))

   points(jitter(e$depth, amount = 0.5), e$doormaster, pch = 21, bg = "grey", cex = 0.3)
}
box()

# Search for evidence of lifting during trawling:
clg()
plot(c(0, 2), c(-0.18, 0.18), type = "n", xaxs = "i", yaxs = "i")
for (i in 111:120){
   e <- read.esonar(year = year, tow.id = s$tow.id[i], type = "depth") 
   h <- read.star.oddi(year = year, tow.id = s$tow.id[i], type = "depth") 
   f <- read.star.oddi(year = year, tow.id = s$tow.id[i], type = "tilt") 

   # Time conversion:
   e$time <- time2min(time(e), start.time(s[i, ]))
   h$time <- time2min(time(h), start.time(s[i, ]))
   f$time <- time2min(time(f), start.time(s[i, ]))
   
   stop.time <- time2min(end.time(s[i, ]), start.time(s[i, ]))
   
   # Convert pressure to estimated depth:
   tt <- h$time[h$time >= 0 & h$time <= stop.time]
   pp <- h$pressure[h$time >= 0 & h$time <= stop.time]
   depth <- mean(-depth(e$longitude[e$time >= 0 & e$time <= stop.time], e$latitude[e$time >= 0 & e$time <= stop.time]))
   h$depth <- (h$pressure / mean(pp[tt >= 0 & tt <= stop.time])) * depth - depth 
   
   tt <- f$time[f$time >= 0 & f$time <= stop.time]
   pp <- f$pressure[f$time >= 0 & f$time <= stop.time]
   f$depth <- (f$pressure / mean(pp[tt >= 0 & tt <= stop.time])) * depth - depth 
   
   vv <- e$speed[e$time >= 0 & e$time <= stop.time]   
   vv <- vv - mean(vv)
   hh <- h$depth[h$time >= 0 & h$time <= stop.time]
   hh <- hh - mean(hh)
   ff <- f$depth[f$time >= 0 & f$time <= stop.time]
   ff <- ff - mean(ff)
   
   tt <-  f$time[f$time >= 0 & f$time <= stop.time]
   vvv <- rep(NA, length(tt))
   aaa <- vvv
    
   # Calculate 2nd-order derivative:
   k <- 13
   n <- length(tt)
   for (j in 1:(n-k)){
      model <- lm(ff[j:(j+k)] ~ I(tt[j:(j+k)]))
      print(model)
      vvv[j+(k-1)/2] <- as.numeric(coef(model)[2])
   }
   #plot(tt, vv, type= "l")
   for (j in 1:(n-k)){
      model <- lm(vvv[j:(j+k)] ~ I(tt[j:(j+k)]))
      aaa[j+(k-1)/2] <- as.numeric(coef(model)[2])
   }  
   
   windows()
   plot(c(0, 5), c(-3, 3), type = "n", xlab = "Time(min)", ylab = "Value", main = s$tow.id[i])
   lines(e$time[e$time >= 0 & e$time <= stop.time], 5*vv, lwd = 2, col = "blue")
   #lines(h$time[h$time >= 0 & h$time <= stop.time], hh, lwd = 2, col = "red")
   lines(f$time[f$time >= 0 & f$time <= stop.time], ff, lwd = 2, col = "green")
   lines(tt, vvv, lwd = 2, col = "red", lty = "dashed")
   
   lines(tt, aaa/20, lwd = 2, col = "purple")
   
   windows()
   #spec.pgram(vv, log = "no", xlim = c(0, 0.2))
   
   spec.pgram(aaa[!is.na(vvv)], log = "no", xlim = c(0, 0.2))
}


# Determine bottom temperatures:
clg()
plot(c(0, 2), c(-0.18, 0.18), type = "n", xaxs = "i", yaxs = "i")
for (i in 1:nrow(s)){
   print(i)
   h <- read.star.oddi(year = year,  tow.id = s$tow.id[i], type = "depth") 
  # f <- read.star.oddi(year = year, tow.id = s$tow.id[i], type = "tilt")
   m <- read.minilog(year = year, tow.id = s$tow.id[i])
      
   h$time <- time2min(time(h), start.time(s[i, ]))
   #f$time <- time2min(time(f), start.time(s[i, ]))
   m$time <- time2min(time(m), start.time(s[i, ]))
   
   end.time <- time2min(end.time(s[i, ]), start.time(s[i, ]))
   
   # Convert pressure to estimated depth:
   h.index <- (h$time >= (min(end.time, 5) - 2)) & (h$time <= min(end.time, 5))
   #f.index <- (f$time >= (min(end.time, 5) - 1)) & (f$time <= min(end.time, 5))
   m.index <- (m$time >= (min(end.time, 5) - 2)) & (m$time <= min(end.time, 5))
     
   s$bottom.temperature.star.oddi.headline[i] <- mean(h$temperature[h.index], na.rm = TRUE)
   #s$bottom.temperature.star.oddi.footrope[i] <- mean(f$temperature[f.index], na.rm = TRUE)
   s$bottom.temperature.minilog[i] <- mean(m$temperature[m.index], na.rm = TRUE)
  
   s$bottom.temperature.star.oddi.headline.slope[i] <- as.numeric(coef(lm(h$temperature[h.index] ~ h$time[h.index]))[2])
  # s$bottom.temperature.star.oddi.footrope.slope[i] <- mean(f$temperature[f.index], na.rm = TRUE)
   
   yy <- m$temperature[h.index]
   xx <- m$time[h.index]
   model <- gam(yy ~ s(xx))
   
   s$bottom.temperature.minilog[i] <- as.numeric(predict(model, newdata = list(xx = end.time)))
   
   s$bottom.temperature.minilog.slope[i] <- as.numeric(coef(model)[2])
   
   lines(h$time[h.index] - (min(end.time, 5) - 2), h$temperature[h.index] - s$bottom.temperature.star.oddi.headline[i], col = "blue", lwd = 0.5)
  # lines(f$time[f.index], f$temperature[f.index], col = "green", lwd = 0.5)
   lines(m$time[m.index] - (min(end.time, 5) - 2), m$temperature[m.index] - s$bottom.temperature.minilog[i], col = "red", lwd = 0.5)
}
plot(s$bottom.temperature, s$bottom.temperature.star.oddi.footrope)
plot(s$bottom.temperature, s$bottom.temperature.star.oddi.headline)
plot(s$bottom.temperature, s$bottom.temperature.minilog)
plot(s$bottom.temperature.star.oddi.headline, s$bottom.temperature.minilog)
abline(0, 1, col = "red")
plot(s$bottom.temperature.star.oddi.headline - s$bottom.temperature.minilog)
plot(s$bottom.temperature.star.oddi.headline, s$bottom.temperature.star.oddi.headline - s$bottom.temperature.minilog)
hline(0, col = "red")      

plot(-depth(longitude(s), latitude(s)), s$bottom.temperature.star.oddi.headline - s$bottom.temperature.minilog, ylim = c(-0.5, 0.5))

plot(-depth(longitude(s), latitude(s)), s$bottom.temperature.minilog.slope, 
     pch = 21, bg = "grey", ylim = c(-0.5, 0.2), xlab = "Depth (meters)", ylab = "Degrees / minute", cex.lab = 1.5)
grid()     
hline(0, col = "red", lwd = 2)
hline(0, col = "red")
plot(-depth(longitude(s), latitude(s)), s$bottom.temperature.star.oddi.headline.slope, ylim = c(-0.5, 0.2))
 
# Compile vessel speed stats:
speed <- matrix(0, nrow = 40, ncol = 7*60+1)
rownames(speed) <- as.character(seq(0.1, 4.0, by = 0.1))
colnames(speed) <- 0:(7*60)
for (i in 1:nrow(s)){
   print(i)
   e <- read.esonar(year = year, tow.id = s$tow.id[i])
   e$time <- time2min(time(e), start.time(s[i, ]))
 
   end.time <- time2min(end.time(s[i, ]), start.time(s[i, ]))
   tt <- e$time[e$time >= 0 & e$time <= end.time]
   ss <- e$speed[e$time >= 0 & e$time <= end.time]
   
   for (j in 1:length(tt)){
      ii <- as.character(ss[j])
      jj <- as.character(round(tt[j]*60))
      if ((ii %in% rownames(speed)) & (jj %in% colnames(speed))) speed[ii, jj] <- speed[ii, jj] + 1
   }
}

# Image plot for speed:
image(as.numeric(colnames(speed)), as.numeric(rownames(speed)), t(speed), xlab = "", ylab = "", col = grey(seq(1, 0, len = 100)), xlim = c(0, 300), xaxs = "i", ylim = c(0.5, 3.5), yaxs = "i")
mtext("Time (seconds)", 1, 2.5, cex = 1.5)
mtext("Speed (knots)", 2, 2.5, cex = 1.5)
#lines(par("usr")[1:2], rep(2, 2), col = "red", lwd = 2, lty = "dashed")
box()
index <- which(speed > 0, arr.ind = TRUE)
v <- cbind(as.numeric(rownames(speed)[index[, 1]]), as.numeric(colnames(speed)[index[, 2]]))
v <- v[(v[,1] <= 1.7) | (v[,1] >= 2.5), ]
#points(v[,2], v[,1], pch = 21, bg = "grey75", cex = 0.5, col = "grey60")

prc <- function(x,  p){
   if (all(is.na(x))) return(NA*p)
   return(approx(x, as.numeric(names(x)), p)$y)
}
res <- NULL
for (i in 1:ncol(speed)){
   res <- rbind(res, prc(cumsum(speed[, i] / sum(speed[, i])), p = c(0.025, 0.25, .5, 0.75, 0.975)))
}  
lines(as.numeric(dimnames(speed)[[2]]), res[,1], lwd = 1, lty = "dotted", col = "red")
lines(as.numeric(dimnames(speed)[[2]]), res[,5], lwd = 1, lty = "dotted", col = "red")
lines(as.numeric(dimnames(speed)[[2]]), res[,2], lwd = 2, lty = "dashed", col = "red")
lines(as.numeric(dimnames(speed)[[2]]), res[,4], lwd = 2, lty = "dashed", col = "red")
lines(as.numeric(dimnames(speed)[[2]]), res[,3], lwd = 2, lty = "solid", col = "red")

# Compile vessel speed stats at the end of tow:
end.speed <- matrix(0, nrow = 41, ncol = 7*60+1)
rownames(end.speed) <- as.character(seq(0.0, 4.0, by = 0.1))
colnames(end.speed) <- 0:(7*60)
for (i in 1:nrow(s)){
   print(i)
   e <- read.esonar(year = 2019, tow.id = s$tow.id[i])
   e$time <- time2min(time(e), end.time(s[i, ]))
 
   liftoff.time <- time2min(liftoff(s[i, ]), end.time(s[i, ]))
   tt <- e$time[e$time >= 0 & e$time <= liftoff.time]
   ss <- e$speed[e$time >= 0 & e$time <= liftoff.time]
   
   if (length(tt) > 0){
      for (j in 1:length(tt)){
         ii <- as.character(ss[j])
         jj <- as.character(round(tt[j]*60))
         if ((ii %in% rownames(end.speed)) & (jj %in% colnames(end.speed))) end.speed[ii, jj] <- end.speed[ii, jj] + 1
      }
   }
}

# Image plot for end speed:
tmp <- end.speed
end.speed <- tmp
end.speed <- t(end.speed)
end.speed <- end.speed / t(repvec(apply(end.speed, 1, sum), 41))
image(as.numeric(rownames(end.speed)), as.numeric(colnames(end.speed)), end.speed, 
      xlab = "", ylab = "", col = grey(seq(1, 0, len = 100)), xlim = c(0, 120), xaxs = "i", ylim = c(0.0, 3.5), yaxs = "i")
mtext("Time (seconds)", 1, 2.5, cex = 1.5)
mtext("Speed (knots)", 2, 2.5, cex = 1.5)
lines(par("usr")[1:2], rep(2, 2), col = "red", lwd = 2, lty = "dashed")
box()

# Plot vessel tracks by the origin:
plot(c(-450, 450), c(-450, 450), xlab = "", ylab = "", type = "n")
angles <- rep(NA, nrow(s))
for (i in 1:nrow(s)){
   print(i)
   e <- read.esonar(year = year, tow.id = s$tow.id[i])
   e$time <- time2min(time(e), start.time(s[i, ]))
 
   end.time <- time2min(end.time(s[i, ]), start.time(s[i, ]))
   liftoff.time <- time2min(liftoff(s[i, ]), start.time(s[i, ]))
       
   tt <- e$time[e$time >= 0 & e$time <= end.time]
   tmp <- deg2km(e$longitude[e$time >= 0 & e$time <= end.time], 
                 e$latitude[e$time >= 0 & e$time <= end.time])
   origin <- c(tmp$x[1], tmp$y[1])
   tmp$x <- (tmp$x-origin[1])*1000
   tmp$y <- (tmp$y-origin[2])*1000
    
   px <- predict(gam(tmp$x ~ s(tt)))
   py <- predict(gam(tmp$y ~ s(tt)))
   angles[i] <- median(atan2(py, px))
   
   lines(tmp$x, tmp$y, col = "grey50")
   
   # Plot extra-bottom time:
   tt <- e$time[e$time >= end.time & e$time <= liftoff.time]
   if (length(tt) > 0){
      tmp <- deg2km(e$longitude[e$time >= end.time & e$time <= liftoff.time], 
                    e$latitude[e$time >= end.time & e$time <= liftoff.time])
      tmp$x <- (tmp$x-origin[1])*1000
      tmp$y <- (tmp$y-origin[2])*1000
      lines(tmp$x, tmp$y, col = "red")   
   }
}
mtext("x (meters)", 1, 2.5, cex = 1.8)
mtext("y (meters)", 2, 2.5, cex = 1.8)

windows()
hist(angles * 180 / pi, n = 100, col = "grey")

# Plot vessel tracks by the end point:
plot(c(-50, 50), c(-50, 50), xlab = "", ylab = "", type = "n")
angles <- rep(NA, nrow(s))
xx <- -400:400
yy <- xx
m <- matrix(0, nrow = length(xx), ncol = length(yy))
rownames(m) <- as.character(xx)
colnames(m) <- as.character(yy)
for (i in 1:nrow(s)){
   print(i)
   e <- read.esonar(year = year, tow.id = s$tow.id[i])
   e$time <- time2min(time(e), start.time(s[i, ]))
 
   end.time <- time2min(end.time(s[i, ]), start.time(s[i, ]))
   liftoff.time <- time2min(liftoff(s[i, ]), start.time(s[i, ]))
       
   tt <- e$time[e$time >= 0 & e$time <= end.time]
   tmp <- deg2km(e$longitude[e$time >= 0 & e$time <= end.time], 
                 e$latitude[e$time >= 0 & e$time <= end.time])
   origin <- c(tmp$x[nrow(tmp)], tmp$y[nrow(tmp)])
   tmp$x <- (tmp$x-origin[1])*1000
   tmp$y <- (tmp$y-origin[2])*1000
    
   px <- predict(gam(tmp$x ~ s(tt)))
   py <- predict(gam(tmp$y ~ s(tt)))
   angles[i] <- median(atan2(py[(tt >= (end.time - 1)) & (tt <= end.time)], 
                             px[(tt >= (end.time - 1)) & (tt <= end.time)]))
     
   # Define rotation matrix:
   R <- matrix(c(cos(-angles[i]), sin(-angles[i]), 
                -sin(-angles[i]), cos(-angles[i])), ncol = 2, byrow = TRUE)
   
   # Perform rotation:
   b <- as.matrix(tmp) %*% R
   lines(-b[, 1], b[, 2], col = "grey50")  
   
   # Plot extra-bottom time:
   index <- which(e$time >= end.time & e$time <= liftoff.time)
   if (length(index) > 0){
      tt <- e$time[index]
      tmp <- deg2km(e$longitude[index], 
                    e$latitude[index])
      tmp$x <- (tmp$x-origin[1])*1000
      tmp$y <- (tmp$y-origin[2])*1000
      b <- as.matrix(tmp) %*% R
      lines(-b[, 1], b[, 2], col = "red")
      
      if (length(tt) > 0){
         for (j in 1:length(tt)){
            ii <- as.character(round(-b[j, 1]))
            jj <- as.character(round(b[j, 2]))
            if ((ii %in% rownames(m)) & (jj %in% colnames(m))) m[ii, jj] <- m[ii, jj] + 1
         }
      }      
   }
}
mtext("x (meters)", 1, 2.5, cex = 1.8)
mtext("y (meters)", 2, 2.5, cex = 1.8)

# Image plot for end speed:
windows()
image(as.numeric(colnames(m)), as.numeric(rownames(m)), m, 
      xlab = "", ylab = "", col = grey(seq(1, 0, len = 100)), xaxs = "i", yaxs = "i", xlim = c(-20, 50), ylim = c(-20, 20))
mtext("X (meters)", 1, 2.5, cex = 1.5)
mtext("Y (meters)", 2, 2.5, cex = 1.5)
lines(par("usr")[1:2], rep(0, 2), col = "red", lwd = 2, lty = "solid")
lines(rep(0, 2), par("usr")[3:4], col = "red", lwd = 2, lty = "solid")
index <- which(m > 0 & m <= 3, arr.ind = TRUE)
v <- cbind(as.numeric(rownames(m)[index[, 2]]), as.numeric(colnames(m)[index[, 1]]))
points(v[,2], v[,1], pch = 21, bg = "grey75", cex = 0.5, col = "grey60")
text(0, par("usr")[3] + 0.5 * -par("usr")[3], "End of tow", srt = 90, pos = 2, cex = 1.25)
text(par("usr")[1] + 0.2 * diff(par("usr")[1:2]), 0, "Direction of tow", srt = 0, pos = 3, cex = 1.25)
box()

# Plot vessel heading with reference to end point:
plot(c(0, 100), c(-60, 60), xlab = "", ylab = "", type = "n", xaxs = "i", yaxs = "i")
angles <- rep(NA, nrow(s))
xx <- -400:400
yy <- xx
m <- matrix(0, nrow = length(xx), ncol = length(yy))
rownames(m) <- as.character(xx)
colnames(m) <- as.character(yy)
for (i in 1:nrow(s)){
   print(i)
   e <- read.esonar(year = year, tow.id = s$tow.id[i])
   e$time <- time2min(time(e), start.time(s[i, ]))
 
   end.time <- time2min(end.time(s[i, ]), start.time(s[i, ]))
   liftoff.time <- time2min(liftoff(s[i, ]), start.time(s[i, ]))
       
   tt <- e$time[e$time >= 0 & e$time <= end.time]
   tmp <- deg2km(e$longitude[e$time >= 0 & e$time <= end.time], 
                 e$latitude[e$time >= 0 & e$time <= end.time])
   origin <- c(tmp$x[nrow(tmp)], tmp$y[nrow(tmp)])
   tmp$x <- (tmp$x-origin[1])*1000
   tmp$y <- (tmp$y-origin[2])*1000
    
   px <- predict(gam(tmp$x ~ s(tt)))
   py <- predict(gam(tmp$y ~ s(tt)))
   angles[i] <- median(atan2(py[(tt >= (end.time - 1)) & (tt <= end.time)], 
                             px[(tt >= (end.time - 1)) & (tt <= end.time)]))
     
   # Define rotation matrix:
   R <- matrix(c(cos(-angles[i]), sin(-angles[i]), 
                -sin(-angles[i]), cos(-angles[i])), ncol = 2, byrow = TRUE)
   
   # Perform rotation:
  # b <- as.matrix(tmp) %*% R
  # lines(-b[, 1], b[, 2], col = "grey50")  
   
   # Plot extra-bottom time:
   index <- which(e$time >= end.time & e$time <= liftoff.time)
   if (length(index) > 1){
      tt <- e$time[index]
      tmp <- deg2km(e$longitude[index], 
                    e$latitude[index])
      tmp$x <- (tmp$x-origin[1])*1000
      tmp$y <- (tmp$y-origin[2])*1000
      b <- as.matrix(tmp) %*% R
    #  lines(-b[, 1], b[, 2], col = "red")
      
      d <- cumsum(sqrt((-b[2:nrow(b), 1] + b[1:(nrow(b)-1), 1])^2 + (b[2:nrow(b), 2] - b[1:(nrow(b)-1), 2])^2))
      an <- atan2(b[, 2], -b[, 1])
      an <- an[2:length(an)]    
      lines(d, an * 180 / pi)
      
      #if (length(tt) > 0){
      #   for (j in 1:length(tt)){
      #      ii <- as.character(round(-b[j, 1]))
      #      jj <- as.character(round(b[j, 2]))
      #      if ((ii %in% rownames(m)) & (jj %in% colnames(m))) m[ii, jj] <- m[ii, jj] + 1
      #   }
      #}      
   }
}
lines(par("usr")[1:2], c(0,0), lwd = 2, col = "red")
mtext("Vessel distance (meters)", 1, 2.5, cex = 1.8)
mtext("Relative heading (degrees)", 2, 2.5, cex = 1.8)
box()

# Plot wing spread with reference to start point:
plot(c(0, 6), c(0, 16), xlab = "", ylab = "", type = "n", xaxs = "i", yaxs = "i", yaxt = "n")
grid()
for (i in 1:nrow(s)){
   print(i)
   e <- read.esonar(year = year, tow.id = s$tow.id[i])
   e$time <- time2min(time(e), start.time(s[i, ]))
 
   end.time <- time2min(end.time(s[i, ]), start.time(s[i, ]))
   liftoff.time <- time2min(liftoff(s[i, ]), start.time(s[i, ]))
   
   tt <- e$time[e$time >= 0 & e$time <= end.time]
   ww <- e$doormaster[e$time >= 0 & e$time <= end.time]
   points(tt, ww, pch = 21, bg = "grey", cex = 0.5)
}
#lines(par("usr")[1:2], c(0,0), lwd = 2, col = "red")
mtext("Time (min)", 1, 2.5, cex = 1.5)
mtext("Wing spread (meters)", 2, 2.5, cex = 1.8)
box()
axis(2, at = seq(0, 16, by = 2))

# Plot wing spread with reference to start point plus post-end observations:
plot(c(0, 7), c(0, 16), xlab = "", ylab = "", type = "n", xaxs = "i", yaxs = "i", yaxt = "n")
grid()
xx <- round(seq(-2, 7, by = 0.1),1)
yy <- round(seq(0, 15, by = 0.1),1)
m <- matrix(0, nrow = length(xx), ncol = length(yy))
rownames(m) <- as.character(xx)
colnames(m) <- as.character(yy)
for (i in 1:nrow(s)){
   print(i)
   e <- read.esonar(year = year, tow.id = s$tow.id[i])
   e$time <- time2min(time(e), start.time(s[i, ]))
 
   end.time <- time2min(end.time(s[i, ]), start.time(s[i, ]))
   liftoff.time <- time2min(liftoff(s[i, ]), start.time(s[i, ]))
   
   tt <- e$time[e$time >= 0 & e$time <= end.time]
   ww <- e$doormaster[e$time >= 0 & e$time <= end.time]
   points(tt, ww, pch = 21, bg = "grey", cex = 0.5)
   
   tt <- e$time[e$time >= end.time & e$time <= liftoff.time]
   ww <- e$doormaster[e$time >= end.time & e$time <= liftoff.time]
   points(tt, ww, pch = 21, bg = "red", cex = 0.75)   
   
   tt <- e$time[e$time >= -2 & e$time <= end.time]
   ww <- e$doormaster[e$time >= -2 & e$time <= end.time]
   tt <- tt[!is.na(ww)]
   ww <- ww[!is.na(ww)]
   
   if (length(tt) > 0){
      for (j in 1:length(tt)){
         ii <- as.character(round(tt[j],1))
         jj <- as.character(round(ww[j],1))
         if ((ii %in% rownames(m)) & (jj %in% colnames(m))) m[ii, jj] <- m[ii, jj] + 1
      }
   }   
}
#lines(par("usr")[1:2], c(0,0), lwd = 2, col = "red")
mtext("Time (min)", 1, 2.5, cex = 1.5)
mtext("Wing spread (meters)", 2, 2.5, cex = 1.8)
legend("topright", legend = c("Before End", "After End"), pch = 21, pt.cex = 2.5, pt.bg = c("grey", "red"), bg = "white", cex = 1.5)
box()
axis(2, at = seq(0, 16, by = 2))


tmp <- m
m <- tmp
m <- m / repvec(apply(m, 1, sum), ncol = ncol(m))
plot(c(0, 5), c(0, 15), xlab = "", ylab = "", type = "n", xaxs = "i", yaxs = "i")
grid()
image(as.numeric(rownames(m)), as.numeric(colnames(m)), m, col = grey(seq(1, 0, len = 100)), add = TRUE,
      xlab = "", ylab = "", xlim = c(-2, 7), xaxs = "i", ylim = c(-10, 10), zlim = c(0, 0.075),  yaxs = "i")
mtext("Time (min)", 1, 2.25, cex = 1.5)
mtext("Wing spread (meters)", 2, 2.25, cex = 1.5)
box()

prc <- function(x,  p) return(approx(x, as.numeric(names(x)), p)$y)
res <- NULL
for (i in 1:nrow(m)){
   res <- rbind(res, prc(apply(m, 1, cumsum)[,i], p = c(0.025, 0.25, .5, 0.75, 0.975)))
}  
lines(as.numeric(dimnames(m)[[1]]), res[,1], lwd = 1, lty = "dotted", col = "red")
lines(as.numeric(dimnames(m)[[1]]), res[,5], lwd = 1, lty = "dotted", col = "red")
lines(as.numeric(dimnames(m)[[1]]), res[,2], lwd = 2, lty = "dashed", col = "red")
lines(as.numeric(dimnames(m)[[1]]), res[,4], lwd = 2, lty = "dashed", col = "red")
lines(as.numeric(dimnames(m)[[1]]), res[,3], lwd = 2, lty = "solid", col = "red")

# Plot headline height with reference to start point plus post-end observations:
plot(c(0, 7), c(0, 5), xlab = "", ylab = "", type = "n", xaxs = "i", yaxs = "i", yaxt = "n")
grid()
xx <- round(seq(-2, 7, by = 0.1),1)
yy <- round(seq(0, 5, by = 0.1),1)
m <- matrix(0, nrow = length(xx), ncol = length(yy))
rownames(m) <- as.character(xx)
colnames(m) <- as.character(yy)
for (i in 1:nrow(s)){
   print(i)
   e <- read.esonar(year = year, tow.id = s$tow.id[i])
   e$time <- time2min(time(e), start.time(s[i, ]))
 
   end.time <- time2min(end.time(s[i, ]), start.time(s[i, ]))
   liftoff.time <- time2min(liftoff(s[i, ]), start.time(s[i, ]))
   
   tt <- e$time[e$time >= 0 & e$time <= end.time]
   ww <- e$headline[e$time >= 0 & e$time <= end.time]
   points(tt, ww, pch = 21, bg = "grey", cex = 0.5)
   
   tt <- e$time[e$time >= end.time & e$time <= liftoff.time]
   ww <- e$headline[e$time >= end.time & e$time <= liftoff.time]
   points(tt, ww, pch = 21, bg = "red", cex = 0.75) 
   
   tt <- e$time[e$time >= -2 & e$time <= end.time]
   ww <- e$headline[e$time >= -2 & e$time <= end.time]
   tt <- tt[!is.na(ww)]
   ww <- ww[!is.na(ww)]
   
   if (length(tt) > 0){
      for (j in 1:length(tt)){
         ii <- as.character(round(tt[j],1))
         jj <- as.character(round(ww[j],1))
         if ((ii %in% rownames(m)) & (jj %in% colnames(m))) m[ii, jj] <- m[ii, jj] + 1
      }
   }       
}
#lines(par("usr")[1:2], c(0,0), lwd = 2, col = "red")
mtext("Time (min)", 1, 2.5, cex = 1.5)
mtext("Headline height (meters)", 2, 2.5, cex = 1.8)
legend("topright", legend = c("Before End", "After End"), pch = 21, pt.cex = 2.5, pt.bg = c("grey", "red"), bg = "white", cex = 1.5)
box()
axis(2, at = seq(0, 5, by = 1))

tmp <- m
m <- tmp
m <- m / repvec(apply(m, 1, sum), ncol = ncol(m))
plot(c(0, 5), c(0, 5), xlab = "", ylab = "", type = "n", xaxs = "i", yaxs = "i")
grid()
image(as.numeric(rownames(m)), as.numeric(colnames(m)), m, col = grey(seq(1, 0, len = 100)), add = TRUE,
      xlab = "", ylab = "", xlim = c(-2, 7), xaxs = "i", ylim = c(0, 5), zlim = c(0, 0.2),  yaxs = "i")
mtext("Time (min)", 1, 2.25, cex = 1.5)
mtext("Headline height (meters)", 2, 2.25, cex = 1.5)
box()

prc <- function(x,  p){
   if (all(is.na(x))) return(NA*p)
   return(approx(x, as.numeric(names(x)), p)$y)
}
res <- NULL
for (i in 1:nrow(m)){
   res <- rbind(res, prc(apply(m, 1, cumsum)[,i], p = c(0.025, 0.25, .5, 0.75, 0.975)))
}  
lines(as.numeric(dimnames(m)[[1]]), res[,1], lwd = 1, lty = "dotted", col = "red")
lines(as.numeric(dimnames(m)[[1]]), res[,5], lwd = 1, lty = "dotted", col = "red")
lines(as.numeric(dimnames(m)[[1]]), res[,2], lwd = 2, lty = "dashed", col = "red")
lines(as.numeric(dimnames(m)[[1]]), res[,4], lwd = 2, lty = "dashed", col = "red")
lines(as.numeric(dimnames(m)[[1]]), res[,3], lwd = 2, lty = "solid", col = "red")

#
dbarplot(table(round(duration(s), 1)), xlab = "Trawl Duration (min)", ylab = "Frequency", width = 1, cex.lab = 1.5)

dbottom <- difftime(liftoff(s), end.time(s), units = "min")
dbottom <- dbottom[dbottom >= 0]
dbarplot(table(round(dbottom, 1)), xlab = "Bottom time after end (min)", ylab = "Frequency", width = 1, cex.lab = 1.5)

plot(-depth(longitude(s), latitude(s)), dbottom, xlab = "Depth (meters)", ylab = "Bottom time after end (min)", ylim = c(0, 11))

# Star-Oddi depth overlay at start of tow:
type = "depth"
plot(c(-2, 7), c(-15, 15), xlab = "", ylab = "", type = "n", xaxs = "i", yaxs = "i", yaxt = "n")
grid()
xx <- round(seq(-2, 7, by = 0.1),1)
yy <- round(seq(-15, 15, by = 0.1),1)
m <- matrix(0, nrow = length(xx), ncol = length(yy))
rownames(m) <- as.character(xx)
colnames(m) <- as.character(yy)
for (i in 1:nrow(s)){
   print(i)
   e <- read.esonar(year = year, tow.id = s$tow.id[i])
   e$time <- time2min(time(e), start.time(s[i, ]))
    
   o <- read.star.oddi(year = year, tow.id = s$tow.id[i], type = type)
   o$time <- time2min(time(o), start.time(s[i, ]))
 
   end.time <- time2min(end.time(s[i, ]), start.time(s[i, ]))
   liftoff.time <- time2min(liftoff(s[i, ]), start.time(s[i, ]))
   
   # Convert pressure to estimated depth:
   tt <- o$time[o$time >= 0 & o$time <= end.time]
   pp <- o$pressure[o$time >= 0 & o$time <= end.time]
   depth <- mean(-depth(e$longitude[e$time >= 0 & e$time <= 1], e$latitude[e$time >= 0 & e$time <= 1]))
   o$depth <- (o$pressure / mean(pp[tt >= 0 & tt <= 1])) * depth - depth 
   
   tt <- o$time[o$time >= -2 & o$time <= end.time]
   dd <- o$depth[o$time >= -2 & o$time <= end.time]
   points(tt, dd, pch = 21, bg = "grey", cex = 0.5)
   
   if (length(tt) > 0){
      for (j in 1:length(tt)){
         ii <- as.character(round(tt[j],1))
         jj <- as.character(round(dd[j],1))
         if ((ii %in% rownames(m)) & (jj %in% colnames(m))) m[ii, jj] <- m[ii, jj] + 1
      }
   }     
}
#lines(par("usr")[1:2], c(0,0), lwd = 2, col = "red")
mtext("Time (min)", 1, 2.5, cex = 1.5)
mtext("Wing spread (meters)", 2, 2.5, cex = 1.8)
legend("topright", legend = c("Before End", "After End"), pch = 21, pt.cex = 2.5, pt.bg = c("grey", "red"), bg = "white", cex = 1.5)
box()
axis(2, at = seq(0, 1, by = 0.2))

tmp <- m
m <- tmp
m <- m / repvec(apply(m, 1, sum), ncol = ncol(m))
plot(c(-2, 7), c(-15, 15), xlab = "", ylab = "", type = "n", xaxs = "i", yaxs = "i")
image(as.numeric(rownames(m)), as.numeric(colnames(m)), m, col = grey(seq(1, 0, len = 100)), add = TRUE,
      xlab = "", ylab = "", xlim = c(-2, 7), xaxs = "i", ylim = c(-10, 10), zlim = c(0, 0.27),  yaxs = "i")
mtext("Time (min)", 1, 2.5, cex = 1.5)
mtext("Relative depth (meters)", 2, 2.5, cex = 1.5)
lines(par("usr")[1:2], rep(0, 2), col = "red", lwd = 2, lty = "dashed")
grid()
box()

# Star-Oddi depth rate change (vertical acceleration) overlay at start of tow:
type = "depth"
k <- 13  # Smoothing window width in seconds.
xx <- round(seq(-2, 7, by = 0.05),2)
yy <- round(seq(-75, 75, by = 1),0)
A <- matrix(0, nrow = length(xx), ncol = length(yy))
rownames(A) <- as.character(xx)
colnames(A) <- as.character(yy)
V <- A
for (i in 1:nrow(s)){
   print(i)
   e <- read.esonar(year = year, tow.id = s$tow.id[i])
   e$time <- time2min(time(e), start.time(s[i, ]))
    
   o <- read.star.oddi(year = year, tow.id = s$tow.id[i], type = type)
   o$time <- time2min(time(o), start.time(s[i, ]))
 
   end.time <- time2min(end.time(s[i, ]), start.time(s[i, ]))
   liftoff.time <- time2min(liftoff(s[i, ]), start.time(s[i, ]))
   
   # Convert pressure to estimated depth:
   tt <- o$time[o$time >= 0 & o$time <= end.time]
   pp <- o$pressure[o$time >= 0 & o$time <= end.time]
   depth <- mean(-depth(e$longitude[e$time >= 0 & e$time <= 1], e$latitude[e$time >= 0 & e$time <= 1]))
   o$depth <- (o$pressure / mean(pp[tt >= 0 & tt <= 1])) * depth - depth 
      
   tt <- o$time[o$time >= -2 & o$time <= end.time]
   dd <- o$depth[o$time >= -2 & o$time <= end.time]
   vv <- rep(NA, length(tt))
   aa <- vv
    
   # Calculate 2nd-order derivative:
   n <- length(tt)
   for (j in 1:(n-k)){
      model <- lm(dd[j:(j+k)] ~ I(tt[j:(j+k)]))
      vv[j+(k-1)/2] <- as.numeric(coef(model)[2])
      model <- lm(dd[j:(j+k)] ~ I(tt[j:(j+k)]^2) + I(tt[j:(j+k)]))
   }
   #plot(tt, vv, type= "l")
   for (j in 1:(n-k)){
      model <- lm(vv[j:(j+k)] ~ I(tt[j:(j+k)]))
      aa[j+(k-1)/2] <- as.numeric(coef(model)[2])
   }   
   plot(tt, vv, type = "l", col = "blue", ylim = c(-80, 80), xlim = c(-2, 5), lwd = 2, xlab = "Time (minutes)", main = s$tow.id[i])
   grid()
   lines(tt, aa, lwd = 2, col = "green")
   hline(0, lwd = 2, lty = "dashed", col = "red")  
   vline(0, lwd = 2, lty = "dashed", col = "red")
   text(4, 60, paste(round(depth), " meters"))
   
   if (length(tt) > 0){
      for (j in 1:length(tt)){
         if (!is.na(aa[j])){
            ii <- as.character(round(round(tt[j] * 20) / 20, 2))
            jj <- as.character(round(vv[j],1))
            kk <- as.character(round(aa[j],0))
            if ((ii %in% rownames(V)) & (jj %in% colnames(V))) V[ii, jj] <- V[ii, jj] + 1
            if ((ii %in% rownames(A)) & (kk %in% colnames(A))) A[ii, kk] <- A[ii, kk] + 1
         }
      }
   }     
}

# Speed plot:
windows()
m <- V
m <- m / repvec(apply(m, 1, sum), ncol = ncol(m))
plot(c(-2, 7), c(-5, 40), xlab = "", ylab = "", type = "n", xaxs = "i", yaxs = "i", xlim = c(-2, 5),  yaxs = "i")
image(as.numeric(rownames(m)), as.numeric(colnames(m)), m, col = grey(seq(1, 0, len = 100)), add = TRUE,
      zlim = c(0, 0.70))
mtext("Time (min)", 1, 2.5, cex = 1.5)
mtext("Speed (meters / minute)", 2, 2.5, cex = 1.5)
hline(0, col = "red", lwd = 2, lty = "dashed")
vline(0, col = "red", lwd = 2, lty = "dashed")
grid()
box()

# Acceleration plot:
windows()
m <- A
m <- m / repvec(apply(m, 1, sum), ncol = ncol(m))
plot(c(-2, 7), c(-60, 60), xlab = "", ylab = "", type = "n", xaxs = "i", yaxs = "i", xlim = c(-2, 5),  yaxs = "i")
image(as.numeric(rownames(m)), as.numeric(colnames(m)), m, col = grey(seq(1, 0, len = 100)), add = TRUE,
      zlim = c(0, 0.06))
mtext("Time (min)", 1, 2.5, cex = 1.5)
mtext("Acceleration (meters / minute^2)", 2, 2.5, cex = 1.5)
hline(0, col = "red", lwd = 2, lty = "dashed")
vline(0, col = "red", lwd = 2, lty = "dashed")
grid()
box()

# Star-Oddi depth overlay at end of tow:
type = "depth"
xx <- round(seq(-2, 7, by = 0.1),1)
yy <- round(seq(-10, 10, by = 0.1),1)
m <- matrix(0, nrow = length(xx), ncol = length(yy))
rownames(m) <- as.character(xx)
colnames(m) <- as.character(yy)
for (i in 1:nrow(s)){
   print(i)
   e <- read.esonar(year = 2018, tow.id = s$tow.id[i])
   e$time <- time2min(time(e), liftoff(s[i, ]))
    
   o <- read.star.oddi(year = 2018, tow.id = s$tow.id[i], type = type)
   o$time <- time2min(time(o), liftoff(s[i, ]))
   
   tt <- o$time[o$time >= -1 & o$time <= 0]
   pp <- o$pressure[o$time >= -1 & o$time <= 0]
   depth <- mean(-depth(e$longitude[e$time >= -2 & e$time <= 0], e$latitude[e$time >= -2 & e$time <= 0]))
   o$depth <- (o$pressure / mean(pp)) * depth - depth 
   
   tt <- o$time[o$time >= -2 & o$time <= 7]
   dd <- o$depth[o$time >= -2 & o$time <= 7]
   
   if (length(tt) > 0){
      for (j in 1:length(tt)){
         ii <- as.character(round(tt[j],1))
         jj <- as.character(round(dd[j],1))
         if ((ii %in% rownames(m)) & (jj %in% colnames(m))) m[ii, jj] <- m[ii, jj] + 1
      }
   }     
}
tmp <- m
m <- m / repvec(apply(m, 1, sum), ncol = ncol(m))
plot(c(-2, 2), c(-10, 10), xlab = "", ylab = "", type = "n", xaxs = "i", yaxs = "i")
image(as.numeric(rownames(m)), as.numeric(colnames(m)), m, col = grey(seq(1, 0, len = 100)), add = TRUE,
      xlab = "", ylab = "", xlim = c(-2, 7), xaxs = "i", ylim = c(-10, 10), zlim = c(0, 0.32),  yaxs = "i")
mtext("Time (min)", 1, 2.5, cex = 1.5)
mtext("Relative depth (meters)", 2, 2.5, cex = 1.5)
lines(par("usr")[1:2], rep(0, 2), col = "red", lwd = 2, lty = "dashed")
grid()
box()

# Star-Oddi depth overlay at end of tow:
type = "acceleration"

# Accelerometer:
var <- "Zacc"
if (var == "G")    ylim = c(5, 15)
if (var == "Xacc") ylim = c(-10, 5)
if (var == "Yacc") ylim = c(-5, 10)
if (var == "Zacc") ylim = c(5, 15)
plot(c(-4, 2), ylim, type = "n", xlab = "", ylab = "")
for (i in 1:nrow(s)){
   end.time <- time2min(end.time(s[i, ]), end.time(s[i, ]))
   liftoff.time <- time2min(liftoff(s[i, ]), end.time(s[i, ]))      
   o <- read.star.oddi(year = 2018, tow.id = s$tow.id[i], type = type)
   o$time <- time2min(time(o), end.time(s[i, ]))
   points(jitter(o$time[o$time >= -4 & o$time < 0], amount = 1/120), o[o$time >= -4 & o$time < 0, var], pch = 21, bg = "grey", cex = 0.2)
   points(jitter(o$time[o$time >= 0 & o$time < 2], amount = 1/120), o[o$time >= 0 & o$time < 2, var], pch = 21, bg = "red", cex = 0.2)
}  
mtext("Time (min)", 1, 2.5, cex = 1.5)
mtext(var, 2, 2.5, cex = 1.5)
lines(par("usr")[1:2], rep(0, 2), col = "red", lwd = 2, lty = "dashed")
grid()
box()

