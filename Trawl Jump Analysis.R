library(gulf)

year <- 2019
s <- read.scset(year = year, valid = 1)

results <- s["tow.id"]
results$depth       <- NA
results$speed.mean  <- NA
results$speed.sd    <- NA
results$speed.min   <- NA
results$speed.max   <- NA
results$headline.sd <- NA
results$footrope.sd <- NA
results$tilt.mean   <- NA 
results$tilt.sd     <- NA 
results$tilt.slope  <- NA 
for (i in 1:nrow(s)){
   cat(paste0(i, ") ", s$tow.id[i], "\n"))
    
   e <- read.esonar(year = year, tow.id = s$tow.id[i])
   e <- e[!duplicated(time(e)), ]
   e$time <- time2min(time(e), start.time(s[i, ]))
   stop.time <- time2min(end.time(s[i, ]), start.time(s[i, ]))
   index <- e$time >= 0 & e$time <= stop.time
   depth <- median(e$depth[index], na.rm = TRUE)
   if (is.na(depth)) depth <- mean(-depth(e$longitude[index], e$latitude[index]), na.rm = TRUE)
   
   tmp <- deg2km(e$longitude, e$latitude)
   e$xkm <- tmp$x
   e$ykm <- tmp$y
   v <- 1.94384 * 1000 * sqrt((e$xkm[3:nrow(e)] - e$xkm[1:(nrow(e)-2)] )^2 + (e$ykm[3:nrow(e)] - e$ykm[1:(nrow(e)-2)] )^2) / (60*(e$time[3:nrow(e)] - e$time[1:(nrow(e)-2)]))
   e$speed <- NA
   e$speed[2:(nrow(e)-1)] <- v  
   h <- read.star.oddi(year = year, tow.id = s$tow.id[i], type = "depth")
   h$time <- time2min(time(h), start.time(s[i, ]))

   f <- read.star.oddi(year = year, tow.id = s$tow.id[i], type = "tilt")
   f$time <- time2min(time(f), start.time(s[i, ]))
 
   res <- data.frame(time = round(e$time[index] * 60 ),
                     speed = e$speed[index])

   res$headline <- h$pressure[match(res$time, round(h$time * 60))]
   res$headline <- depth * res$headline / mean(res$headline, na.rm = TRUE)
   
   res$footrope <- f$pressure[match(res$time, round(f$time * 60))]
   res$footrope <- depth * res$footrope / mean(res$footrope, na.rm = TRUE)

   res$tilt <- f$tilt.x[match(res$time, round(f$time * 60))]

   results$depth[i] <- depth
   results$speed.mean[i]  <- mean(res$speed, na.rm = TRUE)
   results$speed.sd[i]    <- sd(res$speed, na.rm = TRUE)
   results$speed.min[i]   <- min(res$speed, na.rm = TRUE)
   results$speed.max[i]   <- max(res$speed, na.rm = TRUE) 
   if (!all(is.na(res$headline)) & !all(as.numeric(res$headline) == 0)) results$headline.sd[i] <- summary(lm(res$headline ~ res$time))$sigma
   if (!all(is.na(res$footrope)) & !all(as.numeric(res$footrope) == 0)) results$footrope.sd[i] <- summary(lm(res$footrope ~ res$time))$sigma
   results$tilt.mean[i]   <- mean(res$tilt, na.rm = TRUE) 
   results$tilt.sd[i]     <- sd(res$tilt, na.rm = TRUE) 
   results$tilt.slope[i]  <- as.numeric(coef(lm(res$tilt ~ res$time))[2])
}

clg()
plot(res$time, res$speed, type = "l", ylim = c(-2.5, 2.5), lwd = 2, col = "red")
lines(res$time, res$tilt, col = "grey50", lwd = 1) 
#lines(res$time, res$headline, col = "blue", lwd = 2)
#lines(res$time, res$footrope, col = "darkgreen", lwd = 2)   
lines(res$time, res$speed, col = "red", lwd = 2)   
title(main = s$tow.id[i])

t <- e$time[index] * 60
y <- e$speed[index]
   
plot(t, y, type = "l")

# Frequency analysis:
loglike <- function(theta, t, y){
   mu <- theta[["mu"]] + theta[["amplitude"]] * sin(2*pi*(t-theta[["offset"]])/theta[["frequency"]])
   ll <- dnorm(y, mu, exp(theta[["log.sigma"]]), log = TRUE)
   return(-sum(ll))
}

theta <- c(mu = mean(res$speed), amplitude = sd(res$speed), offset = 0, frequency = 35, log.sigma = -1)    
loglike(theta, t, y)
theta <- optim(theta, loglike, t = t, y = y, control = list(trace = 3))$par
plot(res$time, res$speed, pch = 21, bg = "grey")
lines(res$time, theta[["mu"]] + theta[["amplitude"]] * sin(2*pi*(res$time-theta[["offset"]])/theta[["frequency"]]), lwd = 2, col = "red")


theta[["mu"]] <- mean(res$headline)    
theta[["amplitude"]] <- sd(res$headline) 
loglike(theta, res$time, res$headline)
theta <- optim(theta, loglike, t = res$time, y = res$headline, control = list(trace = 3))$par
plot(res$time, res$headline,  pch = 21, bg = "grey")
lines(res$time, theta[["mu"]] + theta[["amplitude"]] * sin(2*pi*(res$time-theta[["offset"]])/theta[["frequency"]]), lwd = 2, col = "red")

clg()
for (i in c(1, 348:352)){
   cat(paste0(i, ") ", s$tow.id[i], "\n"))
    
   e <- read.esonar(year = year, tow.id = s$tow.id[i])
   e <- e[!duplicated(time(e)), ]
   e$time <- time2min(time(e), start.time(s[i, ]))
   stop.time <- time2min(end.time(s[i, ]), start.time(s[i, ]))
   index <- (e$time >= 50/60) & (e$time <= stop.time)
   depth <- median(e$depth[index], na.rm = TRUE)
   if (is.na(depth)) depth <- mean(-depth(e$longitude[index], e$latitude[index]), na.rm = TRUE)
   
   tmp <- deg2km(e$longitude, e$latitude)
   e$xkm <- tmp$x
   e$ykm <- tmp$y
   v <- 1.94384 * 1000 * sqrt((e$xkm[3:nrow(e)] - e$xkm[1:(nrow(e)-2)] )^2 + (e$ykm[3:nrow(e)] - e$ykm[1:(nrow(e)-2)] )^2) / (60*(e$time[3:nrow(e)] - e$time[1:(nrow(e)-2)]))
   e$speed <- NA
   e$speed[2:(nrow(e)-1)] <- v  
   h <- read.star.oddi(year = year, tow.id = s$tow.id[i], type = "depth")
   h$time <- time2min(time(h), start.time(s[i, ]))

   f <- read.star.oddi(year = year, tow.id = s$tow.id[i], type = "tilt")
   f$time <- time2min(time(f), start.time(s[i, ]))
 
   res <- data.frame(time = round(e$time[index] * 60 ),
                     speed = e$speed[index])

   res$headline <- h$pressure[match(res$time, round(h$time * 60))]
   res$headline <- depth * res$headline / mean(res$headline, na.rm = TRUE)
   res$headline <- res$headline - predict(lm(res$headline ~ res$time))
   
   res$footrope <- f$pressure[match(res$time, round(f$time * 60))]
   res$footrope <- depth * res$footrope / mean(res$footrope, na.rm = TRUE)
   res$footrope <- res$footrope - predict(lm(res$footrope ~ res$time))
   res$tilt <- f$tilt.x[match(res$time, round(f$time * 60))]
   
   m <- rbind(0, cbind(0, kronecker(1:4, matrix(1, nrow = 4, ncol = 6)), 0), 0, 0)
   
   windows(width = 8.5, height = 11)
   layout(m)
   par(mar = c(0,0,0,0))
   plot(res$time, res$speed, type = "l", xaxt = "n", col = "grey40", lwd = 1, ylim = c(1, 3.5))
   mtext(paste0(i, ") ", s$tow.id[i], "\n"), 3, 0.5)
   mtext("Speed(knots)", 2, 2.5, cex = 1.0)
   hline(2, lwd = 1, col = "red", lty = "dashed")
   
   plot(res$time, res$headline, lwd = 1, type = "l", col = "grey40", xaxt = "n")
   mtext("Headline variation (m)", 2, 2.5, cex = 1.0)
   r <- par("usr")[3:4]
   hline(0, lwd = 1, col = "red", lty = "dashed")
   
   plot(res$time, res$footrope, ylim = r, type = "l", col = "grey40", xaxt = "n")
   mtext("Footrope variation (m)", 2, 2.5, cex = 1.0)
   hline(0, lwd = 1, col = "red", lty = "dashed")
   
   plot(res$time, res$tilt, type = "l", col = "grey40", xaxt = "n")
   mtext("Tilt angle (degrees)", 2, 2.5, cex = 1.0)
   mtext("Time (s)", 1, 2.5, cex = 1.25)
   hline(0, lwd = 1, col = "red", lty = "dashed")
}





   
   