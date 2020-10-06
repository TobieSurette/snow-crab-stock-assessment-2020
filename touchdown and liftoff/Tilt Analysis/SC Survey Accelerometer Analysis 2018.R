
# Acceleration:
clg()
m <- kronecker(1:2, matrix(1, nrow = 4, ncol = 4))
m <- rbind(0, cbind(0, m, 0), 0, 0)
for (i in 120:140){
   tow.id <- s$tow.id[i]
   t <- read.star.oddi(year = 2018, tow.id = tow.id, type = "tilt")
   a <- read.star.oddi(year = 2018, tow.id = tow.id, type = "acceleration")

   end.time <- time2min(end.time(s[s$tow.id == tow.id, ]), start.time(s[s$tow.id == tow.id, ]))
   liftoff.time <- time2min(liftoff(s[s$tow.id == tow.id, ]), start.time(s[s$tow.id == tow.id, ]))

   t$time <- time2min(time(t), start.time(s[s$tow.id == tow.id, ]))
   a$time <- time2min(time(a), start.time(s[s$tow.id == tow.id, ]))

   vars <- c("G", "Xacc", "Yacc", "Zacc")
   index <- match(t$time, a$time)
   t[vars] <- a[index, vars]

   e <- read.esonar(year = 2018, tow.id = tow.id)
   e$time <- time2min(time(e), start.time(s[s$tow.id == tow.id, ]))
   
   windows()
   layout(m)
   par(mar = c(0,0,0,0))
   
   plot(e$time, e$speed, xlim = c(0, liftoff.time), xaxs = "i", xaxt = "n", ylim = c(0, 3))
   index <- e$time >= end.time & e$time <= liftoff.time
   points(e$time[index], e$speed[index], pch = 21, bg = "red") 
   
   index <- t$time >= 0 & t$time <= liftoff.time
   plot(t$time[index], t$Xacc[index] + sin(pi * t$tilt.x[index] / 180) * t$G[index], ylim = c(-0.02, 0.030), xlim = c(0, liftoff.time), xaxs = "i")
   index <- t$time >= end.time & t$time <= liftoff.time
   points(t$time[index], t$Xacc[index] + sin(pi * t$tilt.x[index] / 180) * t$G[index], pch = 21, bg = "red")
   lines(par("usr")[1:2], c(0, 0), lwd = 2, col = "red")
}

# Speed plot:
clg()
m <- kronecker(1:2, matrix(1, nrow = 4, ncol = 4))
m <- rbind(0, cbind(0, m, 0), 0, 0)
for (i in 161:180){
   tow.id <- s$tow.id[i]
   t <- read.star.oddi(year = 2018, tow.id = tow.id, type = "tilt")
   a <- read.star.oddi(year = 2018, tow.id = tow.id, type = "acceleration")

   end.time <- time2min(end.time(s[s$tow.id == tow.id, ]), start.time(s[s$tow.id == tow.id, ]))
   liftoff.time <- time2min(liftoff(s[s$tow.id == tow.id, ]), start.time(s[s$tow.id == tow.id, ]))

   t$time <- time2min(time(t), start.time(s[s$tow.id == tow.id, ]))
   a$time <- time2min(time(a), start.time(s[s$tow.id == tow.id, ]))

   vars <- c("G", "Xacc", "Yacc", "Zacc")
   index <- match(t$time, a$time)
   t[vars] <- a[index, vars]

   e <- read.esonar(year = 2018, tow.id = tow.id)
   e$time <- time2min(time(e), start.time(s[s$tow.id == tow.id, ]))
   
   windows()
   layout(m)
   par(mar = c(0,0,0,0))
   
   plot(e$time, e$speed, xlim = c(0, liftoff.time), xaxs = "i", xaxt = "n", ylim = c(0, 3))
   index <- e$time >= end.time & e$time <= liftoff.time
   points(e$time[index], e$speed[index], pch = 21, bg = "red") 
   
   index <- t$time >= 0 & t$time <= liftoff.time
   vv <- t$Xacc[index] + sin(pi * t$tilt.x[index] / 180) * t$G[index]
   tt <- t$time[index]
   plot(tt, cumsum(vv), xlim = c(0, liftoff.time), xaxs = "i")
   index <- tt >= end.time & tt <= liftoff.time
   points(tt[index], cumsum(vv)[index], pch = 21, bg = "red")
   lines(par("usr")[1:2], c(0, 0), lwd = 2, col = "red")
}

clg()
m <- kronecker(1:2, matrix(1, nrow = 4, ncol = 4))
m <- rbind(0, cbind(0, m, 0), 0, 0)
windows()
plot(c(0, 10), c(-5, 5), xaxs = "i") 
for (i in 1:nrow(s)){
   tow.id <- s$tow.id[i]
   t <- read.star.oddi(year = 2018, tow.id = tow.id, type = "tilt")
   a <- read.star.oddi(year = 2018, tow.id = tow.id, type = "acceleration")

   end.time <- time2min(end.time(s[s$tow.id == tow.id, ]), start.time(s[s$tow.id == tow.id, ]))
   liftoff.time <- time2min(liftoff(s[s$tow.id == tow.id, ]), start.time(s[s$tow.id == tow.id, ]))

   t$time <- time2min(time(t), start.time(s[s$tow.id == tow.id, ]))
   a$time <- time2min(time(a), start.time(s[s$tow.id == tow.id, ]))

   vars <- c("G", "Xacc", "Yacc", "Zacc")
   index <- match(t$time, a$time)
   t[vars] <- a[index, vars]

   index <- t$time >= 0 & t$time <= liftoff.time
   vv <- cumsum(t$Xacc[index] + sin(pi * t$tilt.x[index] / 180) * t$G[index])
   vv <- vv - vv[1]
   tt <- t$time[index]
   lines(tt, vv, col = "blue")
   index <- tt >= end.time & tt <= liftoff.time
   lines(tt[index], vv[index], col = "red")
   #lines(par("usr")[1:2], c(0, 0), lwd = 2, col = "red")
}

# Drift-adjusted integrated speed differential:
clg()
m <- kronecker(1:2, matrix(1, nrow = 4, ncol = 4))
m <- rbind(0, cbind(0, m, 0), 0, 0)
windows()
plot(c(-5, 5), c(-3, 3), xaxs = "i", ylab = "Delta X speed (m/s)", xlab = "Time from tow end (min)") 
for (i in 1:nrow(s)){
   tow.id <- s$tow.id[i]
   t <- read.star.oddi(year = 2018, tow.id = tow.id, type = "tilt")
   a <- read.star.oddi(year = 2018, tow.id = tow.id, type = "acceleration")

   start.time <- time2min(start.time(s[s$tow.id == tow.id, ]), end.time(s[s$tow.id == tow.id, ]))
   end.time <- time2min(end.time(s[s$tow.id == tow.id, ]), end.time(s[s$tow.id == tow.id, ]))
   liftoff.time <- time2min(liftoff(s[s$tow.id == tow.id, ]), end.time(s[s$tow.id == tow.id, ]))

   t$time <- time2min(time(t), end.time(s[s$tow.id == tow.id, ]))
   a$time <- time2min(time(a), end.time(s[s$tow.id == tow.id, ]))

   vars <- c("G", "Xacc", "Yacc", "Zacc")
   index <- match(t$time, a$time)
   t[vars] <- a[index, vars]

   index <- t$time >= start.time & t$time <= liftoff.time
   vv <- cumsum(t$Xacc[index] + sin(pi * t$tilt.x[index] / 180) * t$G[index])
   vv <- vv - vv[1]
   tt <- t$time[index]

   vvv <- vv[tt <= 0]
   ttt <- tt[tt <= 0]
   model <- lm(vvv ~ ttt)
   vv <- vv - predict(model, newdata = list(ttt = tt))
   
   lines(tt, vv, col = "blue")
   index <- tt >= end.time & tt <= liftoff.time
   lines(tt[index], vv[index], col = "red")   
}

# Drift-adjusted integrated speed differential:
clg()
m <- kronecker(1:2, matrix(1, nrow = 4, ncol = 4))
m <- rbind(0, cbind(0, m, 0), 0, 0)
windows()
plot(c(-4, 2), c(0, 3), xaxs = "i", xlab = "Delta Speed X (m/s)", ylab = "Speed(knots)") 
for (i in 1:nrow(s)){
   tow.id <- s$tow.id[i]
   t <- read.star.oddi(year = 2018, tow.id = tow.id, type = "tilt")
   a <- read.star.oddi(year = 2018, tow.id = tow.id, type = "acceleration")
   e <- read.esonar(year = 2018, tow.id = tow.id)
   
   
   start.time <- time2min(start.time(s[s$tow.id == tow.id, ]), end.time(s[s$tow.id == tow.id, ]))
   end.time <- time2min(end.time(s[s$tow.id == tow.id, ]), end.time(s[s$tow.id == tow.id, ]))
   liftoff.time <- time2min(liftoff(s[s$tow.id == tow.id, ]), end.time(s[s$tow.id == tow.id, ]))

   t$time <- time2min(time(t), end.time(s[s$tow.id == tow.id, ]))
   a$time <- time2min(time(a), end.time(s[s$tow.id == tow.id, ]))
   e$time <- time2min(time(e), end.time(s[s$tow.id == tow.id, ]))
   
   t$speed <- approx(e$time, e$speed, t$time)$y
   
   vars <- c("G", "Xacc", "Yacc", "Zacc")
   index <- match(t$time, a$time)
   t[vars] <- a[index, vars]

   index <- t$time >= start.time & t$time <= liftoff.time
   vv <- cumsum(t$Xacc[index] + sin(pi * t$tilt.x[index] / 180) * t$G[index])
   vv <- vv - vv[1]
   tt <- t$time[index]
   ss <- t$speed[index]
   
   vvv <- vv[tt <= 0]
   ttt <- tt[tt <= 0]
   model <- lm(vvv ~ ttt)
   vv <- vv - predict(model, newdata = list(ttt = tt))
   
   points(vv[tt < 0], jitter(ss[tt < 0], amount = 0.05), col = "blue", pch = 21, bg = "blue", cex = 0.3)
   points(vv[tt >= 0], jitter(ss[tt >= 0], amount = 0.05), pch = 21, bg = "red", cex = 0.3)   
}


plot(t$Xacc[index], t$tilt.x[index])

plot(sin(pi * t$tilt.x[index] / 180), ylim = c(-0.1, 0.05))
plot(t$tilt.x[index], ylim = c(-4, 1))
plot(t$Xacc[index], ylim = c(-1, 1))

plot(t$Xacc[index], sin(pi * t$tilt.x[index] / 180) * t$G[index])
plot(t$Xacc[index] - sin(pi * t$tilt.x[index] / 180) * t$G[index])

plot(t$Xacc[index] + sin(pi * t$tilt.x[index] / 180) * t$G[index], ylim = c(-0.025, 0.025))


plot(t$Xacc[index], t$G[index])

plot(t$Xacc[index], t$Yacc[index])
plot(t$Xacc[index], t$Zacc[index])
plot(t$Yacc[index], t$Zacc[index])

plot(t$Yacc[index], t$tilt.y[index])
plot(t$Zacc[index], t$tilt.z[index])
 