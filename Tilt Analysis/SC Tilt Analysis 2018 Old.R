library(gulf)
library(mgcv)

spline <- function(x, theta){
   # 'xp' - change point on the x axis.
   # 'yp' - change point on the y axis.
   # 'slope' - Slope of the linear part of the change-point model.
   # 'quadratic' - Quadratic coefficient of the polynomial component.
   
   f <- rep(NA, length(x))
   index <- x < theta["xp"]
   dx <- (x[index] - theta["xp"])
   f[index] <- theta["slope"]*dx + theta["yp"]
   if ("quadratic" %in% names(theta)) f[index] <- f[index] + theta["quadratic"]*dx^2
   if ("cubic" %in% names(theta))     f[index] <- f[index] + theta["cubic"]*dx^3
   if ("quartic" %in% names(theta))   f[index] <- f[index] + theta["quartic"]*dx^4
   if ("quintic" %in% names(theta))   f[index] <- f[index] + theta["quintic"]*dx^5
   f[!index] <- theta["slope"]*(x[!index] - theta["xp"]) + theta["yp"]
   
   return(f)
}

init.tilt <- function(t, y, buffer = 1, plot = FALSE){
   # Values to search over:
   tt <- seq(max(-4, min(t)+buffer), min(15, max(t)-buffer), by = 0.05)
   delta <- NA * tt
      
   # Do sweeps:
   for (i in 1:length(tt)){
      delta[i] <- mean(y[(t <= tt[i] + buffer) & (t >= tt[i])]) - 
                  mean(y[(t >= tt[i] - buffer) & (t <= tt[i])])
   }

   # Build parameter vector:
   theta <- c(touchdown = tt[tt < 3][which.min(delta[tt < 3])], 
              liftoff   = tt[tt > 3][which.max(delta[tt > 3])])
  
   if (plot){
      windows()
      plot(tt, delta)
      lines(rep(theta["touchdown"], 2), par("usr")[3:4], lty = "dashed", col = "red", lwd = 2)
      lines(rep(theta["liftoff"], 2), par("usr")[3:4], lty = "dashed", col = "red", lwd = 2)
            
      windows()
      plot(t, y)
      lines(rep(theta["touchdown"], 2), par("usr")[3:4], lty = "dashed", col = "red", lwd = 2)
      lines(rep(theta["liftoff"], 2), par("usr")[3:4], lty = "dashed", col = "red", lwd = 2)
   }
   
   # Pre-trawling period:
   index <- (t >= (theta["touchdown"]-2)) & (t <= theta["touchdown"])
   model <- lm(y[index] ~ t[index])
   beta <- as.numeric(coef(model))   
   sigma.descent <- sqrt(mean(residuals(model)^2))
   theta <- c(theta, descent.constant = beta[1], descent.linear = beta[2])
   
   # Trawling period:
   theta <- c(theta, bottom.constant = mean(y[(t <= (theta["liftoff"])) & (t >= theta["touchdown"])]))
   
   # Post-trawling period:
   index <- (t <= (theta["liftoff"]+2)) & (t >= theta["liftoff"])
   model <- lm(y[index] ~ t[index])
   beta <- as.numeric(coef(model))   
   sigma.ascent <- sqrt(mean(residuals(model)^2))
   theta <- c(theta, ascent.constant = beta[1], ascent.linear = beta[2])
    
   # Logistic slope parameters
   theta <- c(theta, touchdown.log.delta = log(0.2), liftoff.log.delta = log(0.2))
   
   # Error parameters:
   theta <- c(theta, 
              descent.log.sigma = log(sigma.descent), 
              bottom.log.sigma  = log(sd(y[(t <= theta["liftoff"]) & (t >= theta["touchdown"])])),
              ascent.log.sigma = log(sigma.ascent))
       
   return(theta)
}

tilt.mu <- function(t, theta){
   # Evaluate model components:
   y.descent <- theta["descent.linear"] * t + theta["descent.constant"]
   y.bottom <- theta["bottom.constant"]
   y.ascent <- theta["ascent.linear"] * t + theta["ascent.constant"]

   # Logistic transforms:
   beta.touchdown <- 2 * log(3) / exp(theta["touchdown.log.delta"])
   beta.liftoff <- 2 * log(3) / exp(theta["liftoff.log.delta"]) 
   p.touchdown <- 1 / (1 + exp(-beta.touchdown * (t-theta["touchdown"])))
   p.liftoff <- 1 / (1 + exp(-beta.liftoff * (t-theta["liftoff"])))
   
   # Weighted mean:
   mu <- y.descent + 
         p.touchdown * (y.bottom-y.descent) + 
         p.liftoff * (y.ascent-y.bottom)
   
   return(mu)
}


tilt.sigma <- function(t, theta){
   # Logistic transforms:
   beta.touchdown <- 2 * log(3) / exp(theta["touchdown.log.delta"])
   beta.liftoff <- 2 * log(3) / exp(theta["liftoff.log.delta"]) 
   p.touchdown <- 1 / (1 + exp(-beta.touchdown * (t-theta["touchdown"])))
   p.liftoff <- 1 / (1 + exp(-beta.liftoff * (t-theta["liftoff"])))

   # Weighted average of variance values:
   sigma <- exp(theta["descent.log.sigma"]) + 
            p.touchdown * (exp(theta["bottom.log.sigma"]) - exp(theta["descent.log.sigma"])) + 
            p.liftoff * (exp(theta["ascent.log.sigma"]) - exp(theta["bottom.log.sigma"]))
   
   return(sigma)
}

loglike.tilt <- function(theta, t, y){
   return(-sum(dnorm(y, tilt.mu(t, theta),  tilt.sigma(t, theta), log = TRUE)))
}

t <- seq(-2, 2, len = 1000)
clg()
theta <- c(xp = 0, yp = 1, slope = 0, quadratic = 0.1, cubic = -0., quartic = -0.1)
plot(t, spline(t, theta))

year <- 2018
tow.id <- "GP106F"
m <- read.minilog(year = 2018, tow.id = tow.id)
e <- read.esonar(year = 2018, tow.id = tow.id)
h <- read.star.oddi(year = 2018, tow.id = tow.id, location = "headline")
f <- read.star.oddi(year = 2018, tow.id = tow.id, type = "tilt")

s <- read.scset(year = 2018, valid = 1, tow.id = tow.id)

m$time <- time2min(time(m), touchdown(s))
e$time <- time2min(time(e), touchdown(s))
h$time <- time2min(time(h), touchdown(s))
f$time <- time2min(time(f), touchdown(s))

xlim <- c(-1.5, 8)
index <- m$time >= xlim[1] & m$time <= xlim[2]
plot(m$time[index], -m$depth[index], xlab = "Time(min)", ylab = "Depth(m)", cex.lab = 1.5)

# Headline Star-Oddi:
index <- h$time >= xlim[1] & h$time <= xlim[2]
h$minilog.depth <- approx(m$time, m$depth, h$time)$y
beta <- coef(lm(minilog.depth ~ pressure, data = h[index, ]))
h$depth <- beta[2] * h$pressure + beta[1] 
lines(h$time, -h$depth, lwd = 2, col = "blue")
time <- h$time[index]
#model1 <- gam(h$depth[index] ~ s(time))
#lines(h$time[index], -predict(model1), lwd = 2, lty = "dashed", col = "blue")

# Footrope Star-Oddi:
index <- f$time >= xlim[1] & f$time <= xlim[2]
f$minilog.depth <- approx(m$time, m$depth, f$time)$y
beta <- coef(lm(minilog.depth ~ pressure, data = f[index, ]))
f$depth <- beta[2] * f$pressure + beta[1] 
lines(f$time, -f$depth, lwd = 2, col = "red")
time <- f$time[index]
model2 <- gam(f$depth[index] ~ s(time))

f$tilt.x <- (f$tilt.x - mean(f$tilt.x[index])) / sd(f$tilt.x[index])
f$tilt.y <- (f$tilt.y - mean(f$tilt.y[index])) / sd(f$tilt.y[index])
f$tilt.z <- (f$tilt.z - mean(f$tilt.z[index])) / sd(f$tilt.z[index])
                               
points(f$time, par("usr")[3] + 0.5*diff(par("usr")[3:4]) + 5*f$tilt.x, pch = 21, bg = "red")
points(f$time, par("usr")[3] + 0.5*diff(par("usr")[3:4]) + 5*f$tilt.y, pch = 21, bg = "green")
points(f$time, par("usr")[3] + 0.5*diff(par("usr")[3:4]) + 5*f$tilt.z, pch = 21, bg = "blue")
lines(c(0, 0), par("usr")[3:4], lty = "dashed", lwd = 2, col = "red")

windows()
buffer <- c(1:5)
tt <- seq(-2, 14, by = 0.01)
delta <- NA * tt
delta.sd <- NA * tt
limits <- NULL
for (j in 1:length(buffer)){
   for (i in 1:length(tt)){
      index <- f$time >= tt[i] - buffer[j] & f$time <= tt[i]
      delta[i] <- mean(f$tilt.x[f$time <= tt[i] + buffer[j] & f$time >= tt[i]]) - 
                  mean(f$tilt.x[f$time >= tt[i] - buffer[j] & f$time <= tt[i]])
      delta.sd[i] <- sd(f$tilt.x[f$time <= tt[i] + buffer[j] & f$time >= tt[i]]) - 
                     sd(f$tilt.x[f$time >= tt[i] - buffer[j] & f$time <= tt[i]])   
   }
   if (j == 1) plot(tt, delta) else lines(tt, delta, col = rainbow(length(buffer))[j], lwd = 2)
   limits <- c(tt[tt < 3][which.min(delta[tt < 3])], tt[tt > 3][which.max(delta[tt > 3])])
   lines(rep(limits[1], 2), par("usr")[3:4], lty = "dashed", col = "red", lwd = 2)
   lines(rep(limits[2], 2), par("usr")[3:4], lty = "dashed", col = "red", lwd = 2)
}

problems <- c("316F", "269F", "282F", "350F", "352F", "174A1", "149F", "088F", "084A1", "092A1", "057F", "055F", "132F", "091F", 
              "258F", "182F", "178F", "245F", "284F", "277F", "266F", "270F", "264F", "241F", "246F", "202A1", "206A2")
problems <- paste0("GP", problems)

# Removed tows that were fixed:
problems <- setdiff(problems, c("GP316F", "GP269F", "GP282F", "GP350F", "GP352F", "GP084A1", "GP055F", "GP091F", "GP258F", "GP178F", "GP284F", "GP277F", "GP266F", "GP246F"))

clg()
year <- 2018
s <- read.scset(year = year, valid = 1)
s <- s[!(s$tow.id %in% problems), ]
plot = TRUE
res <- NULL
for (i in 301:nrow(s)){
   print(i)
   f <- read.star.oddi(year = 2018, tow.id = s$tow.id[i], type = "tilt")
   f$time <- time2min(time(f), touchdown(s[i,]))

   #index <- which(f$time >= -3 & f$time <= 7)
   #theta <- init.tilt(f$time[index], f$tilt.x[index], plot = TRUE) 
   
   theta <- init.tilt(f$time, f$tilt.x) 
   index <- which(f$time >= (theta["touchdown"] - 2) & f$time <= (theta["liftoff"] + 2))
   
   loglike.tilt(theta, f$time[index], f$tilt.x[index])

   for (j in 1:10) theta <- optim(theta, loglike.tilt, t = f$time[index], y = f$tilt.x[index], control = list(maxit = 300, trace = 0))$par
   theta <- optim(theta, loglike.tilt, t = f$time[index], y = f$tilt.x[index], control = list(maxit = 5000, trace = 0))$par
   
   # Load Minilog data:
   minilog <- read.minilog(year = year, tow.id = s$tow.id[i])
   minilog$time <- time2min(time(minilog), touchdown(s[i,])) 
   
   # Scale Star-Oddi footrope to Minilog depth:
   tmp <- approx(minilog$time, minilog$depth, f$time[(f$time >= theta["touchdown"]-2) & (f$time <= theta["liftoff"] + 2)])
   tmp$z <- f$pressure[(f$time >= theta["touchdown"]-2) & (f$time <= theta["liftoff"] + 2)]
   beta <- coef(lm(y ~ z, data  = tmp))
   f$depth <- beta[1] + beta[2] * f$pressure
   
   # Load E-Sonar data:
   esonar <- read.esonar(year = year, tow.id = s$tow.id[i])
   esonar$time <- time2min(time(esonar), touchdown(s[i,])) 
   esonar <- esonar[!is.na(esonar$depth), ]
   
   # Scale Star-Oddi footrope to Minilog depth:
   tmp <- approx(minilog$time, minilog$depth, esonar$time[(esonar$time >= theta["touchdown"]-2) & (esonar$time <= theta["liftoff"] + 2)])
   tmp$z <- esonar$depth[(esonar$time >= theta["touchdown"]-2) & (esonar$time <= theta["liftoff"] + 2)]
   if (length(tmp$y) > 2){
      beta <- coef(lm(y ~ z, data  = tmp))
      esonar$depth <- beta[1] + beta[2] * esonar$depth   
   }
   
   clg()
   
   if (plot){
      windows();
      m <- rbind(0, cbind(0, kronecker(1:2, matrix(1, nrow = 4, ncol = 5)) , 0), 0)
      layout(m)
      par(mar = c(0, 0, 0, 0))
      xlim = c(theta["touchdown"]-2, theta["liftoff"] + 2)
      ylim <- range(f$depth[f$time > xlim[1] & f$time < xlim[2]])
      ylim[2] <- ylim[2] + 0.1 * diff(ylim)
      plot(f$time, f$depth, xlim = xlim, ylim = ylim, xlab = "", ylab = "", xaxt = "n", yaxt = "n")
      grid()
      mtext("Depth(meters)", 2, 2.5, cex = 1.5)
      mtext(s$tow.id[i], 3, 2.5, cex = 1.5)
      lines(rep(theta["touchdown"], 2), par("usr")[3:4], lwd = 2, col = "red", lty = "dashed")
      lines(rep(theta["liftoff"], 2), par("usr")[3:4], lwd = 2, col = "red", lty = "dashed")
      axis(2)
      lines(minilog$time, minilog$depth, lwd = 2, col = "green")
      points(esonar$time, esonar$depth, pch = 21, bg = "red", cex = 1.25)
      
      plot(f$time, f$tilt.x, xlim = xlim, ylim = c(-30, 90), pch = 21, bg = "grey", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
      grid()
      lines(f$time, tilt.mu(f$time, theta), lwd = 2, col = "blue")
      lines(f$time, tilt.mu(f$time, theta) + tilt.sigma(f$time, theta), lwd = 2, col = "blue", lty = "dashed")
      lines(f$time, tilt.mu(f$time, theta) - tilt.sigma(f$time, theta), lwd = 2, col = "blue", lty = "dashed")
      lines(rep(theta["touchdown"], 2), par("usr")[3:4], lwd = 2, col = "red", lty = "dashed")
      lines(rep(theta["liftoff"], 2), par("usr")[3:4], lwd = 2, col = "red", lty = "dashed")
      axis(1); axis(2);
      mtext("Time(min)", 1, 3.0, cex = 1.5)
      mtext("Angle (degrees)", 2, 2.5, cex = 1.5)
   }
   
   res <- rbind(res, cbind(year = year, tow.id = s$tow.id[i], as.data.frame(t(theta))))
}

excel(res)
index <- match(res$tow.id, s$tow.id)

plot(depth(longitude(s), latitude(s)), res$touchdown)

plot(res$touchdown * 60)
dbarplot(table(round(res$touchdown * 60)))

grid()

plot(res$liftoff - res$touchdown)

plot(res$bottom.constant)

plot(depth(longitude(s), latitude(s)), exp(res$bottom.log.sigma))

gulf.map(sea = TRUE)
points(longitude(s), latitude(s), pch = 21, cex = 0.25 * exp(res$bottom.log.sigma), bg = "red")

gulf.map(sea = TRUE)
points(longitude(s), latitude(s), pch = 21, cex = 0.25 * (res$liftoff - res$touchdown), bg = "red")

gulf.map(sea = TRUE)
points(longitude(s), latitude(s), pch = 21, cex = 0.5 * sqrt(abs(res$touchdown*60)), bg = "red")

plot(exp(res$touchdown.log.delta)*60, ylim = exp(c(-5, -1))*60)
dbarplot(table(round(exp(res$touchdown.log.delta)*60)))

plot(exp(res$liftoff.log.delta)*60, ylim = c(0, 40))
dbarplot(table(round(exp(res$liftoff.log.delta)*60)), xlim = c(0, 60))
             
grid()

plot(res$bottom.constant)

t <- seq(-1.5, 2, len = 1000)
p1 <- predict(model1, newdata = list(time = t))
p2 <- predict(model2, newdata = list(time = t))

n = length(t)
plot(diff(diff(p1)), ylim = c(-0.0008, 0), xlim = c(300, 500), col = "blue")
lines(diff(diff(p2)), lwd = 2, col = "red") 
       
