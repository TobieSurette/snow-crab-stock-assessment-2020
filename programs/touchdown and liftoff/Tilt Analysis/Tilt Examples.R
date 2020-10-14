source("U:/Trawl Analyses/Tilt Analysis/Tilt Functions.R")

vline <- function(x, ...) for (i in 1:length(x)) lines(rep(x[i], 2), par("usr")[3:4], ...)
graphics.off()

# Illustration of tilt functions:
windows()
x <- seq(-1, 7, len = 1000)
xp <- c(0, 5)
slope = c(1, -3, 0, 3, 0)
w = c(0.5, 1)
sigma <- c(0.5, 0.1, 0.75)
mu <- mu.tilt(x, xp = xp, slope = slope, w = w)
sigma <- sigma.tilt(x, xp = xp, sigma = sigma, w = w)
plot(x, mu, type = "l", lwd = 2, col = "blue")
lines(x, mu + sigma, type = "l", lwd = 2, lty = "dashed", col = "red")
lines(x, mu - sigma, type = "l", lwd = 2, lty = "dashed", col = "red")


# Analyze tilt angle data for a single tow:
clg()
f <- read.star.oddi(year = 2018, tow.id = "GP071F", type = "tilt")
f$time <- time2min(time(f), touchdown(read.scset(year = unique(f$year), tow.id = unique(f$tow.id))))
theta <- init.tilt(f$time, f$tilt.x) 
   # Build parameter vector:
   v <- list(xp = as.numeric(theta[c("touchdown", "liftoff")]),
             log.sigma = as.numeric(theta[grep("log.sigma", names(theta))]),
             intercept = as.numeric(theta[c("descent.constant")]),
             log.w = c(log(0.2), log(0.4)),
             slope = c( as.numeric(theta["descent.linear"]),            # Descent rate.
                       -as.numeric(theta[c("descent.constant")]) / 0.2, # Descent to bottom transition rate.
                        0,                                              # Bottom rate.
                        as.numeric(theta[c("descent.constant")]) / 0.4, # Bottom to ascent transition rate.
                        as.numeric(theta["ascent.linear"])))            # Ascent rate.
theta <- unlist(v)
f <- f[which(f$time >= (theta["xp1"] - 2) & f$time <= (theta["xp2"] + 2)), ]   
plot.tilt(f$time, f$tilt.x, theta)
loglike.tilt(theta, f$time, f$tilt.x)
theta <- fit.tilt(f$time, f$tilt.x, theta = theta, reps = 10)
plot.tilt(f$time, f$tilt.x, theta)

# Prototypes for cod condition:
x <- seq(0, 365, len = 1000)
plot(x, transition(x, range = c(0, 365), s = 0.5, deg = 3), lwd = 2, type= "l", col = "blue")

slope <- c(-0.1, 0.2, -0.1)
y <- 1 + slope[1] * x + 
    (slope[2]-slope[1]) * plm(x, xp = 50, width  = 200, deg = 4) + 
    (slope[3]-slope[2]) * plm(x, xp = 220, width  = 200, deg = 4)
plot(x, y, lwd = 2, type= "l", col = "blue")

# Temperature versus depth model:
s <- read.rvset(year = 2000:2018, experiment = 1)
s <- s[which(!is.na(s$bottom.temperature) & s$bottom.temperature < 40), ]
mu <- function(x, theta){
   #c <- (x - theta["xp"])^3
   q <- (x - theta["xp"])^3
   q[x > theta["xp"]] <- 0
   #c[x > theta["xp"]] <- 0
   s <- sigmoid(x, range = c(theta[["xp"]], theta[["upper"]]), s = theta[["s"]], deg = 4)
   v <- theta[["intercept"]] + theta[["quadratic"]] * q + theta[["sigmoid"]] * s
   return(v)
}
sigma <- function(x, theta){
   q <- exp(theta[["log.sigma.quadratic"]]) * (x - theta[["xp"]])^2 
   q[x > theta[["xp"]]] <- 0
   v <- exp(theta[["log.sigma.intercept"]]) + q
   return(v)
}
loglike <- function(theta, x, y){
   v <- dnorm(y, mu(x, theta), sigma(x, theta), log = TRUE)
   return(-sum(v)) 
}
theta <- c(xp = 70, quadratic = -0.00011, sigmoid = 4.88, log.sigma.intercept = -0.11, log.sigma.quadratic = -6.75, intercept = 0.72, s = 2.04, upper = 277)
loglike(theta, x = depth(s), y = s$bottom.temperature)
res <- NULL
years <- sort(unique(s$year))
cols <- colorRampPalette(c("blue", "red"))(length(years))
# Model fit:
plot(depth(s), s$bottom.temperature, pch = 21, bg = "grey", xlab = "Depth(m)", ylab = "Temperature(C)")             
xx <- seq(10, 400, len = 1000)
for (i in 1:length(years)){
   index <- s$year == years[i]
   res <- rbind(res, optim(theta, loglike, x = depth(s)[index], y = s$bottom.temperature[index], control = list(trace = 3, maxit = 3000))$par)
   lines(xx, mu(xx, res[i, ]), lwd = 2, col = cols[i])
}
res <- as.data.frame(res)
dbarplot(res$sigmoid)
# Standardized residuals:
plot(log(depth(s)), (s$bottom.temperature - mu(depth(s), theta))/sigma(depth(s), theta), xlim = log(c(10, 200)), ylim = c(-4, 4))







