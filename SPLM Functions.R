# PLM transition kernel
# Defined over [0,1], value of zero x < 0 and constant slope x > 1
plm.kernel <- function(x, s = 0, p){
   if (missing(p)) p <- 1 / (1+exp(-s))
   v <- rep(NA, length(x))
   index <- x <= p
   v[index] <- -(0.2/p^3)*x[index]^5 + (0.5/p^2)*x[index]^4
   v[!index] <- (0.2/((1-p)^3))*(x[!index]-p)^5 - (0.5/((1-p)^2))*(x[!index]-p)^4 + (x[!index]-p)^2 + p*(x[!index]-p) + (3/10)*p^2
   v[x < 0] <- 0
   v[x >= 1] <- (x[x >= 1] - 1) - 2/5*p + 7/10
   return(v)
}

splm <- function(x, theta, xp, intercept, log.w = 0, slope, s = 0){
   # SPLM - Smooth piecewise linear model (spline version).
   
   k <- length(xp) # Model order.
   
   # Transform parameters:
   w <- exp(log.w)
   if (length(s) == 1) s <- rep(s, k)
   if (length(w) == 1) w <- rep(w, k) 
   if (length(slope) != (k+1)) stop("'slope' must contain ", k+1," elements.")
   p <- 1 / (1 + exp(-s))
     
   # Calculate values:
   v <- slope[1]*x + intercept
   for (i in 1:k) v <- v + diff(slope[i:(i+1)]) * w[i] * plm.kernel((x-xp[i]+ p[i]*w[i])/w[i], s = s[i]) 
        
   return(v)     
}

mu.depth <- function(x, theta){
   # Calculate component means and variance:
   mu <- splm(x, xp = theta[c("touchdown.time", "liftoff.time")], 
                 intercept = theta["intercept"],
                 slope = theta[c("linear.descent", "linear.bottom", "linear.ascent")], 
                 log.w = theta[grep("log.w", names(theta))])
  
   # Cubic and quartic terms:
   s <- 0
   if (length(grep("skewness", names(theta)) > 0))  s <- theta[grep("skewness", names(theta))]
   if (length(s) == 1) s <- rep(s, 2)
   p <- 1 / (1+exp(-s))  
   w <- 1
   if (length(grep("log.w", names(theta)) > 0)) w <- exp(theta[grep("log.w", names(theta))])
   if (length(w) == 1) w <- rep(w, 2)
   
   if ("cubic.descent" %in% names(theta)){
      lower <- theta["touchdown.time"] - p[1] * w[1]
      mu[x < lower] <- mu[x < lower] + theta["cubic.descent"] * (x[x < lower]-lower)^3
   }
   if ("cubic.ascent" %in% names(theta)){  
      upper <- theta["liftoff.time"] - (1-p[2]) * w[2]
      mu[x > upper] <- mu[x > upper] + theta["cubic.ascent"] * (x[x > upper]-upper)^3         
   }
   if ("quartic.descent" %in% names(theta)){
      lower <- theta["touchdown.time"] - p[1] * w[1]
      mu[x < lower] <- mu[x < lower] + theta["quartic.descent"] * (x[x < lower]-lower)^4
   }
   if ("quartic.ascent" %in% names(theta)){  
      upper <- theta["liftoff.time"] - (1-p[2]) * w[2]
      mu[x > upper] <- mu[x > upper] + theta["quartic.ascent"] * (x[x > upper]-upper)^3         
   }    
   
   return(mu)
}

loglike.depth <- function(theta, x, y, fixed, discrete = FALSE, collapse = TRUE){
   # Incorporate fixed parameters:
   if (!missing(fixed)) theta <- c(theta[setdiff(names(theta), names(fixed))], fixed)

   # Calculate mean and error:
   mu <- mu.depth(x, theta)
   sigma <- exp(theta["log.sigma"])

   # Log-likelihood values:
   if (!discrete) ll <- dnorm(y, mu, sigma, log = TRUE) else ll <- log(pnorm(y+1, mu, sigma)-pnorm(y-1, mu, sigma))

   # Sum over log-likelihoods:
   if (collapse) ll <- -sum(ll)

   return(ll)
}

plot.depth <- function(x, y, theta){
   plot(x, y, pch = 21, bg = "grey", cex = 0.8)
   mu <- mu.depth(x, theta)
   lines(x, mu, lwd = 2, col = "blue")
     
   s <- theta[grep("skewness", names(theta))]
   if (length(s) == 0) s <- 0
   if (length(s) == 1) s <- rep(s, 2)
   p <- 1 / (1+exp(-s))
   xlim <- c(theta["touchdown.time"] - p[1]*exp(theta["touchdown.log.w"]), theta["touchdown.time"] + (1-p[1])*exp(theta["touchdown.log.w"]))
   vline(xlim, lwd = 2, lty = "dashed", col = "red")
   vline(theta["touchdown.time"], lwd = 2, lty = "solid", col = "red")
   xlim <- c(theta["liftoff.time"] - p[2]*exp(theta["liftoff.log.w"]), theta["liftoff.time"] + (1-p[2])**exp(theta["liftoff.log.w"]))
   vline(xlim, lwd = 2, lty = "dashed", col = "red")
   vline(theta["liftoff.time"], lwd = 2, lty = "solid", col = "red")
}

      
clg(); plot(x, splm(x, c(0, 1), c(1.25, 0.75), slope = c(1, -1), log.w = log(0.2)), type= "l")



fl <- function(x) return(-2*x^3+3*x^2)
fr <- function(x) return(fl(1-x))

x <- seq(0, 1, len = 10000)

plot(c(0, max(x)), c(0, 1), type = "n")
lines(x, fl(x), col = "blue")
lines(x+1, fr(x), col = "green")

Fl <- function(x) return(x^3*(1-0.5*x))
Fr <- function(x) return(0.5*x^4-x^3+x+0.5)

x <- seq(0, 2, len = 10000)

plot(c(0, max(x)), c(0, 1), type = "n")
lines(x, Fl(x), col = "blue")
lines(x+1, Fr(x), col = "green")

phil <- function(x) return(-0.1*x^5 + 0.25*x^4)
phir <- function(x) return(0.1*x^5-0.25*x^4+0.5*x^2+0.5*x + 3/20)

x <- seq(0, 2, len = 10000)

plot(c(0, max(x)), c(0, 1), type = "n")
lines(x, phil(x), col = "blue")
lines(x+1, phir(x), col = "green")

vline(2)
hline(1)

#===============================================================================

fl <- function(x) return(-2*x^3+3*x^2)
fr <- function(x) return(fl(1-x))
fr <- function(x) return(+2*x^3-3*x^2+1)

p <- 0.16
x <- seq(0, 1, len = 10000)

plot(c(0, 1), c(0, 1), type = "n", xaxs = "i", yaxs = "i")
lines(x, fl(x/p), col = "blue")
lines(x+p, fr(x/(1-p)), col = "green")
vline(p, lwd = 2, lty = "dashed", col = "red")

Fl <- function(x, p = 1) return(-(1/(p^3))*x^4 + (2/p^2)*x^3)
Fr <- function(x) return((1/(1-p)^3)*(x^4)-(2/((1-p)^2))*(x)^3 + 2*x + p)

x <- seq(0, 1, len = 10000)
plot(c(0, 1), c(0, 1), type = "n", xaxs = "i", yaxs = "i")
lines(x, Fl(x, p = p), col = "blue")
lines(x+p, Fr(x), col = "green")
vline(p, lwd = 2, lty = "dashed", col = "red")
hline(p/2, lwd = 2, lty = "dashed", col = "red")

# Piecewise transition kernel:

   
phil <- function(x, p = 0.5) return(-(0.2/p^3)*x^5 + (0.5/p^2)*x^4)
phir <- function(x, p = 0.5) return((0.2/((1-p)^3))*x^5 - (0.5/((1-p)^2))*x^4 + x^2 + p*x + (3/10)*p^2) 



clg(); 
xp = c(-1, 2)
yp = c(1.25, 1.5)
plot(x, splm(x, xp = xp, yp = yp, slope = c(0.5, -1.5), log.w = log(0.5)), type= "l", lwd = 2, col = "blue")
vline(xp, lwd = 2, col = "red")
hline(yp, lwd = 2, col = "red")


plot(x, plm.kernel(x), type = "l", lwd = 2, col = "blue")


plot(x, x - w*plm.kernel(x+0.5, s = -1) - plm.kernel(x-1.5, s = 1))
 
w <- 0.5
s <- c(0, -1)
p <- 1 / (1 + exp(-s))
xp <- c(0, 1.5) # Inflection points.
slope <- c(0.5, 0, -1)
plot(x, slope[1]*x + 
        diff(slope[1:2]) * w * plm.kernel((x-xp[1]+ p[1]*w)/w, s = s[1]) + 
        diff(slope[2:3]) * w * plm.kernel((x-xp[2]+ p[2]*w)/w, s = s[2]))
vline(c(xp[1]-p[1]*w, xp[1]+w*(1-p[1])), lty = "dashed", lwd = 2, col = "red")
vline(xp[1], lty = "solid", lwd = 2, col = "red")

vline(c(xp[2]-p[2]*w, xp[2]+w*(1-p[2])), lty = "dashed", lwd = 2, col = "red")
vline(xp[2], lty = "solid", lwd = 2, col = "red")

 
x <- seq(-2, 3, len = 10000)

plot(c(0, max(x)), c(0, 1), type = "n")
lines(x, phil(x, p = p), col = "blue")
lines(x+p, phir(x, p = p), col = "green")

vline(2)
hline(1)