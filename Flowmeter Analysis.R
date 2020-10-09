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

tows <- x$tow.id[substr(x$tow.id,3,5) %in% substr(x$tow.id[substr(x$tow.id,2,2) == "C"], 3, 5)]
res <- NULL
for (i in 1:length(tows)){
   f <- read.flowmeter(year = 2019, tow.id = tows[i])
   if (!is.null(f)){
      e <- read.esonar(year = year, tow.id = tows[i])
      e$time <- time2min(time(e), start.time(x[x$tow.id == tows[i], ]))
      
      stop.time <- time2min(end.time(x[x$tow.id == tows[i], ]), start.time(x[x$tow.id == tows[i], ]))
      liftoff.time <- time2min(liftoff(x[x$tow.id == tows[i], ]), start.time(x[x$tow.id == tows[i], ]))

      f$time <- time2min(time(f), start.time(x[x$tow.id == tows[i], ]))
      
      f <- f[f$time >= 0 & f$time <= liftoff.time, ]
      f$time <- f$time - stop.time
      e$time <- e$time - stop.time
      
      # Import vessel speed:
      f$speed <- e$speed[match(round(f$time * 60), round(e$time * 60))]
      
      tmp <- f[c("time", "Speed.(m/s)", "tow.id", "speed")]
      names(tmp) <- c("time", "flow", "tow.id", "speed")
      res <- rbind(res, tmp)
   }
}

res$depth <- -round(depth(x$longitude, x$latitude)[match(res$tow.id, x$tow.id)], 1)
res$warp <- round(1.8288 * x$warp[match(res$tow.id, x$tow.id)], 1)
res$time <- res$time * 60

res <- res[!is.na(res$speed), ]

res$speed <- 0.51444 * res$speed

cvars <- paste0("current.", unique(res$tow.id))
svars <- paste0("scale.", unique(res$tow.id))

theta <- c(speed.winch = 0.1, log_sigma = -1)
theta[cvars] <- 0.2
theta[svars] <- 1

x <- res

loglike <- function(theta, x){
   # Function defintions:
   step <- function(x) return(as.numeric(x >= 0)) # Step function.
   w <- function(t, w, d) return(sqrt((w - theta[["speed.winch"]]*t)^2 - d^2))  # Trawl distance w/r to vessel.
   dw <- function(t, w, d) return(-(theta[["speed.winch"]]*(w-theta[["speed.winch"]]*t)) / w(t,w,d)) # Trawl speed w/r to vessel.
   
   # Flow current prediction: 
   current <- theta[paste0("current.", x$tow.id)] # Water current estimates.
   scale <- theta[paste0("scale.", x$tow.id)]     # Scaling factor between flowmeter and trawl speed.
   mu <- scale * (x$speed - step(x$time) * dw(x$time, x$warp, x$depth) * x$time + current)
   
   # Log-likelihood values:
   ll <- dnorm(x$flow, mu, exp(theta[["log_sigma"]]), log = TRUE)
   
   return(-sum(ll))
}

loglike(theta, res)



theta <- optim(theta, loglike, x = res, control = list(maxit = 2000, trace = 3))$par


# Function defintions:
step <- function(x) return(as.numeric(x >= 0)) # Step function.
w <- function(t, w, d) return(sqrt((w - theta[["speed.winch"]]*t)^2 - d^2))  # Trawl distance w/r to vessel.
dw <- function(t, w, d) return(-(theta[["speed.winch"]]*(w-theta[["speed.winch"]]*t)) / w(t,w,d)) # Trawl speed w/r to vessel.
   

tow.id <- unique(res$tow.id)[8]
x <- res[res$tow.id == tow.id, ]

# Flow current prediction: 
current <- theta[paste0("current.", x$tow.id)] # Water current estimates.
scale <- theta[paste0("scale.", x$tow.id)]     # Scaling factor between flowmeter and trawl speed.
mu <- scale * (x$speed - step(x$time) * dw(x$time, x$warp, x$depth) * x$time + current)
    
clg()
plot(x$time, x$flow)
lines(x$time, mu, col = "red", lwd = 2)


x <- res[res$tow.id == tow.id, ]

cvars <- paste0("current.", unique(x$tow.id))
svars <- paste0("scale.", unique(x$tow.id))

theta <- c(speed.winch = 0.1, log_sigma = -1)
theta[cvars] <- 0.2
theta[svars] <- 0.65

loglike(theta, x)
theta <- optim(theta, loglike, x = x, control = list(maxit = 2000, trace = 3))$par

