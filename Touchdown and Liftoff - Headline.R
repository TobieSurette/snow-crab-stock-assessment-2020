library(gulf)

source("C:/gulf package/gulf/R/liftoff.scset.R")

year <- 2019 
                                          
# Read revised start-end times:
inits <- read.table("U:/Snow Crab/Stock Assessment 2019/Touchdown and Swept Area Results/Touchdown and Liftoff - Tilt 2019.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
s <- read.scset(year = year, valid = 1)
s$touchdown.init <- inits$touchdown[match(inits$tow.id, s$tow.id)]
s$liftoff.init <- inits$liftoff[match(inits$tow.id, s$tow.id)]

# Smooth piecewise-linear model log-likelihood:
mu <- function(x, theta){
   # Parse parameter vector:
   xp    <- theta[["xp"]]
   yp    <- theta[["yp"]]
   
   # Calculate 'beta' parameters:
   beta <- theta[["slope0"]]
   beta[2] <- theta[["slope1"]] - theta[["slope0"]]

   # Rescale 'q' parameter:
   q <- 0.01 / (2 * log(3))
   
   # Add 'yp' smoothing correction:
   yp <- yp - beta[2] * q * log(2)
         
   # Calculate piecewise linear model prediction:
   mu <- yp + beta[1] * (x - xp) + beta[2] * q * log(1 + exp((x-xp)/q))
   index <- !is.finite(mu) & (x > xp)
   mu[index] <- yp + (beta[1] + beta[2]) * (x[index] - xp)
   index <- !is.finite(mu) & (x < xp)
   mu[index] <- yp + beta[1] * (x[index] - xp)
   
   return(mu)
}

loglike <- function(theta, x, y) return(-sum(dnorm(y, mu(x, theta), exp(theta[["log_sigma"]]), log = TRUE)))

s$touchdown.headline <- "        "
s$liftoff.headline <- "        "
for (i in 360:nrow(s)){
   print(i)
   # Load star oddi headline data:
   t <- read.star.oddi(year = year, tow.id = s$tow.id[i], type = "depth")
   t$time <- time2sec(time(t), start.time(s[s$tow.id == s$tow.id[i], ]))

   # Initial value for touchdown:
   touchdown = time2sec(time.default(paste0(as.character(date(s[i, ])), " ", s$touchdown.init[i], " AST")), start.time(s[s$tow.id == s$tow.id[i], ]))
   liftoff   = time2sec(time.default(paste0(as.character(date(s[i, ])), " ", s$liftoff.init[i], " AST")), start.time(s[s$tow.id == s$tow.id[i], ]))
   
   # Touchdown data: 
   x <- t$time[(t$time >= (touchdown - 120)) & (t$time <= (touchdown + 120))]
   y <- t$pressure[(t$time >= (touchdown - 120)) & (t$time <= (touchdown + 120))]
   
    if (length(x) > 9){
      # Initialize touchdown model parameters: 
      slope0 <- coef(lm(y[x < touchdown] ~ x[x < touchdown]))[[2]]
      slope1 <- coef(lm(y[x > touchdown] ~ x[x > touchdown]))[[2]]
      yp <- mean(y[(x >= (touchdown - 10)) & (x <= (touchdown + 10))])
      sigma <- sd(c(residuals(lm(y[x < touchdown] ~ x[x < touchdown])), residuals(lm(y[x > touchdown] ~ x[x > touchdown]))))

      # Fit touchdown model parameters: 
      theta <- c(slope0 = slope0, slope1 = slope1, xp = touchdown, yp = yp, log_sigma = log(sigma))
      for (j in 1:15) theta <- optim(theta, loglike, x = x, y = y, control = list(trace = 0, maxit = 2000))$par
      s$touchdown.headline[i] <- strsplit(as.character(start.time(s[i, ]) + round(theta[["xp"]])), " ")[[1]][2]
   }
   
   if (length(x) > 9){
      # Liftoff data: 
      x <- t$time[(t$time >= (liftoff - 120)) & (t$time <= (liftoff + 120))]
      y <- t$pressure[(t$time >= (liftoff - 120)) & (t$time <= (liftoff + 120))]
      
      # Initialize liftoff model parameters: 
      slope0 <- coef(lm(y[x < liftoff] ~ x[x < liftoff]))[[2]]
      slope1 <- coef(lm(y[x > liftoff] ~ x[x > liftoff]))[[2]]
      yp <- mean(y[(x >= (liftoff - 10)) & (x <= (liftoff + 10))])
      sigma <- sd(c(residuals(lm(y[x < liftoff] ~ x[x < liftoff])), residuals(lm(y[x > liftoff] ~ x[x > liftoff]))))

      # Fit touchdown model parameters: 
      theta <- c(slope0 = slope0, slope1 = slope1, xp = liftoff, yp = yp, log_sigma = log(sigma))
      for (j in 1:15) theta <- optim(theta, loglike, x = x, y = y, control = list(trace = 0, maxit = 2000))$par
      s$liftoff.headline[i] <- strsplit(as.character(start.time(s[i, ]) + round(theta[["xp"]])), " ")[[1]][2]
   }
}

# Check differences:
index <- s$touchdown.headline != "        "
headline <- time.default(paste0(as.character(date(s)[index]), " ", s$touchdown.headline[index], " AST"))
tilt <- time.default(paste0(as.character(date(s)[index]), " ", s$touchdown.init[index], " AST"))
tilt-headline

index <- s$liftoff.headline != "        "
headline <- time.default(paste0(as.character(date(s)[index]), " ", s$liftoff.headline[index], " AST"))
tilt <- time.default(paste0(as.character(date(s)[index]), " ", s$liftoff.init[index], " AST"))
tilt-headline

# Write results:
res <- s[c("year", "tow.id", "touchdown.headline", "liftoff.headline")]
names(res) <- c("year", "tow.id", "touchdown", "liftoff")
write.table(res[vars], file = "U:/Snow Crab/Stock Assessment 2019/Touchdown and Swept Area Results/Touchdown and Liftoff - Headline 2019.csv", row.names = FALSE, sep = ",")
