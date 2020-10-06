hline <- function(x, ...) for (i in 1:length(x)) lines(par("usr")[1:2], rep(x[i], 2), ...)
vline <- function(x, ...) for (i in 1:length(x)) lines(rep(x[i], 2), par("usr")[3:4], ...)

init.tilt <- function(t, y, buffer = 1, theta, plot = FALSE){
   # INIT.TILT - Initialize tilt model parameter vector.

   # Values to search over:
   tt <- seq(max(-4, min(t)+buffer), min(15, max(t)-buffer), by = 0.05)
   delta <- NA * tt

   # Do sweeps:
   for (i in 1:length(tt)){
      delta[i] <- mean(y[(t <= tt[i] + buffer) & (t >= tt[i])]) -
                  mean(y[(t >= tt[i] - buffer) & (t <= tt[i])])
   }

   # Build parameter vector:
   tmp <- c(touchdown = tt[tt < 3][which.min(delta[tt < 3])],
            liftoff = tt[tt > 3][which.max(delta[tt > 3])])
   if (!missing(theta)){
      if ("touchdown" %in% names(theta)) tmp[["touchdown"]] <- theta[["touchdown"]]
      if ("liftoff" %in% names(theta))   tmp[["liftoff"]]   <- theta[["liftoff"]]
   }
   theta <- tmp

   if (plot){
      dev.new()
      plot(tt, delta)
      lines(rep(theta["touchdown"], 2), par("usr")[3:4], lty = "dashed", col = "red", lwd = 2)
      lines(rep(theta["liftoff"], 2), par("usr")[3:4], lty = "dashed", col = "red", lwd = 2)

      dev.new()
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

parse.theta <- function(theta){
   # PARSE.THETA -

   v <- list(xp = theta[grep("xp", names(theta))],
             slope = theta[grep("slope", names(theta))],
             log.w = theta[grep("log.w", names(theta))],
             log.sigma = theta[grep("log.sigma", names(theta))],
             intercept = theta[grep("intercept", names(theta))])

   # Order parameters:
   if (length(v$xp) > 0) v$xp <- sort(v$xp)
   for (i in 2:length(v)){
      num <- as.numeric(gsub("[a-zA-Z._]", "", names(v[[i]])))
      if (!any(is.na(num))) v[[i]] <- v[[i]][order(num)]
   }

   return(v)
}

mu.tilt <- function(x, theta, xp = 0, slope, intercept = 0, width, degree = 2){
   # MU.TILT - Tilt angle mean model.
   if (!missing(theta)){
      theta <- parse.theta(theta)
      xp <- theta$xp
      slope <- theta$slope
      w <- exp(theta$log.w)
      intercept <- theta$intercept
   }

   # MU.TILT - Mean tilt angle model.
   k <- length(xp)  # Model order.
   if (length(w) == 1) w <- rep(w, k)
   if (length(slope) != (2*k+1)) stop("Number of pivot points is not correct.")

   y <- intercept + slope[1] * x
   for (i in 1:k){
     y <- y + diff(slope[(2*i-1):(2*i)]) *  plm(x, xp = xp[i]-w[i]/2, width = w[i], degree = degree) +
              diff(slope[(2*i):(2*i+1)]) *  plm(x, xp = xp[i]+w[i]/2, width = w[i], degree = degree)
   }

   return(y)
}

sigma.tilt <- function(x, theta, xp = 0, sigma, w, degree = 3){
   # SIGMA.TILT - Tilt angle model error.
   if (!missing(theta)){
      theta <- parse.theta(theta)
      xp <- theta$xp
      w <- exp(theta$log.w)
      sigma <- exp(theta$log.sigma)
   }

   k <- length(xp)  # Model order.
   if (length(w) == 1) w <- rep(w, k)
   if (length(sigma) != (k+1)) stop("Number of pivot points is not correct.")

   y <- sigma[1]
   for (i in 1:k){
      y <- y + (sigma[i+1]-sigma[i]) * sigmoid(x, xp = xp[i], width = w[i], degree = degree)
   }

   return(y)
}

loglike.tilt <- function(theta, x, y, fixed){
   # LOGLIKE.TILT - Log-likelihood function for tilt angle model.

   if (!missing(fixed)) theta <- c(theta, fixed)
   v <- dnorm(y, mu.tilt(x, theta), sigma.tilt(x, theta), log = TRUE)

   return(-sum(v))
}

plot.tilt <- function(x, y, theta, xlab = "", ylab = "", ...){
   if (is.null(dev.list())) dev.new(width = 8.5, height = 10)

   if ("star.oddi" %in% names(x)){
      m <- kronecker(matrix(1:2, ncol = 1), matrix(1, nrow = 4, ncol = 5))
      m <- rbind(0, cbind(0, m, 0), 0, 0)
      layout(m)
      par(mar = c(0, 0, 0, 0))

      # Plot angles:
      plot(x, y, type = "n", xlab = xlab, ylab = ylab, ...)
      grid()
      points(x, y, pch = 21, bg = "grey", cex = 0.7)
      lines(x, mu.tilt(x, theta) - sigma.tilt(x, theta), lwd = 1, lty = "dashed", col = "red")
      lines(x, mu.tilt(x, theta) + sigma.tilt(x, theta), lwd = 1, lty = "dashed", col = "red")
      vline(theta[grep("xp", names(theta))], col = "red", lwd = 2)
   }else{
      plot(x, y, type = "n", xlab = xlab, ylab = ylab, ...)
      grid()
      points(x, y, pch = 21, bg = "grey", cex = 0.7)
      lines(x, mu.tilt(x, theta), lwd = 2, col = "blue")
      lines(x, mu.tilt(x, theta) - sigma.tilt(x, theta), lwd = 1, lty = "dashed", col = "red")
      lines(x, mu.tilt(x, theta) + sigma.tilt(x, theta), lwd = 1, lty = "dashed", col = "red")
      vline(theta[grep("xp", names(theta))], col = "red", lwd = 2)
   }
}

fit.tilt <- function(x, y, theta, reps = 5, trace = 3){
   # FIT.TILT - EStimate tilt model parameters.
   if (missing(theta)) theta <- init.tilt(x, y)

   fixed <- c("xp1", "xp2")
   fixed <- theta[fixed]
   theta <- theta[setdiff(names(theta), names(fixed))]
   for (i in 1:3) theta <- optim(theta, loglike.tilt, x = x, y = y, fixed = fixed, control = list(trace = trace))$par

   theta <- optim(c(theta, fixed), loglike.tilt, x = x, y = y, control = list(trace = trace))$par
   for (i in 1:reps) theta <- optim(theta, loglike.tilt, x = x, y = y, control = list(trace = trace, maxit = 800))$par
   theta <- optim(theta, loglike.tilt, x = x, y = y, control = list(trace = trace, maxit = 5000))$par

   return(theta)
}
