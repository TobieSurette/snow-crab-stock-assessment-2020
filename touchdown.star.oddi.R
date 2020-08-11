touchdown.star.oddi <- function(x){
   # Determine touchdown time.
   
   # Convert time to seconds:
   tx <- time(x)
   x$time <- as.numeric(tx - time(x[1, ]) + 1, units = "secs")

   # Define predictor and response variables:
   t <- x$time
   if ("pressure" %in% names(x)){
      y <- x$pressure
      pressure <- TRUE
   }else{
      y <- x$depth
      pressure <- FALSE
   }
   
   # Fill out 't' and interpolate 'y':
   if (any(unique(diff(t)) > 1)){
      ti <- min(t):max(t)
      yi <- approx(t, y, ti)$y
      t <- ti
      y <- yi
   }
   
   # Evaluate piecewise-linear prototype model:
   k <- 60 # Length of model evaluations.
   if (pressure) slope <- 1/30 else slope <- 0.31
   my <- c(slope * ((1-round(k/2)):0), rep(0, round(k/2)))
   
   # Isolate relevant data:
   index <- (y >= (max(y) / 2))
   tt <- t[index]
   yy <- y[index]
   
   # Number of observations:
   n <- length(tt)

   ss <- NULL
   for (i in 1:(n-k)){
      mu <- mean(yy[i:(i+k-1)] - my)
      ss[i] <- sum((yy[i:(i+k-1)] - my - mu)^2)
   }
   # Apply local smooth:
   for (i in 1:5) ss[2:(length(ss)-1)] <- 0.25*ss[1:(length(ss)-2)] + 0.5*ss[2:(length(ss)-1)] + 0.25*ss[3:length(ss)]
   
   # Find local minima:
   dss <- diff(ss)
   index <- which((dss[1:(length(dss)-1)] <= 0) & (dss[2:length(dss)] >= 0)) + round(k/2) + 1
   
   # Eliminate solutions which are not at the bottom:
   index <- index[(abs(yy[index] - max(yy)) / max(yy)) < 0.1]
   
   # Find best solution:
   index <- index[which(ss[index - round(k/2) - 1] == min(ss[index - round(k/2) - 1]))[1]]

   return(tx[tt[index]])
}
