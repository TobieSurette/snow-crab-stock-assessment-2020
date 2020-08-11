standardize.star.oddi <- function(x){
   # Standardize probe signal with respect to touchdown time.
   
   # Calculate touchdown time:
   tp <- touchdown(x)
   tp <- as.numeric(tp - time(x[1, ]), units = "secs") + 1
   
   # Convert time to seconds:
   x$time <- as.numeric(time(x) - time(x[1, ]), units = "secs") + 1

   # Center time series on touchdown time:
   t <- x$time - tp

   # Define response variable:
   if ("pressure" %in% names(x)) y <- x$pressure else y <- x$depth
   
   # Standardize to initial values:
   scale <- mean(y[(t >= 0) & (t <=120)])
   y <- (y / scale) - 1

   yy <- y[(t >= -60) & (t <= 0)]
   tt <- t[(t >= -60) & (t <= 0)]

   slope <- coef(lm(yy ~ tt - 1))

   y <- (1/120) * (y / slope) + 1
   
   res <- list(t = t, y = y, tp = tp, scale = function(y) return((1/120)*(((y/scale)-1)/slope) + 1))

   x$time.relative <- t
   x$scaled <- y
   return(x)
}
