transition <- function(x, skewness = 0, degree = 0, xp = 0, width = 2, range){
   # TRANSITION - Transition kernel (2nd order derivative) for piecewise linear models.

   # Initialize result variable:
   v <- rep(0, length(x))

   # Convert skewness to proportions:
   p <- 1 / (1+exp(-skewness))

   # Location and scale transforms:
   if (!missing(range)){
      width <- diff(range)
      xlim <- c(xp-(1-p)*width, xp+p*width) # Domain bounds.
      xp <- range[1] + (xp-xlim[1])
   }
   x <- (2 * (x-xp)) / width

   # Skewness scaling:
   x[x <= 0] <- x[x <= 0] / (2*(1-p))
   x[x >= 0] <- x[x >= 0] / (2*p)

   # Dirac delta function:
   if (degree == -1) v[x == 0] <- 1

   # Constant basis:
   if (degree == 0) v[(x >= -1) & (x <= 1)] <- 0.5

   # Linear basis:
   if (degree == 1){
      ix <- (x >= -1) & (x <= 0)
      v[ix] <- 1+x[ix]
      ix <- (x >= 0) & (x <= 1)
      v[ix] <- 1-x[ix]
   }

   # Quadratic basis:
   if (degree == 2) v[x >= -1 & x <= 1] <- 0.75*(1-(x[x >= -1 & x <= 1]^2))

   # Cubic basis:
   if (degree == 3){
      ix <- (x >= -1) & (x <= 0)
      v[ix] <- -2 * x[ix]^3 - 3 * x[ix]^2 + 1
      ix <- (x >= 0) & (x <= 1)
      v[ix] <- 2 * x[ix]^3 - 3 * x[ix]^2 + 1
   }

   # Quartic basis:
   if (degree == 4){
      ix <- (x >= -1) & (x <= 0)
      v[ix] <- -3 * x[ix]^4 - 8 * x[ix]^3 - 6 * x[ix]^2 + 1
      ix <- (x >= 0) & (x <= 1)
      v[ix] <- -3 * x[ix]^4 + 8 * x[ix]^3 - 6 * x[ix]^2 + 1
      v <- 1.25 * v
   }

   return(v)
}

sigmoid <- function(x, skewness = 0, degree = 0, xp = 0, width = 2, range){
   # SIGMOID - Sigmoid kernel (1st order derivative) for piece-wise linear models.

   # Convert skewness to proportions:
   p <- 1 / (1+exp(-skewness))

   # Location and scale transforms:
   if (!missing(range)){
      width <- diff(range)
      xlim  <- c(xp - (1-p) * width, xp + p * width) # Domain bounds.
      xp    <- range[1] + (xp - xlim[1])
   }
   x <- (2 * (x-xp)) / width

   # Skewness scaling:
   x[x <= 0] <- x[x <= 0] / (2*(1-p))
   x[x >= 0] <- x[x >= 0] / (2*p)

   # Sigmoid of a Dirac kernel (step function):
   v <- as.numeric(x >=0)

   # Constant kernel basis (linear sigmoid):
   if (degree == 1){
      ix <- (x >= -1) & (x <= 1)
      v[ix] <- 0.5*(x[ix]+1)
   }

   # Linear kernel basis (quadratic sigmoid):
   if (degree == 2){
      ix <- (x >= -1) & (x <= 0)
      v[ix] <- 0.5*x[ix]^2 + x[ix] + 0.5
      ix <- (x >= 0) & (x <= 1)
      v[ix] <- -0.5*x[ix]^2 + x[ix] + 0.5
   }

   # Quadratic kernel basis (cubic sigmoid):
   if (degree == 3){
      ix <- (x >= -1) & (x <= 1)
      v[ix] <- -0.25*x[ix]^3 + 0.75*x[ix]+0.5
   }

   # Cubic kernel basis (quartic sigmoid):
   if (degree == 4){
      ix <- (x >= -1) & (x <= 0)
      v[ix] <- -0.5 * x[ix]^4 - x[ix]^3 + x[ix] + 0.5
      ix <- (x >= 0) & (x <= 1)
      v[ix] <- 0.5 * x[ix]^4 - x[ix]^3 + x[ix] + 0.5
   }

   # Quartic basis:
   if (degree == 5){
      ix <- (x >= -1) & (x <= 0)
      v[ix] <- -3 * x[ix]^4 - 8 * x[ix]^3 - 6 * x[ix]^2 + 1
      ix <- (x >= 0) & (x <= 1)
      v[ix] <- -3 * x[ix]^4 + 8 * x[ix]^3 - 6 * x[ix]^2 + 1
      v <- 1.25 * v
   }

   # Re-normalize:
   v[x <= 0] <- 2*(1-p) * v[x <= 0]
   v[x >= 0] <- 2*p * (v[x >= 0] - 0.5) + (1-p)

   return(v)
}

plm <- function(x, skewness = 0, degree = 1, xp = 0, width = 2, range){
   # PLM - Piece-wise linear model kernels.

   # Standard piecewise linear model (deg = 1):
   v <- rep(0, length(x))

   # Convert skewness to proportions:
   p <- 1 / (1+exp(-skewness))

   # Location and scale transforms:
   if (!missing(range)){
      width <- diff(range)
      xlim <- c(xp-(1-p)*width, xp+p*width) # Domain bounds.
      xp <- range[1] + (xp-xlim[1])
   }
   x <- (2 * (x-xp)) / width

   # Skewness scaling:
   x[x <= 0] <- x[x <= 0] / (2*(1-p))
   x[x >= 0] <- x[x >= 0] / (2*p)

   # Dirac kernel basis (linear plm):
   v[x >= 0] <- x[x >= 0]

   # Quadratic kernel basis (quadratic plm):
   if (degree == 2){
      ix <- (x >= -1) & (x <= 1)
      v[ix] <- 0.25*x[ix]^2 + 0.5*x[ix] + 0.25
   }

   # Linear kernel basis (cubic plm):
   if (degree == 3){
      ix <- (x >= -1) & (x <= 0)
      v[ix] <- (1/6)*x[ix]^3 + 0.5*x[ix]^2 + 0.5*x[ix] + (1/6)
      ix <- (x >= 0) & (x <= 1)
      v[ix] <- -(1/6)*x[ix]^3 + 0.5*x[ix]^2 + 0.5*x[ix] + (1/6)
   }

   # Quadratic kernel basis (quartic plm):
   if (degree == 4){
      ix <- (x >= -1) & (x <= 1)
      v[ix] <- -(1/16) * x[ix]^4 + (3/8) * x[ix]^2 + 0.5 * x[ix] + (3/16)
   }

   # Cubic kernel basis (quintic plm):
   if (degree == 5){
      ix <- (x >= -1) & (x <= 0)
      v[ix] <- -0.1 * x[ix]^5 - 0.25 * x[ix]^4 + 0.5 * x[ix]^2 + 0.5 * x[ix] + 3/20
      ix <- (x >= 0) & (x <= 1)
      v[ix] <- 0.1 * x[ix]^5 - 0.25 * x[ix]^4 + 0.5 * x[ix]^2 + 0.5 * x[ix] + 3/20
   }

   v[x <= 0] <- 2*(1-p) * v[x <= 0]
   if (degree == 2) v[x >= 0] <- 2*p * (v[x >= 0] - 0.25) + 2*(1-p)*0.25
   if (degree == 3) v[x >= 0] <- 2*p * (v[x >= 0] - (1/6)) + 2*(1-p)*(1/6)
   if (degree == 4) v[x >= 0] <- 2*p * (v[x >= 0] - (3/16)) + 2*(1-p)*(3/16)
   if (degree == 5) v[x >= 0] <- 2*p * (v[x >= 0] - (3/20)) + 2*(1-p)*(3/20)

   v <- (width / 2) * v

   return(v)
}
