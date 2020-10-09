# Official (traditional) method for calculating swept area:

year <- 2020
s <- read.scsset(year = year, valid = 1)

# Calculate swept areas using variograms:
filter.door.spread <- function(x, y){
   # Time units must be in seconds
   # Removes jumps and outlier values from doorspread time series.

   # Numerical derivative:
   dx = x[1:(length(x)-1)] + diff(x) / 2;
   dy = diff(y) / diff(x)

   index <- list()

   # Create index for good jump values spread values
   w = c(0.66013124790083, 0.26192618962672, 0.07794256247246)
   mu = c(0.00824951746954, 0.11118260831751, -1.71913946320026)
   sigma = c(0.11368746901165, 1.11954351176077, 7.15360493441592)

   # Mixture probability:
   p <- cbind(w[1] * pnorm(dy, mu[1], sigma[1]),
              w[2] * pnorm(dy, mu[2], sigma[2]),
              w[3] * pnorm(dy, mu[3], sigma[3]))
   index[[1]] <- (p[,1] / apply(p, 1, sum)) < 0.5
   index[[1]] <- c(index[[1]], FALSE) | c(FALSE, index[[1]])

   # Create index for good door spread values
   w = c(0.33068949584221, 0.61082187033402, 0.05848863382376)
   mu = c(3.27205471234792, 8.22742249966165, 42.31317985456323)
   sigma = c(1.25711929204254, 2.97051565272387, 30.29589148065443)

   p <- cbind(w[1] * pnorm(y, mu[1], sigma[1]),
              w[2] * pnorm(y, mu[2], sigma[2]),
              w[3] * pnorm(y, mu[3], sigma[3]))
   index[[2]] <- (p[,3] / apply(p, 1, sum)) < 0.5

   index <- !index[[1]] & index[[2]]

   return(index)
}

variogram.fit <- function(x, y){
   # Distance matrix:
   H <- dist(x)
   H <- as.matrix(H)[lower.tri(H)]

   # Empirical semivariance:
   V <- dist(y)
   V <- ((as.matrix(V)[lower.tri(V)])^2) / 2

   ss <- function(theta, H, V){
      # Parse parameters:
      range <- abs(theta[1])
      sill <- abs(theta[2])

      # Model semivariance:
      Vhat <- sill * (((3*H) / (2*range)) - ((H^3) / (2*(range^3))))
      Vhat[H > range] <- sill

      # Sum-of-squares:
      ss <- sum((V-Vhat)^2)

      return(ss)
   }

   # Fit parameters:
   theta <- c(100, mean(V))
   for (j in 1:50) theta <- abs(optim(theta, ss, control = list(maxit = 5000, trace = 0), H = H, V = V)$par)

   names(theta) <- c("range", "sill")

   return(theta)
}

swept.area <- rep(NA, nrow(s))
h <- seq(0, 300)
# Loop over tows:
clg()
for (i in 1:nrow(s)){
   print(i)

   # Load E-sonar data:
   if (s$tow.id[i] == "GP276F") e <- data.frame() else e <- read.esonar(s[i,])

   if (length(e) > 0) e <- e[!is.na(wingspread(e)), ]
   if (nrow(e) > 0){
      e$speed <- 1000 * 1.852 * e$speed / 3600  # Knots to meters per second conversion.
      e$wingspread <- wingspread(e)
      e <- aggregate(e[c("speed", "wingspread")], by = e[c("date", "time")], mean, na.rm = TRUE)

      # Truncate data between start and end times:
      index <- (time(e) >= time(s[i, ], "touchdown")) & (time(e) <= time(s[i, ], "stop"))
      e <- e[index, ]
   }

    if (nrow(e) > 0){
      # Calculate instantaneous swept area:
      e$swept.area.rate <- e$speed * e$wingspread
      e$time <- time2sec(time(e), time(s[i, ], "touchdown"))

      # Filter outliers using model:
      index <- filter.door.spread(e$time, e$swept.area.rate)
      e <- e[index, ]
   }

   if (nrow(e) >= 7){
      # Fit variogram to data:
      theta <- variogram.fit(e$time, e$swept.area.rate)

      Vhat <- theta["sill"] * (((3*h) / (2*theta["range"])) - ((h^3) / (2*(theta["range"]^3))))
      Vhat[h > theta["range"]] <- theta["sill"]

      # Interpolate:
      #C = v$sill - v$vfun(as.matrix(dist(e$time)))
      H <- as.matrix(dist(e$time))
      V <- theta["sill"] * (((3*H) / (2*theta["range"])) - ((H^3) / (2*(theta["range"]^3))))
      V[H > theta["range"]] <- theta["sill"]
      C <- theta["sill"] - V
      duration <- as.numeric(time(s[i,], "stop") - time(s[i,], "touchdown"))
      t0 <- 0:round(duration * 60)
      t0 <- setdiff(t0, e$time)
      h0 <- abs(repvec(t0, ncol = length(e$time)) - repvec(e$time, nrow = length(t0)))
      V0 <- theta["sill"] * (((3*h0) / (2*theta["range"])) - ((h0^3) / (2*(theta["range"]^3))))
      V0[h0 > theta["range"]] <- theta["sill"]
      C0 <- theta["sill"] - V0
      #C0 <- v$sill - v$vfun(h0)

      # Augment linear system for ordinary kriging:
      C <- rbind(cbind(C, 1),1)
      C[nrow(C), ncol(C)] <- 0
      C0 <- cbind(C0, 1)

      mu <- (t(solve(C) %*% t(C0))[,1:length(e$time)] %*% t(t(e$swept.area.rate )))[, 1]
      t <- c(e$time, t0)
      mu <- c(e$swept.area.rate, mu)
      mu <- mu[order(t)]
      t <- t[order(t)]

      # Calculate integrated swept area:
      swept.area[i] <- sum(mu)
   }
}
names(swept.area) <- s$tow.id

# Ad hoc corrections for 2020:
swept.area[c("GP187F", "GP319F", "GP250F", "GP320F", "GP241A1")] <- NA

# Perform 10-nearest neighbour averaging for other tows:
others <- rep(NA, sum(is.na(swept.area)))
names(others) <- names(swept.area[which(is.na(swept.area))])
tmp <- swept.area
index <- s$tow.id %in% names(swept.area[!is.na(swept.area)])
for (i in 1:length(others)){
   print(i)
   j <- which(s$tow.id == names(others)[i])
   d <- distance(longitude(s[j,]), latitude(s[j,]), longitude(s)[index], latitude(s)[index])[1,]
   names(d) <- s$tow.id[index]
   d <- d[order(d)]
   swept.area[j] <- mean(swept.area[names(d[1:10])])
}

# Define swept area method:
s$swept.area.method <- ""
s$swept.area.method[s$valid == 1] <- "model"
s$swept.area.method[s$valid == 1 & s$tow.id %in% names(others)] <- "average"

# Attach swept area:
s$swept.area[match(names(swept.area), s$tow.id)] <- swept.area
s$swept.area <- round(s$swept.area)

# Output to file:
write.csv(s[c("date", "tow.id", "swept.area", "swept.area.method")], file = paste0("data/raw/scs.swept.area.", 2020, ".csv"), row.names = FALSE)

