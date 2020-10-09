library(gulf)

source("C:/gulf package/gulf/R/liftoff.scset.R")

year <- 2019 

# Read revised start-end times:
res <- read.table("U:/Snow Crab/Stock Assessment 2019/Tilt Initial Touchdown Liftoff 2019.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

s <- read.scset(year = year, valid = 1)

loglike <- function(theta, x, y){
   # Transition functions:
   p1 <- 1 / (1 + exp(-4*theta[["m1"]]*(x - theta[["touchdown"]])))
   p2 <- 1 / (1 + exp(-4*theta[["m2"]]*(x - theta[["liftoff"]])))
   
   c <- theta[c("c0", "c1", "c2")]
   phi <- 1 / (1 + exp(-theta[c("phi0", "phi1", "phi2")]))
   sigma <- exp(theta[c("sigma0", "sigma1", "sigma2")])
   
   # Smooth parameter transitions:
   c <- c[1] + p1 * (c[2]-c[1]) + p2 * (c[3]-c[2])
   phi <- phi[1] + p1 * (phi[2]-phi[1]) + p2 * (phi[3]-phi[2])
   sigma <- sigma[1] + p1 * (sigma[2]-sigma[1]) + p2 * (sigma[3]-sigma[2])
   
   ll <- dnorm(y[2:n], phi * y[1:(n-1)] + c, sigma, log = TRUE)
   
   return(-sum(ll))
}

for (i in 1:nrow(s)){
   tow.id <- s$tow.id[i]
   t <- read.star.oddi(year = 2019, tow.id = tow.id, type = "tilt")
   t$time <- time2sec(time(t), start.time(s[s$tow.id == tow.id, ]))

   theta <- c(touchdown = res$touchdown[res$tow.id == tow.id] * 60, 
              liftoff = res$liftoff[res$tow.id == tow.id] * 60)

   x <- t$time[(t$time >= (theta[["touchdown"]] - 120)) & (t$time <= (theta[["liftoff"]] + 120))]
   y <- t$tilt.x[(t$time >= (theta[["touchdown"]] - 120)) & (t$time <= (theta[["liftoff"]] + 120))]
   n <- length(x)

   yy <- y[x < theta[["touchdown"]]] 
   model0 <- lm(yy[2:length(yy)] ~ yy[1:(length(yy)-1)])

   yy <- y[x >= theta[["touchdown"]] & x <= theta[["liftoff"]]] 
   model1 <- lm(yy[2:length(yy)] ~ yy[1:(length(yy)-1)])

   yy <- y[x > theta[["liftoff"]]] 
   model2 <- lm(yy[2:length(yy)] ~ yy[1:(length(yy)-1)])

   theta <- c(theta, 
              c(c0 = as.numeric(coef(model0)[1]),
                c1 = as.numeric(coef(model1)[1]),
                c2 = as.numeric(coef(model2)[1]),
                phi0 = as.numeric(coef(model0)[2]),
                phi1 = as.numeric(coef(model1)[2]),
                phi2 = as.numeric(coef(model2)[2]),
                sigma0 = log(summary(model0)$sigma),
                sigma1 = log(summary(model1)$sigma),
                sigma2 = log(summary(model2)$sigma),
                m1 = 0.05,
                m2 = 0.05))             

   theta[grep("phi", names(theta))] <- log(theta[grep("phi", names(theta))] / (1-theta[grep("phi", names(theta))]))
   if (is.na(theta["phi1"])) theta[["phi1"]] = 0.1
    if (is.na(theta["phi0"])) theta[["phi0"]] <- 0.5
   loglike(theta, x, y)
   
   for (j in 1:15) theta <- optim(theta, loglike, x = x, y = y, control = list(trace = 3, maxit = 2000))$par

   clg()
   windows(width = 11)
   plot(x, y)
   vline(theta[c("touchdown", "liftoff")])
   
   s$touchdown[i] <- theta[["touchdown"]] / 60
   s$liftoff[i] <- theta[["liftoff"]] / 60
   s$touchdown.old[i] <- res$touchdown[res$tow.id == tow.id] 
   s$liftoff.old[i] <- res$liftoff[res$tow.id == tow.id]   
   
   title(main = paste0(i, ") ", tow.id))
}

# Revisions:
ii <- c(63,69,94, 102, 107, 113,116, 127, 130, 147, 148, 167, 170, 211, 202, 187, 237, 257, 382, 384)
tows <- c('GP199F','GP202A2','GP176F','GP077F','GP080A1','GP079F','GP066F','GP019F','GP032F','GP003F','GP004F',
           'GP065F','GP033F','GP221A3','GP187F','GP109F','GP209F','GP070A1','GC276F','GC277F')
          
s$touchdown[match(tows, s$tow.id)] <- s$touchdown.old[match(tows, s$tow.id)]


s$touchdown[match(tows, s$tow.id)] <- s$touchdown.old[match(tows, s$tow.id)]



# Reformat:
start.time.logbook end.time.logbook

s$start.time <- s$start.time.logbook
s$end.time <- s$end.time.logbook
s$liftoff.time <- 

s$start.time <- substr(as.character(start.time(s) + s$touchdown * 60), 12, 20)
s$liftoff.time <- substr(as.character(start.time(s) + s$liftoff * 60), 12, 20)


clg()
for (i in 1:length(tows)){
   k <- which(s$tow.id == tows[i])
   tow.id <- s$tow.id[k]
   t <- read.star.oddi(year = 2019, tow.id = tow.id, type = "tilt")
   t$time <- time2min(time(t), start.time(s[s$tow.id == tow.id, ]))

   m <- rbind(0, cbind(0, kronecker(1:2, matrix(1, nrow = 4, ncol = 5)), 0), 0)
   windows(width = 15)
   layout(m)
   par(mar = c(0, 0, 0, 0))
   xlim <- c(s$touchdown.old[k] - 2, s$liftoff.old[k] + 2)
   plot(t$time, t$pressure, xlim = xlim)
   vline(s[k, c("touchdown", "liftoff")])
   vline(s[k, c("touchdown.old", "liftoff.old")], col = "blue")
   
   mtext(paste0(i, ") ", tow.id), 3, 1.5)
   
   plot(t$time, t$tilt.x, xlim = xlim)
   vline(s[k, c("touchdown", "liftoff")])
   vline(s[k, c("touchdown.old", "liftoff.old")], col = "blue")
}


# Isolate comparative tows:
s <- s[substr(s$tow.id, 3, 5) %in% substr(s$tow.id[substr(s$tow.id, 2, 2) == "C"], 3, 5), ]

s$end.time <- NA
s$touchdown <- NA
s$liftoff <- NA
s$temperature <- NA
s$esonar <- NA
for (i in  1:nrow(s)){
   h <- read.star.oddi(year = year, tow.id = s$tow.id[i])
   h$time <- time2min(time(h), start.time(s[i,]))
   s$end.time[i] <- time2min(end.time(s[i, ]), start.time(s[i,]))
   s$touchdown[i] <- res$touchdown[res$tow.id == s$tow.id[i]]
   s$liftoff[i] <- res$liftoff[res$tow.id == s$tow.id[i]]
  
   index <- (h$time >= (s$liftoff[i] - 3)) & (h$time <= (s$liftoff[i]- 1))
   print(sum(index))
   
   s$temperature[i] <- mean(h$temperature[index])
   
   e <- read.esonar(year = year, tow.id = s$tow.id[i])
   e$time <- time2min(time(e), start.time(s[i,]))
   s$esonar[i] <- sum(!is.na(e$doormaster[e$time >= s$touchdown[i] & e$time <= s$liftoff[i]]))
 
}

s[substr(s$tow.id, 2, 2) == "C", ]


