library(gulf)
library(mgcv)

year <- 2019 

source("U:/Trawl Analyses/Tilt Analysis/Tilt Functions.R")
source("U:/Trawl Analyses/PLM Functions/PLM Kernel Functions.R")

s <- read.scset(year = year, valid = 1)
res <- NULL

clg()
plot(c(0, 5), c(0, 3), type = "n")
tows <- setdiff(1:nrow(s), c(4, 32, 34, 48, 76, 83, 101, 135, 186))
cols <- rainbow(length(tows))
k <- 1
h <- NULL
for (i in tows){
   f <- read.esonar(year = year, tow.id = s$tow.id[i], type = "tilt")

   # Analyze tilt angle data for a single tow:
   if (!is.null(f)){
      f$time <- time2min(time(f), touchdown(s[i, ]))
    
      index <- which(f$time >= 0 & f$time <= 5)
      lines(f$time[index], f$speed[index], lwd = 2, col = cols[k])
      k <- k + 1
      
      h <- rbind(h, c(mean(f$speed[index]), sd(f$speed[index])))
   }
}
plot(h[, 1], h[, 2])
plot(h[, 1])
plot(h[, 2])

clg()
plot(c(0, 5), c(0, 15), type = "n", ylim = c(0, 15), yaxs = "i", xaxs = "i", xlab = "Time(min)", ylab = "Wind spread(m)")
tows <- setdiff(1:nrow(s), c(4, 32, 34, 48, 76, 83, 101, 135, 186))
cols <- rainbow(length(tows))
k <- 1
h <- NULL
grid()
for (i in tows){
   f <- read.esonar(year = year, tow.id = s$tow.id[i], type = "tilt")

   # Analyze tilt angle data for a single tow:
   if (!is.null(f)){
      f$time <- time2min(time(f), touchdown(s[i, ]))
    
      index <- which(f$time >= 0 & f$time <= 5)
      points(f$time[index], f$doormaster[index], pch = 21, bg = cols[k], cex = 0.75)
      k <- k + 1
      
      h <- rbind(h, c(mean(f$doormaster[index]), sd(f$doormaster[index])))
   }
}
box()
plot(h[, 1], h[, 2])
plot(h[, 1])
plot(h[, 2])

clg()
tows <- setdiff(setdiff(1:nrow(s), c(3, 4, 16, 27, 28, 195)), 1:195)
for (i in tows){
   f <- read.star.oddi(year = year, tow.id = s$tow.id[i], type = "tilt")

   # Analyze tilt angle data for a single tow:
   if (!is.null(f)){
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
      theta <- fit.tilt(f$time, f$tilt.x, theta = theta, reps = 10)
   
      clg()
      windows()
      plot.tilt(f$time, f$tilt.x, theta)
      title(main = paste0(i, ")  ", s$tow.id[i]))
   
      res <- rbind(res, cbind(data.frame(survey = "sc", year = f$year[1], tow.id = s$tow.id[i]), t(as.data.frame(theta))))
   }
}

str <- names(res)
str <- gsub("xp1", "touchdown", str)
str <- gsub("xp2", "liftoff", str)
rownames(res) <- NULL
names(res) <- str
res$validated <- NA
#write.table(res, file = "U:/Snow Crab/Stock Assessment 2019/SC Survey 2019 Tilt Results 2.csv", row.names = FALSE, sep = ",")

# Add 'validated' column of zeroes and ones:
res <- read.table("U:/Snow Crab/Stock Assessment 2019/SC Survey 2019 Tilt Results.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

# Spot corrections:
res$liftoff[res$tow.id == "GP282F"] <- 14.68
res$liftoff[res$tow.id == "GP187F"] <- 14.07
res$liftoff[res$tow.id == "GP156F"] <- 14.66
res$liftoff[res$tow.id == "GP174A1"] <- 5.32
res$touchdown[res$tow.id == "GP174A1"] <- -0.31
res$liftoff[res$tow.id == "GP149F"] <- 5.94
res$liftoff[res$tow.id == "GP164F"] <- 6.19
res$liftoff[res$tow.id == "GP088F"] <- 5.56
res$touchdown[res$tow.id == "GP088F"] <- -0.086
res$liftoff[res$tow.id == "GP022F"] <- 6.95
res$touchdown[res$tow.id == "GP022F"] <- -0.035
res$touchdown[res$tow.id == "GP030F"] <- -0.058
res$liftoff[res$tow.id == "GP051F"] <- 7.39
res$liftoff[res$tow.id == "GP092A1"] <- 5.41
res$liftoff[res$tow.id == "GP057F"] <- 5.14
res$liftoff[res$tow.id == "GP132F"] <- 5.37
res$touchdown[res$tow.id == "GP132F"] <- 0
res$liftoff[res$tow.id == "GP182F"] <- 5.74
res$liftoff[res$tow.id == "GP264F"] <- 6.37
res$liftoff[res$tow.id == "GP245F"] <- 5.58
res$touchdown[res$tow.id == "GP245F"] <- -0.17
res$liftoff[res$tow.id == "GP206A2"] <- 5.09
res$touchdown[res$tow.id == "GP206A2"] <- -0.104
res$liftoff[res$tow.id == "GP202A1"] <- 5.24
res$touchdown[res$tow.id == "GP202A1"] <- -0.114

write.table(res, file = "U:/Snow Crab/Stock Assessment 2019/SC Survey 2019 Tilt Results.csv", row.names = FALSE, sep = ",")

s <- read.scset(year = year, valid = 1)
index <- match(s$tow.id, res$tow.id)
str <- as.character(start.time(s) + 60* res$liftoff[index])
s$liftoff.time <- unlist(lapply(strsplit(str, " "), function(x) x[2]))
str <- as.character(start.time(s) + 60* res$touchdown[index])
s$start.time <- unlist(lapply(strsplit(str, " "), function(x) x[2]))

tows <- res$tow.id[res$validated == 0]

for (i in 21:30){
   windows(height = 11, width = 8.5)
   plot.scset(s[s$tow.id == tows[i], ])
}

clg()
year <- 2018
s <- read.scset(year = year, valid = 1)
s <- s[!(s$tow.id %in% problems), ]
plot = TRUE
res <- NULL
for (i in 301:nrow(s)){
   print(i)
   f <- read.star.oddi(year = 2018, tow.id = s$tow.id[i], type = "tilt")
   f$time <- time2min(time(f), touchdown(s[i,]))

   #index <- which(f$time >= -3 & f$time <= 7)
   #theta <- init.tilt(f$time[index], f$tilt.x[index], plot = TRUE) 
   
   theta <- init.tilt(f$time, f$tilt.x) 
   index <- which(f$time >= (theta["touchdown"] - 2) & f$time <= (theta["liftoff"] + 2))
   
   loglike.tilt(theta, f$time[index], f$tilt.x[index])

   for (j in 1:10) theta <- optim(theta, loglike.tilt, t = f$time[index], y = f$tilt.x[index], control = list(maxit = 300, trace = 0))$par
   theta <- optim(theta, loglike.tilt, t = f$time[index], y = f$tilt.x[index], control = list(maxit = 5000, trace = 0))$par
   
   # Load Minilog data:
   minilog <- read.minilog(year = year, tow.id = s$tow.id[i])
   minilog$time <- time2min(time(minilog), touchdown(s[i,])) 
   
   # Scale Star-Oddi footrope to Minilog depth:
   tmp <- approx(minilog$time, minilog$depth, f$time[(f$time >= theta["touchdown"]-2) & (f$time <= theta["liftoff"] + 2)])
   tmp$z <- f$pressure[(f$time >= theta["touchdown"]-2) & (f$time <= theta["liftoff"] + 2)]
   beta <- coef(lm(y ~ z, data  = tmp))
   f$depth <- beta[1] + beta[2] * f$pressure
   
   # Load E-Sonar data:
   esonar <- read.esonar(year = year, tow.id = s$tow.id[i])
   esonar$time <- time2min(time(esonar), touchdown(s[i,])) 
   esonar <- esonar[!is.na(esonar$depth), ]
   
   # Scale Star-Oddi footrope to Minilog depth:
   tmp <- approx(minilog$time, minilog$depth, esonar$time[(esonar$time >= theta["touchdown"]-2) & (esonar$time <= theta["liftoff"] + 2)])
   tmp$z <- esonar$depth[(esonar$time >= theta["touchdown"]-2) & (esonar$time <= theta["liftoff"] + 2)]
   if (length(tmp$y) > 2){
      beta <- coef(lm(y ~ z, data  = tmp))
      esonar$depth <- beta[1] + beta[2] * esonar$depth   
   }
   
   clg()
   
   if (plot){
      windows();
      m <- rbind(0, cbind(0, kronecker(1:2, matrix(1, nrow = 4, ncol = 5)) , 0), 0)
      layout(m)
      par(mar = c(0, 0, 0, 0))
      xlim = c(theta["touchdown"]-2, theta["liftoff"] + 2)
      ylim <- range(f$depth[f$time > xlim[1] & f$time < xlim[2]])
      ylim[2] <- ylim[2] + 0.1 * diff(ylim)
      plot(f$time, f$depth, xlim = xlim, ylim = ylim, xlab = "", ylab = "", xaxt = "n", yaxt = "n")
      grid()
      mtext("Depth(meters)", 2, 2.5, cex = 1.5)
      mtext(s$tow.id[i], 3, 2.5, cex = 1.5)
      lines(rep(theta["touchdown"], 2), par("usr")[3:4], lwd = 2, col = "red", lty = "dashed")
      lines(rep(theta["liftoff"], 2), par("usr")[3:4], lwd = 2, col = "red", lty = "dashed")
      axis(2)
      lines(minilog$time, minilog$depth, lwd = 2, col = "green")
      points(esonar$time, esonar$depth, pch = 21, bg = "red", cex = 1.25)
      
      plot(f$time, f$tilt.x, xlim = xlim, ylim = c(-30, 90), pch = 21, bg = "grey", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
      grid()
      lines(f$time, tilt.mu(f$time, theta), lwd = 2, col = "blue")
      lines(f$time, tilt.mu(f$time, theta) + tilt.sigma(f$time, theta), lwd = 2, col = "blue", lty = "dashed")
      lines(f$time, tilt.mu(f$time, theta) - tilt.sigma(f$time, theta), lwd = 2, col = "blue", lty = "dashed")
      lines(rep(theta["touchdown"], 2), par("usr")[3:4], lwd = 2, col = "red", lty = "dashed")
      lines(rep(theta["liftoff"], 2), par("usr")[3:4], lwd = 2, col = "red", lty = "dashed")
      axis(1); axis(2);
      mtext("Time(min)", 1, 3.0, cex = 1.5)
      mtext("Angle (degrees)", 2, 2.5, cex = 1.5)
   }
   
   res <- rbind(res, cbind(year = year, tow.id = s$tow.id[i], as.data.frame(t(theta))))
}

excel(res)
index <- match(res$tow.id, s$tow.id)

plot(depth(longitude(s), latitude(s)), res$touchdown)

plot(res$touchdown * 60)
dbarplot(table(round(res$touchdown * 60)))

grid()

plot(res$liftoff - res$touchdown)

plot(res$bottom.constant)

plot(depth(longitude(s), latitude(s)), exp(res$bottom.log.sigma))

gulf.map(sea = TRUE)
points(longitude(s), latitude(s), pch = 21, cex = 0.25 * exp(res$bottom.log.sigma), bg = "red")

gulf.map(sea = TRUE)
points(longitude(s), latitude(s), pch = 21, cex = 0.25 * (res$liftoff - res$touchdown), bg = "red")

gulf.map(sea = TRUE)
points(longitude(s), latitude(s), pch = 21, cex = 0.5 * sqrt(abs(res$touchdown*60)), bg = "red")

plot(exp(res$touchdown.log.delta)*60, ylim = exp(c(-5, -1))*60)
dbarplot(table(round(exp(res$touchdown.log.delta)*60)))

plot(exp(res$liftoff.log.delta)*60, ylim = c(0, 40))
dbarplot(table(round(exp(res$liftoff.log.delta)*60)), xlim = c(0, 60))
             
grid()

plot(res$bottom.constant)

t <- seq(-1.5, 2, len = 1000)
p1 <- predict(model1, newdata = list(time = t))
p2 <- predict(model2, newdata = list(time = t))

n = length(t)
plot(diff(diff(p1)), ylim = c(-0.0008, 0), xlim = c(300, 500), col = "blue")
lines(diff(diff(p2)), lwd = 2, col = "red") 
       
