
years <- 2017:2019
vars <- c("distance.passive", "duration.passive", "speed.passive", "wing.spread.passive")
res <- NULL
for (i in 1:length(years)){
   print(years[i])
   file <- paste0("U:/Snow Crab/Stock Assessment 2019/Touchdown and Swept Area Results/Touchdown and Liftoff - Tilt ", years[i], ".csv")

   t <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)

   s <- read.scset(year = years[i], valid = 1)
   s <- s[substr(s$tow.id,2,2) != "C", ]
   
   s$start.time <- t$touchdown[match(s$tow.id, t$tow.id)]
   s$liftoff <- t$liftoff[match(s$tow.id, t$tow.id)]
   s[vars] <- NA
   for (j in 1:nrow(s)){
      cat(paste0(j, ") ", "\n"))
      # Load eSonar data:
      e <- read.esonar(s[j,], year = s$year[j])
      if (!is.null(e)){
         # Coordinate conversion:
         tmp <- deg2km(e$longitude, e$latitude)
         e$x <- tmp$x
         e$y <- tmp$y
         
         # Reference times and conversions:
         e$time <- time2sec(time(e), start.time(s[j,]))
         stop.time <- time2sec(end.time(s[j,]), start.time(s[j,]))
         liftoff.time <- time2sec(time.default(paste0(as.character(date(s[j,])), " ", s$liftoff[j], " AST")), start.time(s[j,]))
   
         # Calculate vessel travel distance:
         s$speed.passive[j] <- 0.514444 * mean(e$speed[which(e$time >= stop.time & e$time <= liftoff.time)])
         s$duration.passive[j] <- liftoff.time - stop.time
         s$distance.passive[j] <- s$duration.passive[j] * s$speed.passive[j]
         v <- e$doormaster[which(e$time >= stop.time & e$time <= liftoff.time)]
         v <- v[!is.na(v)]
         v <- v[v < 20]
         if (length(v) > 0) s$wing.spread.passive[j] <- median(v)
      }            
   }
   
   res <- rbind(res, s)
}

boxplot(res$distance.passive ~ res$year, ylim = c(0, 150), yaxs = "i")
mtext("Year", 1, 2.5, cex = 1.5)
mtext("Vessel Travel Distance (meters)", 2, 2.5, cex = 1.5)

boxplot(res$duration.passive ~ res$year, ylim = c(0, 200), yaxs = "i")
mtext("Year", 1, 2.5, cex = 1.5)
mtext("Time before liftoff (s)", 2, 2.5, cex = 1.5)

boxplot(res$speed.passive ~ res$year, ylim = c(0, 2), yaxs = "i")
mtext("Year", 1, 2.5, cex = 1.5)
mtext("Mean vessel speed (m/s)", 2, 2.5, cex = 1.5)

boxplot(res$wing.spread.passive ~ res$year, ylim = c(0, 15), yaxs = "i")
mtext("Year", 1, 2.5, cex = 1.5)
mtext("Mean wing spread (m)", 2, 2.5, cex = 1.5)

tmp <- data.frame(tow.id = unique(substr(s$tow.id,3,5)))
tmp[as.character(years)] <- NA
var <- "duration.passive"
for (i in 1:length(years)){
   ss <- res[res$year == years[i], ]
   index <- match(tmp$tow.id, substr(ss$tow.id, 3, 5))  
   tmp[, as.character(years[i])] <- ss[,var][index] 
}

lim <- c(0, 200)
plot(tmp[, "2017"], tmp[, "2018"], pch = 21, bg = "grey", xlim = lim, ylim = lim, xaxs = "i", yaxs = "i", xlab = paste(var, 2017), ylab = paste(var, 2018))
grid()
abline(0, 1, col = "red", lwd = 2)
box()

plot(tmp[, "2018"], tmp[, "2019"], pch = 21, bg = "grey", xlim = lim, ylim = lim, xaxs = "i", yaxs = "i", xlab = paste(var, 2018), ylab = paste(var, 2019))
grid()
abline(0, 1, col = "red", lwd = 2)
box()

plot(tmp[, "2017"], tmp[, "2019"], pch = 21, bg = "grey", xlim = lim, ylim = lim, xaxs = "i", yaxs = "i", xlab = paste(var, 2017), ylab = paste(var, 2019))
grid()
abline(0, 1, col = "red", lwd = 2)
box()

# Comparative comparison:
vars <- c("distance.passive", "duration.passive", "speed.passive")
res <- NULL
file <- paste0("U:/Snow Crab/Stock Assessment 2019/Touchdown and Swept Area Results/Touchdown and Liftoff - Tilt 2019.csv")
t <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)
s <- read.scset(year = 2019, valid = 1)

jm <- s[substr(s$tow.id,2,2) == "C", ]
av <- s[substr(s$tow.id,3,5) %in% substr(jm$tow.id, 3, 5) & substr(s$tow.id,2,2) != "C", ]

jm$start.time <- t$touchdown[match(jm$tow.id, t$tow.id)]
jm$liftoff    <- t$liftoff[match(jm$tow.id, t$tow.id)]
av$start.time <- t$touchdown[match(av$tow.id, t$tow.id)]
av$liftoff    <- t$liftoff[match(av$tow.id, t$tow.id)]

jm[vars] <- NA
for (j in 1:nrow(jm)){
   cat(paste0(j, ") ", "\n"))
   # Load eSonar data:
   e <- read.esonar(jm[j,], year = jm$year[j])
   if (!is.null(e)){
      # Reference times and conversions:
      e$time <- time2sec(time(e), start.time(jm[j,]))
      stop.time <- time2sec(end.time(jm[j,]), start.time(jm[j,]))
      liftoff.time <- time2sec(time.default(paste0(as.character(date(jm[j,])), " ", jm$liftoff[j], " AST")), start.time(jm[j,]))
   
      # Calculate vessel travel distance:
      jm$speed.passive[j] <- 0.514444 * mean(e$speed[which(e$time >= stop.time & e$time <= liftoff.time)])
      jm$duration.passive[j] <- liftoff.time - stop.time
      jm$distance.passive[j] <- jm$duration.passive[j] * jm$speed.passive[j]
   }            
}
   
av[vars] <- NA
for (j in 1:nrow(av)){
   cat(paste0(j, ") ", "\n"))
   # Load eSonar data:
   e <- read.esonar(av[j,], year = av$year[j])
   if (!is.null(e)){
      # Reference times and conversions:
      e$time <- time2sec(time(e), start.time(av[j,]))
      stop.time <- time2sec(end.time(av[j,]), start.time(av[j,]))
      liftoff.time <- time2sec(time.default(paste0(as.character(date(av[j,])), " ", av$liftoff[j], " AST")), start.time(av[j,]))
   
      # Calculate vessel travel distance:
      av$speed.passive[j] <- 0.514444 * mean(e$speed[which(e$time >= stop.time & e$time <= liftoff.time)])
      av$duration.passive[j] <- liftoff.time - stop.time
      av$distance.passive[j] <- av$duration.passive[j] * av$speed.passive[j]
   }            
}

plot(jm$distance.passive, av$distance.passive, xlim = c(0, 140), ylim = c(0, 140))
abline(0, 1, lwd = 2, col = "red")

