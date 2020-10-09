library(gulf)
library(mgcv)

years <- 2017:2019
s <- read.scset(year = years, valid = 1)
#s <- s[substr(s$tow.id, 2, 2) != "C", ]

haul <- NULL
tilt <- NULL
for (i in 1:length(years)){
   tmp <- read.csv(paste0("U:/Snow Crab/Stock Assessment 2019/Touchdown and Swept Area Results/Haul times ", years[i], ".csv"), stringsAsFactors = FALSE)
   if (years[i] == 2019) tmp <- rbind(tmp, read.csv(paste0("U:/Snow Crab/Stock Assessment 2019/Touchdown and Swept Area Results/Haul times comparative ", years[i], ".csv"), stringsAsFactors = FALSE))
   names(tmp) <- c("year", "tow.id", "hour", "minute", "second")
   haul <- rbind(haul, tmp)
   tmp <- read.csv(paste0("U:/Snow Crab/Stock Assessment 2019/Touchdown and Swept Area Results/Touchdown and Liftoff - Tilt ", years[i], ".csv"), stringsAsFactors = FALSE)
   tilt <- rbind(tilt, tmp)
}

haul <- haul[!is.na(haul$hour), ]

# Calculate julian day:
s$julian <- julian(date(s))

vars <- c("year", "tow.id")

# Get passive phase vessel speed and trawl width:
res <- read.csv(paste0("U:/Snow Crab/Stock Assessment 2019/Touchdown and Swept Area Results/Touchdown and Liftoff - Tilt ", 2017, ".csv"), stringsAsFactors = FALSE)
res <- rbind(res, read.csv(paste0("U:/Snow Crab/Stock Assessment 2019/Touchdown and Swept Area Results/Touchdown and Liftoff - Tilt ", 2018, ".csv"), stringsAsFactors = FALSE))
res <- rbind(res, read.csv(paste0("U:/Snow Crab/Stock Assessment 2019/Touchdown and Swept Area Results/Touchdown and Liftoff - Tilt ", 2019, ".csv"), stringsAsFactors = FALSE))
index <- match(s[vars], res[vars])
s$liftoff.time <- res$liftoff[index]
s$end.time[s$end.time == "        "] <- s$end.time.logbook[s$end.time == "        "]

s$wing.spread.mean <- NA
s$wing.spread.median <- NA
for (i in 1:nrow(s)){
   if (i %% 10 == 0) print(i)
   reference.time <- time.default(paste0(as.character(date(s[i, ])), " ", s$start.time[i], " AST")) 
   stop.time <- time2sec(time.default(paste0(as.character(date(s[i, ])), " ", s$end.time[i], " AST")), reference.time)
   liftoff.time <- time2sec(time.default(paste0(as.character(date(s[i, ])), " ", s$liftoff.time[i], " AST")), reference.time) 
               
   e <- read.esonar(year = s$year[i], tow.id = s$tow.id[i])
   if (!is.null(e)){
      e$time <- time2sec(time(e), reference.time)
      index <- which(e$time >= stop.time &  e$time <= liftoff.time)   
      s$passive.speed[i] <- mean(e$speed[index], na.rm = TRUE)
      s$wing.spread.mean[i] <- mean(e$doormaster[index], na.rm = TRUE)
      s$wing.spread.median[i] <- median(e$doormaster[index], na.rm = TRUE)
   }
}

s$wing.spread.mean[which(abs(s$wing.spread.mean - s$wing.spread.median) > 1)] <- NA
s$wing.spread.mean[which((s$wing.spread.mean > 12) | (s$wing.spread.mean < 1))] <- NA

# Fill-in wing spread values:
for (i in 1:length(years)){
   index <- which(!is.na(s$wing.spread.mean) & (s$year == years[i]))
   ss <- s[index, ]
   model <- gam(wing.spread.mean ~ s(julian), data = ss)
   index <- which(is.na(s$wing.spread.mean) & (s$year == years[i]))
   s$wing.spread.mean[index] <- predict(model, newdata = list(julian = s$julian[index]))
}

# Metric conversion:
s$passive.speed <- 0.514444 * s$passive.speed
s$depth <- s$depth * 1.8288
s$warp <- s$warp * 1.8288

#=========================================================================================
# Remove 'redone' label:
s$tow.id <- gsub("R", "", s$tow.id)
haul$tow.id <- gsub("R", "", haul$tow.id)
tilt$tow.id <- gsub("R", "", tilt$tow.id)

vars <- c("year", "tow.id")
index <- match(s[vars], tilt[vars])
s$liftoff <- tilt$liftoff[index]
s$liftoff <- time.default(paste(as.character(date(s)), s$liftoff, "AST"))

s$end.time <- s$end.time.logbook
s$end.time <- end.time(s)

s$passive.duration <- as.numeric(difftime(s$liftoff, s$end.time, unit = "secs"))

index <- match(haul[vars], s[vars])
haul$valid <- s$valid[index]
haul <- haul[!is.na(haul$valid), ]
 
index <- match(haul[vars], s[vars])
haul[c("year", "month", "day")] <- s[index, c("year", "month", "day")]

# Spot correction:
haul <- haul[!(haul$tow.id == "GP353F" & haul$year == 2019 & haul$hour == 9), ]
haul <- haul[!(haul$tow.id == "GP176F" & haul$year == 2017 & haul$hour == 14), ]
haul <- haul[!(haul$tow.id == "GP041F" & haul$year == 2017 & haul$hour == 17), ]
haul <- haul[!(haul$tow.id == "GP151F" & haul$year == 2017 & haul$hour == 18), ]

haul$time <- time.default(haul)
s$haul.time <- haul$time[match(s[vars], haul[vars])]

s$haul.duration <- as.numeric(difftime(s$haul.time, s$end.time, unit = "secs"))

s$haul.duration[s$haul.duration > 2000] <- NA
s$haul.duration[s$haul.duration <= 0] <- NA

index <- which(!is.na(s$haul.duration) & !is.na(s$warp) & s$year == 2017)
model <- gam(haul.duration ~ s(warp), data = s[index, ])
s$haul.duration[index[abs(residuals(model)) > 50]] <- NA
index <- which(!is.na(s$haul.duration) & !is.na(s$warp) & s$year == 2018)
model <- gam(haul.duration ~ s(warp), data = s[index, ])
s$haul.duration[index[abs(residuals(model)) > 50]] <- NA
index <- which(!is.na(s$haul.duration) & !is.na(s$warp) & s$year == 2019)
model <- gam(haul.duration ~ s(warp), data = s[index, ])
s$haul.duration[index[abs(residuals(model)) > 90]] <- NA

# Correct haul durations using warp lengths:
tmp <- aggregate(s$haul.duration, by = s[c("year", "warp")], mean, na.rm = TRUE)
index <- which(is.na(s$haul.duration))
s$haul.duration[index] <- tmp$x[match(s[index, c("year", "warp")], tmp[c("year", "warp")])]

s$winch.speed <- s$warp / s$haul.duration

s$winch.distance <- sqrt(s$warp^2 - s$depth^2) - sqrt((s$warp - s$winch.speed * s$passive.duration)^2 - s$depth^2) 
s$winch.distance[s$winch.distance <= 0] <- NA

s$liftoff.angle <- 180 * asin(s$depth /  (s$warp - s$winch.speed * s$passive.duration)) / pi

# Fill-in winch distance:
for (i in 1:length(years)){
   index <- which(!is.na(s$winch.distance) & (s$year == years[i]))
   ss <- s[index, ]
   model <- gam(winch.distance ~ s(julian), data = ss)
   index <- which(is.na(s$winch.distance) & (s$year == years[i]))
   s$winch.distance[index] <- predict(model, newdata = list(julian = s$julian[index]))
}

# Fill-in passive speed:
index <- which(!is.na(s$passive.speed))
ss <- s[index, ]
model <- gam(passive.speed ~ s(julian), data = ss)
index <- which(is.na(s$passive.speed))
s$passive.speed[index] <- predict(model, newdata = list(julian = s$julian[index]))

s$vessel.distance <- s$passive.speed * s$passive.duration
s$passive.distance <- s$winch.distance + s$vessel.distance
s$passive.swept.area <- s$wing.spread.mean * s$passive.distance
                    
vars <- c("passive.duration", "passive.speed", "winch.speed", "vessel.distance", "winch.distance", "liftoff.angle", "wing.spread.mean", "passive.swept.area")
f <- function(x) c(quantile(x, p = c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm = TRUE), mean = mean(x, na.rm = TRUE))
res <- NULL
for (i in 1:length(vars)){
   tmp <- aggregate(s[vars[i]], by = s["year"], f)
   names(tmp) <- gsub(vars[i], "", names(tmp))
   tmp$variable <- vars[i]
   res <- rbind(res, tmp)
}       

write.csv(s[c("year", "tow.id", "passive.swept.area")], 
          row.names = FALSE,
          file = "U:/Snow Crab/Stock Assessment 2019/Touchdown and Swept Area Results/Passive Swept Area 2017-2019.csv")
               
# Passive distance:
plot(jitter(s$warp[s$year == 2018] / 1.8288, amount = 12.5), s$winch.distance[s$year == 2018], 
     xlim = c(0, 200), xaxs = "i", ylim = c(0, 200), yaxs = "i", ylab = "Winch distance (meters)", xlab = "Warp length (fathoms)")
grid()
points(jitter(s$warp[s$year == 2017] / 1.8288, amount = 12.5), s$winch.distance[s$year == 2017], pch = 21, bg = "blue")
points(jitter(s$warp[s$year == 2019] / 1.8288, amount = 12.5), s$winch.distance[s$year == 2019], pch = 21, bg = "red")

# Liftoff angle:
plot(s$liftoff.angle[s$year == 2018], ylim = c(0, 90), yaxs = "i", xlab = "Tow", ylab = "Liftoff Angle (degrees)", pch = 21, bg = "grey")
grid()
points(s$liftoff.angle[s$year == 2017], pch = 21, bg = "blue")
points(s$liftoff.angle[s$year == 2019], pch = 21, bg = "red")
box()

# Winch distance:
plot(s$winch.distance[s$year == 2018], ylim = c(0, 200), yaxs = "i", xlab = "Tow", ylab = "Winch distance (meters)", pch = 21, bg = "grey")
grid()
points(s$winch.distance[s$year == 2017], pch = 21, bg = "blue")
points(s$winch.distance[s$year == 2019], pch = 21, bg = "red")
box()



