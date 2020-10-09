library(gulf)
library(TMB)
setwd("U:/Snow Crab/Stock Assessment 2019")
 
year <- 2019
   
source("C:/gulf package/gulf/R/liftoff.R")
source("C:/gulf package/gulf/R/liftoff.scset.R")
source("C:/gulf package/gulf/R/read.flowmeter.R")

# These are relative to the LOGBOOK start times:
x <- read.scset(year = year, valid = 1)
times <- read.csv(file = "U:/Snow Crab/Stock Assessment 2019/Touchdown and Liftoff Times 2019.csv", header = TRUE, stringsAsFactors = FALSE)

tmp <- x$start.time
x$start.time <- x$start.time.logbook 
x$end.time <- x$end.time.logbook
x$liftoff.time <- "        "
index <- match(x$tow.id, times$tow.id)
x$liftoff.time <- substr(start.time(x) + times$liftoff[index] * 60, 12, 20)
x$start.time <- tmp

tows <- x$tow.id[substr(x$tow.id,3,5) %in% substr(x$tow.id[substr(x$tow.id,2,2) == "C"], 3, 5)]

clg()
res <- data.frame(tow.id = tows, stringsAsFactors = FALSE)
res$speed.vessel <- NA
res$speed.vessel.passive <- NA
res$flow <- NA
res$flow.passive <- NA
res$duration.passive <- NA
res$speed.trawl.passive <- NA
res$distance.trawl.passive <- NA
for (i in 1:length(tows)){
   f <- read.flowmeter(year = 2019, tow.id = tows[i])
   if (!is.null(f)){
      e <- read.esonar(year = year, tow.id = tows[i])
      e$time <- time2min(time(e), start.time(x[x$tow.id == tows[i], ]))
      
      stop.time <- time2min(end.time(x[x$tow.id == tows[i], ]), start.time(x[x$tow.id == tows[i], ]))
      liftoff.time <- time2min(liftoff(x[x$tow.id == tows[i], ]), start.time(x[x$tow.id == tows[i], ]))

      f$time <- time2min(time(f), start.time(x[x$tow.id == tows[i], ]))
      
      res$speed.vessel[i] <- mean(e$speed[e$time >=0 & e$time <= stop.time])
      res$speed.vessel.passive[i] <- mean(e$speed[e$time >= stop.time & e$time <= liftoff.time])
      res$flow[i] <- mean(f$"Speed.(m/s)"[f$time >=0 & f$time <= stop.time])
      res$flow.passive[i] <- mean(f$"Speed.(m/s)"[f$time >= stop.time & f$time <= liftoff.time])
      res$duration.passive[i] <- 60*(liftoff.time - stop.time)
   }
}

res$speed.trawl.passive <- res$speed.vessel * res$flow.passive / res$flow
res$distance.trawl.passive <- res$duration.passive * 0.514444 * res$speed.trawl.passive

res <- res[!is.na(res$speed.vessel), ]
vars <- c("year", "month", "day", "tow.number")

res <- cbind(x[match(res$tow.id, x$tow.id), vars], res)
res$comparative <- substr(res$tow.id, 2, 2) == "C"
excel(sort(res, by = c(vars, "comparative")))
