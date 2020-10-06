library(gulf.data)
library(gulf.graphics)
library(gulf.spatial)
library(mgcv)

year <- 2020

source("Tilt Analysis/Tilt Functions.R")
source("PLM Functions/PLM Kernel Functions.R")

# Read survey set data:
s <- read.scsset(year = year, valid = 1)

# Load touchdown and liftoff initial value:
inits <- read.csv("data/raw/scs.touchdown.inits.2020.csv", header = TRUE, stringsAsFactors = FALSE)
tmp <- read.csv("data/raw/scs.liftoff.inits.2020.csv", header = TRUE, stringsAsFactors = FALSE)
inits$liftoff <- tmp$liftoff[match(inits$tow.id, tmp$tow.id)]
rm(tmp)

res <- NULL

clg()
tows <- s$tow.id[!(s$tow.id %in% res$tow.id)]
for (i in 1:length(tows)){
   ii <- which(s$tow.id == tows[i])
   print(ii)
   f <- read.star.oddi(s[ii, ], probe = "tilt")

   # Analyze tilt angle data for a single tow:
   if (!is.null(f)){
      t <- time2min(time(f), start.time(s[ii, ]))
      y <- f$"tilt-x"

      # Define initial values:
      tmp <- inits[inits$tow.id == s$tow.id[ii], ]
      theta <- c(touchdown = time2min(as.POSIXlt(paste(tmp$date, tmp$touchdown, "ADT")), start.time(s[ii, ])),
                 liftoff = time2min(as.POSIXlt(paste(tmp$date, tmp$liftoff, "ADT")), start.time(s[ii, ])))
      theta <- init.tilt(t, f$"tilt-x", theta = theta)

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
      index <- which(t >= (theta["xp1"] - 2) & t <= (theta["xp2"] + 2))

      #clg()
      #dev.new()
      #plot.tilt(t[index], y[index], theta)
      theta <- fit.tilt(t[index], y[index], theta = theta, reps = 10)

      clg()
      dev.new()
      plot.tilt(t[index], y[index], theta)
      title(main = paste0(ii, ")  ", s$tow.id[ii]))

      res <- rbind(res, cbind(data.frame(project = "scs", year = s$date[ii], tow.id = s$tow.id[ii]), t(as.data.frame(theta))))
   }
}

str <- names(res)
str <- gsub("xp1", "touchdown", str)
str <- gsub("xp2", "liftoff", str)
rownames(res) <- NULL
names(res) <- str
res$validated <- NA

# Convert to absolute time:
s <- read.scsset(year = 2020, valid = 1)
index <- match(s$tow.id, res$tow.id)
str <- as.character(start.time(s) + 60* res$liftoff[index])
s$liftoff.time <- unlist(lapply(strsplit(str, " "), function(x) x[2]))
str <- as.character(start.time(s) + 60* res$touchdown[index])
s$touchdown.time <- unlist(lapply(strsplit(str, " "), function(x) x[2]))


# Write to file:
write.csv(s[c("date", "tow.id", "touchdown.time")], file = "scs.touchdown.time.2020.csv", row.names = FALSE)
write.csv(s[c("date", "tow.id", "liftoff.time")], file = "scs.liftoff.time.2020.csv", row.names = FALSE)


head(s)

