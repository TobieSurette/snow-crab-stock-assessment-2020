library(gulf)
library(mgcv)

year <- 2017 

# Update Star Oddi read files:
source("C:/gulf package/gulf/R/star.oddi.path.str.R")
source("C:/gulf package/gulf/R/star.oddi.file.str.R")
source("C:/gulf package/gulf/R/read.star.oddi.R")

# Convenience functions:
hline <- function(x, col = "red", lty = "solid", ...) for (i in 1:length(x)) lines(par("usr")[1:2], c(x[i], x[i]), lty = lty, col = col, ...)
vline <- function(x, col = "red", lty = "solid", ...) for (i in 1:length(x)) lines(c(x[i], x[i]), par("usr")[3:4], lty = lty, col = col, ...)

source("U:/Trawl Analyses/Tilt Analysis/Tilt Functions.R")
source("U:/Trawl Analyses/PLM Functions/PLM Kernel Functions.R")

# Load tow data:
s <- read.scset(year = year, valid = 1)

# Load initial touchdown and liftoff values:                                         
file <- paste0("U:/Snow Crab/Stock Assessment 2019/Touchdown and Swept Area Results/Touchdown and Liftoff - Inits ", year, ".csv")
inits <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)

# List of tows to remove from analysis due to issues:
remove <- ""

res <- NULL

clg()
for (i in 1:nrow(s)){
   print(i)
   if (!(s$tow.id[i] %in% remove) & !(s$tow.id[i] %in% res$tow.id)){
      f <- read.star.oddi(year = year, tow.id = s$tow.id[i], type = "tilt")

      # Analyze tilt angle data for a single tow:
      if (!is.null(f)){
         f$time <- time2min(time(f), touchdown(s[i, ]))
         
         theta <- NULL
         if (s$tow.id[i] %in% inits$tow.id){
            theta = c(theta, 
                      c(touchdown = time2min(time.default(paste0(as.character(date(s[i, ])), " ", inits$touchdown[inits$tow.id == s$tow.id[i]], " AST")), start.time(s[i, ]))),
                      c(liftoff   = time2min(time.default(paste0(as.character(date(s[i, ])), " ", inits$liftoff[inits$tow.id == s$tow.id[i]], " AST")), start.time(s[i, ]))))
         }   
         
         theta <- init.tilt(f$time, f$tilt.x, theta = theta, plot = FALSE) 
   
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
         theta <- fit.tilt(f$time, f$tilt.x, theta = theta, reps = 10, trace = 0)
   
         clg()
         windows()
         plot.tilt(f$time, f$tilt.x, theta)
         title(main = paste0(i, ")  ", s$tow.id[i]))
   
         res <- rbind(res, cbind(data.frame(survey = "sc", year = f$year[1], tow.id = s$tow.id[i]), t(as.data.frame(theta))))
      }
   }
}

# Save backup
tmp <- res  

# Rename fields:
str <- names(res)
str <- gsub("xp1", "touchdown", str)
str <- gsub("xp2", "liftoff", str)
rownames(res) <- NULL
names(res) <- str

res <- cbind(s[c("year", "tow.id")], data.frame(touchdown = res$touchdown[match(s$tow.id, res$tow.id)], 
                                                liftoff   = res$liftoff[match(s$tow.id, res$tow.id)]))

#if (year == 2017){
#   res$touchdown[res$tow.id == "GP125F"] <- 0.05454588
#   res$liftoff[res$tow.id == "GP125F"] <- 5.06590999
#   res$touchdown[res$tow.id == "GP074F"] <- -0.4052117
#   res$liftoff[res$tow.id == "GP074F"] <- 9.3628664
#   res$touchdown[res$tow.id == "GP157F"] <- 0.05454588
#   res$liftoff[res$tow.id == "GP157F"] <- 5.82954642
#   res$liftoff[res$tow.id == "GP271F"] <- 6.306819
#   res$liftoff[res$tow.id == "GP190F"] <- 5.845456
#   res$touchdown[res$tow.id == "GP348F"] <- -0.1132743
#   res$liftoff[res$tow.id == "GP348F"] <- 7.8407080
#   res$touchdown[res$tow.id == "GP214F"] <- -0.08863596
#   res$liftoff[res$tow.id == "GP214F"] <- 5.65454641
#   res$touchdown[res$tow.id == "GP137F"] <- -0.1204541
#   res$liftoff[res$tow.id == "GP137F"] <- 5.3363646
#   res$touchdown[res$tow.id == "GP263F"] <- -0.009090493
#   res$liftoff[res$tow.id == "GP263F"] <- 5.972728255
#   res$touchdown[res$tow.id == "GP355A1"] <- 0.01818231 
#   res$liftoff[res$tow.id == "GP355A1"] <- 8.63030434
#}

#if (year == 2018){
#   # Ad-hoc spot corrections:
#   res$liftoff[res$tow.id == "GP174A1"] <- 5.29
#   res$liftoff[res$tow.id == "GP164F"] <- 6.168
#   res$liftoff[res$tow.id == "GP088F"] <- 5.45
#   res$liftoff[res$tow.id == "GP034F"] <- 5.27
#}

#if (year == 2019){
#   # Ad-hoc spot corrections:
#   res$liftoff[res$tow.id == "GP226F"] <- 5.481
#   res$touchdown[res$tow.id == "GP003F"] <- 0.037
#   res$liftoff[res$tow.id == "GP003F"]   <- 5.225  
#   res$liftoff[res$tow.id == "GP011F"]   <- 7.165   
#   res$liftoff[res$tow.id == "GP085F"]   <- 9.453
#   res$liftoff[res$tow.id == "GP181F"]   <- 5.828
#   res$touchdown[res$tow.id == "GP221A3"] <- 0.00388
#   res$liftoff[res$tow.id == "GP221A3"]   <- 6.012   
#   res$liftoff[res$tow.id == "GP143F"] <- 6.44   
#   res$liftoff[res$tow.id == "GP294F"] <- 5.85
#   res$touchdown[res$tow.id == "GP240A2"] <- 0.041
#   res$liftoff[res$tow.id == "GP240A2"]   <- 6.677557873
#   res$touchdown[res$tow.id == "GP176F"] <- 0.053
#   res$liftoff[res$tow.id == "GP176F"]   <- 7.447 
#}

# Convert to string format:
index <- !is.na(res$touchdown)
res$touchdown.str <- "        "
res$touchdown.str[index] <- unlist(lapply(strsplit(as.character(start.time(s[index, ]) + round(60 * res$touchdown[index])), " "), function(x) x[2]))
index <- !is.na(res$liftoff)
res$liftoff.str <- "        "
res$liftoff.str[index] <- unlist(lapply(strsplit(as.character(start.time(s[index, ]) + round(60 * res$liftoff[index])), " "), function(x) x[2]))

# Ad-hoc spot corrections:
if (year == 2017){
   res$touchdown[res$tow.id == "GP125F"]  <- "20:42:48"
   res$liftoff[res$tow.id == "GP125F"]    <- "20:47:41"
   res$touchdown[res$tow.id == "GP074F"]  <- "20:28:05"
   res$liftoff[res$tow.id == "GP074F"]    <- "20:33:44"
   res$touchdown[res$tow.id == "GP157F"]  <- "05:52:45"
   res$liftoff[res$tow.id == "GP157F"]    <- "05:58:26"
   res$liftoff[res$tow.id == "GP271F"]    <- "13:47:59"
   res$liftoff[res$tow.id == "GP190F"]    <- "10:47:25"                           
   res$touchdown[res$tow.id == "GP348F"]  <- "10:35:59"
   res$liftoff[res$tow.id == "GP348F"]    <- "10:43:45"
   res$touchdown[res$tow.id == "GP214F"]  <- "11:23:01"
   res$liftoff[res$tow.id == "GP214F"]    <- "11:28:47"
   res$touchdown[res$tow.id == "GP137F"]  <- "14:16:29"
   res$liftoff[res$tow.id == "GP137F"]    <- "14:21:45"
   res$touchdown[res$tow.id == "GP263F"]  <- "14:34:08"
   res$liftoff[res$tow.id == "GP263F"]    <- "14:40:06"
   res$touchdown[res$tow.id == "GP355A1"] <- "15:53:01"
   res$liftoff[res$tow.id == "GP355A1"]   <- "16:01:11"
}

if (year == 2018){
   # Ad-hoc spot corrections:
   res$liftoff[res$tow.id == "GP174A1"] <- "20:57:46"
   res$liftoff[res$tow.id == "GP164F"]  <- "18:38:52"
   res$liftoff[res$tow.id == "GP088F"]  <- "09:51:21"
   res$liftoff[res$tow.id == "GP034F"]  <- "18:17:24"
}

if (year == 2019){  
   res$liftoff.str[res$tow.id == "GP226F"]    <- "07:04:42"
   res$touchdown.str[res$tow.id == "GP003F"]  <- "08:00:01"
   res$liftoff.str[res$tow.id == "GP003F"]    <- "08:05:13"
   res$liftoff.str[res$tow.id == "GP011F"]    <- "13:51:52"
   res$liftoff.str[res$tow.id == "GP085F"]    <- "17:25:07"
   res$liftoff.str[res$tow.id == "GP181F"]    <- "13:29:07"
   res$touchdown.str[res$tow.id == "GP221A3"] <- "08:56:15"
   res$liftoff.str[res$tow.id == "GP221A3"]   <- "09:02:16"
   res$liftoff.str[res$tow.id == "GP143F"]    <- "16:17:58"
   res$liftoff.str[res$tow.id == "GP294F"]    <- "17:26:46"
   res$touchdown.str[res$tow.id == "GP240A2"] <- "11:05:50"
   res$liftoff.str[res$tow.id == "GP240A2"]   <- "11:12:29"
   res$touchdown.str[res$tow.id == "GP176F"]  <- "07:20:25"
   res$liftoff.str[res$tow.id == "GP176F"]    <- "07:27:49"
}

# Spot checks:
clg()
tows <- c("GP174A1", "GP149F", "GP164F", "GP088F", "GP034F")
tows <- c("GP355A1")
m <- kronecker(matrix(1:2), matrix(1, nrow = 4, ncol = 5))
m <- rbind(0, cbind(0, m, 0), 0)
for (i in 1:length(tows)){
   print(tows[i])
   f <- read.star.oddi(year = year, tow.id = tows[i], type = "tilt")
   f$time <- time2min(time(f), touchdown(s[s$tow.id == tows[i], ]))
   d <- read.star.oddi(year = year, tow.id = tows[i])
   if (!is.null(d)) d$time <- time2min(time(d), touchdown(s[s$tow.id == tows[i], ]))
   xlim = c(-2, res$liftoff[res$tow.id == tows[i]]+5)
   
   if (any(is.na(xlim))) xlim = c(-2, 10)
   xlim = c(-2, 14)
   windows(width = 11)
   layout(m)
   par(mar = c(0, 0, 0, 0))
   plot(d$time, d$pressure, xlim = xlim, xaxs = "i")
   mtext(tows[i], 3, 2, cex = 1.5)
   vline(res[res$tow.id == tows[i], c("touchdown", "liftoff")], col = "red", lwd = 2)
   plot(f$time, f$tilt.x, xlim = xlim, xaxs = "i")
   vline(res[res$tow.id == tows[i], c("touchdown", "liftoff")], col = "red", lwd = 2) 
   #print(draw.points()  )
}
 
# Write results:
vars <- c("year", "tow.id", "touchdown", "liftoff")
res$touchdown <- res$touchdown.str
res$liftoff <- res$liftoff.str
file <- paste0("U:/Snow Crab/Stock Assessment 2019/Touchdown and Swept Area Results/Touchdown and Liftoff - Tilt ", year, ".csv")
write.table(res[vars], file = file, row.names = FALSE, sep = ",")


