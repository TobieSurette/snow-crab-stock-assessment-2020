library(gulf)
library(TMB)
setwd("U:/Snow Crab/Stock Assessment 2019")
 
year <- 2019
thin <- 5
reference <- "stop.time"
buffer <- 60 # Number of seconds to leave in the analysis before and after the reference bounds.

source("C:/gulf package/gulf/R/liftoff.R")
source("C:/gulf package/gulf/R/liftoff.scset.R")

parameter.str <- function(x){
   for (i in 1:length(x)){
      cat(paste0(names(x[i]), " = ", as.numeric(x[i]), ", \n"))
   }
}
                   
update.parameters <- function(p, theta){
   for  (i in 1:length(theta)){
      if (names(theta[i]) %in% names(p)){
         if (length(p[names(theta[i])]) == length(theta[i])){
            p[names(theta[i])] <- as.numeric(theta[i])
         }
      }
   }
   return(p)
}   
   
# Define table of data filters:
tmp <- c("330F" = "a", "336A1" = "a", "327F" = "a", "332F" = "a", "322F" = "a", 
         "319F" = "s", "304F" = "s", "318F" = "a", "309F" = "a", "302F" = "s", 
         "288F" = "s", "291F" = "a", "296F" = "a", "297F" = "a", "307F" = "a",
         "305F" = "a",
         "306F" = "a", "299F" = "a", "283F" = "p", "274F" = "p", "286F" = "p",
         "292F" = "s", "298F" = "p", "289F" = "s", "280F" = "p",
         "272F" = "a", "267F" = "p", "256F" = "s", "254F" = "a", "276F" = "a",
         "266F" = "a", "277F" = "a", "284F" = "a", "294F" = "a", "301F" = "a",
         "340F" = "p", "317F" = "p", "311F" = "a", "339F" = "a", "329F" = "a")
table <- data.frame(tow.id = names(tmp), av = as.vector(tmp))
table$jm <- c(NA, NA, NA, NA, NA, "a", "a", NA, "a", "a", "a", NA, NA, "a", NA, "a", "a", "a",  
              "s", "s", "s", "p", "s", "p", "s", "a", "s", "p", "a", "a", "a", "a", "a", "a", "a", "s", "a", "a", "a", "a")
table$av <- as.character(table$av)
table$jm <- as.character(table$jm)

# Read tow data:
x <- read.scset(year = year, valid = 1)

# Read touchdown and liftoff data:
tmp <- read.csv(file = "U:/Snow Crab/Stock Assessment 2019/Touchdown and Swept Area Results/Touchdown and Liftoff - Tilt 2019.csv", header = TRUE, stringsAsFactors = FALSE)
touchdown <- tmp$touchdown
names(touchdown) <- tmp$tow.id
liftoff <- tmp$liftoff
names(liftoff) <- tmp$tow.id

# Import:
x$start.time <- touchdown[x$tow.id]
x$liftoff.time <- liftoff[x$tow.id]
        
vessel <- "av"
#tows <- unique(substr(x$tow.id[substr(x$tow.id, 2, 2) == "C"], 3, 10)) 
data <- NULL
data$t <- NULL
data$width <- NULL
data$tow <- NULL
data$comparative <- NULL
speed <- data
speed$speed <- NULL
res <- NULL
for (i in 1:nrow(x)){
   print(x$tow.id[i])
   
   # Load eSonar file:
   e <- read.esonar(year = year, tow.id = x$tow.id[i])
   e$time <- time2sec(time(e), start.time(x[i, ]))
   
   # Define event times:
   start.time   <- 0
   stop.time    <- time2sec(end.time(x[i, ]), start.time(x[i, ]))
   liftoff.time <- time2sec(liftoff(x[i, ]), start.time(x[i, ]))
   
   # Avalon paring:
   if (x$tow.id[i] %in% gsub("C", "P", x$tow.id[substr(x$tow.id, 2, 2) == "C"])){
      if (table$av[table$tow.id == substr(x$tow.id[i],3,10)] == "p") e <- e[e$hydrophone == "P", ]
      if (table$av[table$tow.id == substr(x$tow.id[i],3,10)] == "s")  e <- e[e$hydrophone == "S", ]   
   }
   
   # Jean Mathieu:
   if (x$tow.id[i] %in% x$tow.id[substr(x$tow.id, 2, 2) == "C"]){       
      if (table$jm[table$tow.id == substr(x$tow.id[i],3,10)] %in% c("s", "p")){
         print(123)
         ee <- read.esonar(year = year, tow.id = gsub("C", "P", x$tow.id[i]))
         ee$time <- time2sec(time(ee), start.time(x[i, ]))
      
         if (table$jm[table$tow.id == substr(x$tow.id[i],3,10)] == "p") ee <- ee[ee$hydrophone == "P", ]
         if (table$jm[table$tow.id == substr(x$tow.id[i],3,10)] == "s") ee <- ee[ee$hydrophone == "S", ]
         e <- rbind(e, ee)
      }
   }
   
   # Truncate wing spread observations:
   e$doormaster[e$doormaster < 2.5] <- NA
   e$doormaster[e$doormaster >= 15] <- NA
   e$doormaster[e$doormaster >= 12 & e$time < 0] <- NA
   
   # Store reference times:
   res <- rbind(res, data.frame(tow          = i-1, 
                                tow.id       = x$tow.id[i], 
                                start.time   = start.time, 
                                stop.time    = stop.time,  
                                liftoff.time = liftoff.time))
   
   # Prepare vessel speed variables:
   e$tow <- i - 1
   e$speed <- 0.514444 * e$speed
   tmp <- e[(e$time >= -60) & e$time <= (liftoff.time + 60), c("time", "tow", "speed")]
   tmp <- aggregate(tmp["speed"], by = tmp[c("time", "tow")], mean)
   tmp$t <- tmp$time
   speed <- rbind(speed, tmp[c("t", "tow", "speed")])

   e <- e[!is.na(e$doormaster), ]
   e <- e[(e$time >= -60) & e$time <= (liftoff.time + 60), ]
   e <- e[order(e$time), ]
   e$time <- round(e$time)
   
   data$t <- c(data$t, e$time)
   data$width <- c(data$width, e$doormaster)
   data$tow <- c(data$tow, e$tow)
}
s <- speed

# Speed data matrices and active and passive trawling phase masks: 
speed <- matrix(NA, nrow = length(min(data$t):max(data$t)), ncol = nrow(x))
rownames(speed) <- min(data$t):max(data$t)
colnames(speed) <- 0:(nrow(x)-1)
active <- passive <- speed
for (i in 0:max(tmp$tow)){
   v <- s[s$tow == i, "speed"]
   t <- s[s$tow == i, "t"]

   ss <- approx(t, v, round(as.numeric(rownames(speed))))$y
   ss[is.na(ss)] <- v[match(round(as.numeric(rownames(speed))), t)][is.na(ss)]
   speed[,i+1] <- ss
   
   bounds <- c(res$start.time[res$tow == i], res$stop.time[res$tow == i], res$liftoff.time[res$tow == i])
   t <- round(as.numeric(rownames(active)))
   
   # Define active and passive trawling phase:
   active[(t >= bounds[1]) & (t <= bounds[2]), i+1] <- 1
   passive[(t > bounds[2]) & (t <= bounds[3]), i+1] <- 1
}
active[is.na(active)] <- 0
passive[is.na(passive)] <- 0

#image(active + passive)

# Round off data and speed matrices:
t <- round(as.numeric(rownames(speed)))
g <- round(t / thin)
speed.tmp <- active.tmp <- passive.tmp <- NULL
for (i in 1:ncol(speed)){
   speed.tmp <- cbind(speed.tmp, aggregate(speed[, i], by = list(t = g), mean, na.rm = TRUE)[, 2])
   active.tmp <- cbind(active.tmp, aggregate(active[, i], by = list(t = g), sum)[, 2])
   passive.tmp <- cbind(passive.tmp, aggregate(passive[, i], by = list(t = g), sum)[, 2])
}
speed <- speed.tmp
active <- active.tmp 
passive <- passive.tmp

dimnames(speed) <- list(t = aggregate(g, by = list(g), mean)[,1], tow = (1:ncol(speed))-1)
dimnames(active) <- dimnames(speed)
dimnames(passive) <- dimnames(speed)

# Fill-in missing speed values:
m <- apply(speed, 1, mean, na.rm = TRUE)
for (i in 1:nrow(speed)){
   if ((as.numeric(rownames(speed)[i])* thin) < 100){
      index <- which(is.na(speed[i,]))
      speed[i, index] <- as.numeric(m[i])
   }
}
speed[is.na(speed)] <- 0

# Round off data:
data$t <- round(data$t / thin)
data <- as.list(aggregate(data["width"], by = data[c("t", "tow")], mean, na.rm = TRUE))
data$comparative <- as.numeric(substr(x$tow.id, 2, 2) == "C")

# Truncate to stop.time:
if (reference == "stop.time"){
   v <- min(nrow(active), max(apply(active, 2, function(x) max(which(x>0)))) + round(buffer / thin))
   speed   <- speed[1:v, ]
   active  <- active[1:v, ]
   passive <- passive[1:v, ]
   
   index <- data$t %in% round(as.numeric(rownames(speed)))

   data$t <- data$t[index]
   data$tow <- data$tow[index]
   data$width <- data$width[index]
}

# Add data offset:
index <- data$t
data$t <- data$t - min(data$t)
data <- data[c("t", "width", "tow", "comparative")]

data$speed <- speed
data$active <- active
data$passive <- passive
   
# Add spatial coordinates:
tmp <- deg2km(longitude(x), latitude(x))
data$x <- tmp$x[1:352]
data$y <- tmp$y[1:352]
data$station <- match(substr(x$tow.id, 3, 5), substr(x$tow.id[1:352], 3, 5)) - 1

# Initialize parameters:
station_effect <- aggregate(data["width"], by = data["tow"], mean)
station_effect <- rbind(station_effect, data.frame(tow = setdiff(0:(nrow(x)-1), station_effect$tow), width = 7))
station_effect <- station_effect[order(station_effect$tow), ]

comparative_effect <- station_effect[data$comparative == 1, 2]
station_effect <- station_effect[data$comparative == 0, 2]

parameters <- list(alpha = mean(station_effect),
                   station_effect = station_effect - mean(station_effect),
                   log_sigma_process_station = log(sd(station_effect - mean(station_effect))/2),
                   log_sigma_obs_station = log(sd(station_effect - mean(station_effect))/2),                   
                   log_a_station = 0,
                   log_rho_station = 2,
                   comparative_effect = comparative_effect, 
                   mu_comparative = 0,
                   log_sigma_comparative = log(sd(station_effect - mean(station_effect))/3),
                   lambda_global = aggregate(data$width, by = data["t"], mean)[,2],
                   log_sigma_process_global = log(sd(aggregate(data$width, by = data["t"], mean)[,2])),                
                   lambda = matrix(0, nrow = max(data$t)+1, ncol = length(station_effect)),
                   log_sigma_process = log(sd(data$width) / 3),
                   log_a_t = 0,
                   log_rho_t = 1,
                   log_sigma_obs = log(sd(data$width) / 3),
                   log_sigma_outlier = log(sd(data$width) / 3),
                   logit_p_outlier = -2)
                                                   
# Refine initial values:
theta <- c(alpha                     =  7.9921450, 
           log_sigma_process_station = -0.4116883,   
           log_sigma_obs_station     = -0.8826444,   
           log_a_station             =  0.2872290,   
           log_rho_station           =  3.6355767,  
           log_sigma_process_global  = -0.3264302,  
           log_sigma_process         =  0.3120048, 
           log_sigma_obs             = -1.4795433, 
           log_sigma_outlier         =  0.5634776, 
           log_rho_t                 =  2.1359568, 
           logit_p_outlier           = -1.6098416, 
           log_a_t                   = -1.0274929) 
           
parameters <- update.parameters(parameters, theta)
           
# Compile model:
compile("Wingspread_hierarchical_comparative.cpp")
dyn.load(dynlib("Wingspread_hierarchical_comparative"))

# Create TMB object:
obj <- MakeADFun(data       = data,
                 parameters = parameters,
                 random     = c("station_effect", "lambda_global", "lambda", "comparative_effect"),
                 DLL        = "Wingspread_hierarchical_comparative")

# Fit model:
theta <- optim(obj$par, obj$fn, control = list(trace = 3, maxit = 400))$par 
#theta <- optim(theta, obj$fn, control = list(trace = 3, maxit = 400))$par 

obj$par <- theta

rep  <- sdreport(obj)
fixed <- summary(rep, "fixed")
random <- summary(rep, "random")

tmp <- rep

dbarplot(rep$value)
rep$sd


lambda <- random[rownames(random) == "lambda", 1]
dim(lambda) <- c(nrow(data$speed), length(data$x))

lambda_global <- random[rownames(random) == "lambda_global", 1]
dbarplot(lambda_global, width = 1)

station_effect <- random[rownames(random) == "station_effect", 1]
dbarplot(station_effect, width = 1)

comparative_effect <- random[rownames(random) == "comparative_effect", 1]
dbarplot(fixed["mu_comparative", 1] + comparative_effect, width = 1)

x$tow.effect <- as.numeric(c(station_effect, comparative_effect))

windows()
gulf.map(sea = TRUE)
index <- which(x$tow.effect >= 0)
points(longitude(x)[index], latitude(x)[index], pch = 21, bg = "red", cex = 2.5 * sqrt(x$tow.effect[index]))
index <- which(x$tow.effect < 0)
points(longitude(x)[index], latitude(x)[index], pch = 21, bg = "grey", cex = 2.5 * sqrt(-x$tow.effect[index]))

#p_outlier <- 1/(1+exp(-random[grep("logit_p_outlier", rownames(random)), 1]))
#dbarplot(p_outlier, width = 1)

swept.area <- as.numeric(obj$report()$swept_area)
 
x$swept.area <- as.numeric(obj$report()$swept_area)
x$mean.width <- as.numeric(obj$report()$mean_width)
 
windows()
gulf.map(sea = TRUE)
v <- x$swept.area - mean(x$swept.area, na.rm = TRUE)
index <- which(v >= 0)
points(longitude(x)[index], latitude(x)[index], pch = 21, bg = "red", cex = 0.08 * sqrt(v[index]))
index <- which(v < 0)
points(longitude(x)[index], latitude(x)[index], pch = 21, bg = "grey", cex = 0.08 * sqrt(-v[index]))

 
x$mean.width <- as.numeric(obj$report()$mean_width)
windows()
gulf.map(sea = TRUE)
v <- x$mean.width - mean(x$mean.width, na.rm = TRUE)
index <- which(v >= 0)
points(longitude(x)[index], latitude(x)[index], pch = 21, bg = "red", cex = 1.5 * sqrt(v[index]))
index <- which(v < 0)
points(longitude(x)[index], latitude(x)[index], pch = 21, bg = "grey", cex = 1.5 * sqrt(-v[index]))

mu <- matrix(NA, nrow = nrow(data$speed), ncol = length(data$station))
for (j in 1:length(data$x)){
   mu[,j] = fixed["alpha", 1] + 
            station_effect[j] + 
            exp(fixed["log_sigma_process_global", 1]) * lambda_global + 
            exp(fixed["log_sigma_process", 1]) * lambda[,data$station[j]+1]
}
for (j in (length(data$x)+1):length(data$station)){
   mu[,j] = fixed["alpha", 1] + 
            station_effect[data$station[j]+1] + 
            exp(fixed["log_sigma_process_global", 1]) * lambda_global + 
            exp(fixed["log_sigma_process", 1]) * lambda[,data$station[j]+1] + 
            comparative_effect[j-length(data$x)];   
}
dimnames(mu) <- dimnames(data$speed)   

rownames(mu) <- as.numeric(rownames(mu)) * thin
colnames(mu) <- x$tow.id
# save(mu, file = "Wingpsread profiles 2019.Rdata")

clg()
for (i in 351:392){
   windows()
   dbarplot(mu[,i], as.numeric(rownames(mu)) * thin, width = 1, border = NA, xlim = c(-160, 600), ylim = c(0, 15), yaxs = "i", xaxs = "i")
   lines(as.numeric(rownames(mu)) * thin, mu[,i], lwd = 2, col = "black")
   
   points((data$t[(data$tow == (i-1))] + min(as.numeric(rownames(mu)))) * thin, data$width[(data$tow == (i-1))], pch = 21, bg = "red")    
          
   lines(c(0, 0), par("usr")[3:4], col = "red", lwd = 2)
   lines(rep(res$stop.time[res$tow.id == x$tow.id[i]], 2), par("usr")[3:4], col = "red", lwd = 2)
   lines(rep(res$liftoff.time[res$tow.id == x$tow.id[i]], 2), par("usr")[3:4], col = "red", lwd = 2)   
   
   title(main = paste0(x$tow.id[i], "(", length(data$width[(data$tow == (i-1))]), "), Area = ", round(swept.area[i])))           
}

# 

         
                             
