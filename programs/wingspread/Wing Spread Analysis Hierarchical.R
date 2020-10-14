library(gulf)
library(TMB)
setwd("U:/Snow Crab/Stock Assessment 2019")
 
year <- 2019
thin <- 5

source("C:/gulf package/gulf/R/liftoff.R")
source("C:/gulf package/gulf/R/liftoff.scset.R")

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

x <- read.scset(year = year, valid = 1)
times <- read.csv(file = "U:/Snow Crab/Stock Assessment 2019/Touchdown and Liftoff Times 2019.csv", header = TRUE, stringsAsFactors = FALSE)

tmp <- x$start.time
x$start.time <- x$start.time.logbook 
x$end.time <- x$end.time.logbook
x$liftoff.time <- "        "
index <- match(x$tow.id, times$tow.id)
x$liftoff.time <- substr(start.time(x) + times$liftoff[index] * 60, 12, 20)
x$start.time <- tmp
        
vessel <- "av"
tows <- unique(substr(x$tow.id[substr(x$tow.id, 2, 2) == "C"], 3, 10)) 
data <- NULL
data$t <- 
data$width <- NULL
data$tow <- NULL
data$comparative <- NULL
speed <- data
speed$speed <- NULL
res <- NULL
for (i in 1:length(tows)){
   for (vessel in c("av", "jm")){
      print(tows[i])
      if (vessel == "av"){
         tow.id <- paste0("GP", tows[i])
         e <- read.esonar(year = year, tow.id = tow.id)
         if (table$av[table$tow.id == tows[i]] == "p") e <- e[e$hydrophone == "P", ]
         if (table$av[table$tow.id == tows[i]] == "s") e <- e[e$hydrophone == "S", ]
         e$time <- time2sec(time(e), start.time(x[x$tow.id == tow.id, ]))
         start.time <- 0
         stop.time <- time2sec(end.time(x[x$tow.id == tow.id, ]), start.time(x[x$tow.id == tow.id, ]))
         liftoff.time <- time2sec(liftoff(x[x$tow.id == tow.id, ]), start.time(x[x$tow.id == tow.id, ]))
      }
      if (vessel == "jm"){   
         tow.id <- paste0("GC", tows[i])
         e <- read.esonar(year = year, tow.id = tow.id)
         e$time <- time2sec(time(e), start.time(x[x$tow.id == tow.id, ]))
         start.time <- 0
         stop.time <- time2sec(end.time(x[x$tow.id == tow.id, ]), start.time(x[x$tow.id == tow.id, ]))
         liftoff.time <- time2sec(liftoff(x[x$tow.id == tow.id, ]), start.time(x[x$tow.id == tow.id, ]))
   
         if (table$jm[table$tow.id == tows[i]] %in% c("s", "p")){
            ee <- read.esonar(year = year, tow.id = tow.id)
            ee$time <- time2sec(time(ee), start.time(x[x$tow.id == tow.id, ]))
      
            if (table$jm[table$tow.id == tows[i]] == "p") ee <- ee[ee$hydrophone == "P", ]
            if (table$jm[table$tow.id == tows[i]] == "s") ee <- ee[ee$hydrophone == "S", ]
            e <- rbind(e, ee)
         }
      }
   
      # Store reference times:
      res <- rbind(res, data.frame(tow = i-1, 
                                   tow.id = tow.id, 
                                   start.time = start.time, 
                                   stop.time = stop.time,  
                                   liftoff.time = liftoff.time))
   
      # Prepare vessel speed variables:
      e$tow <- i - 1
      e$comparative <- as.numeric(substr(e$tow.id, 2, 2) == "C")
      tmp <- e[(e$time >= -60) & e$time <= (liftoff.time + 60), c("time", "tow", "speed", "comparative")]
      tmp <- aggregate(tmp["speed"], by = tmp[c("time", "tow", "comparative")], mean)
      tmp$t <- tmp$time
      speed <- rbind(speed, tmp[c("t", "tow", "speed", "comparative")])

      e <- e[!is.na(e$doormaster), ]
      e <- e[(e$time >= -60) & e$time <= (liftoff.time + 60), ]
      e <- e[order(e$time), ]
      e$time <- round(e$time)
   
      data$t <- c(data$t, e$time)
      data$width <- c(data$width, e$doormaster)
      data$tow <- c(data$tow, e$tow)
      data$comparative <- c(data$comparative, e$comparative)
   }
}

s0 <- speed[speed$comparative == 0, ]
s1 <- speed[speed$comparative == 1, ]

r0 <- res[substr(res$tow.id, 2, 2) == "P", ]
r1 <- res[substr(res$tow.id, 2, 2) == "C", ]

# Speed data matrices and active and passive trawling phase masks: 
speed <- matrix(NA, nrow = length(min(data$t):max(data$t)), ncol = max(tmp$tow)+1)
rownames(speed) <- min(data$t):max(data$t)
colnames(speed) <- 0:max(tmp$tow)
active <- passive <- speed
active_c <- passive_c <- speed_c <- speed
for (i in 0:max(tmp$tow)){
   v <- s0[s0$tow == i, "speed"]
   t <- s0[s0$tow == i, "t"]

   ss <- approx(t, v, round(as.numeric(rownames(speed))))$y
   ss[is.na(ss)] <- v[match(round(as.numeric(rownames(speed))), t)][is.na(ss)]
   speed[,i+1] <- ss
   
   bounds <- c(r0$start.time[r0$tow == i], r0$stop.time[r0$tow == i], r0$liftoff.time[r0$tow == i])
   t <- round(as.numeric(rownames(active)))
   
   # Define active and passive trawling phase:
   active[(t >= bounds[1]) & (t <= bounds[2]), i+1] <- 1
   passive[(t > bounds[2]) & (t <= bounds[3]), i+1] <- 1
   
   v <- s1[s1$tow == i, "speed"]
   t <- s1[s1$tow == i, "t"]

   ss <- approx(t, v, round(as.numeric(rownames(speed))))$y
   ss[is.na(ss)] <- v[match(round(as.numeric(rownames(speed))), t)][is.na(ss)]
   speed_c[,i+1] <- ss
   
   bounds <- c(r1$start.time[r1$tow == i], r1$stop.time[r1$tow == i], r1$liftoff.time[r1$tow == i])
   t <- round(as.numeric(rownames(active_c)))
   
   # Define active and passive trawling phase:
   active_c[(t >= bounds[1]) & (t <= bounds[2]), i+1] <- 1
   passive_c[(t > bounds[2]) & (t <= bounds[3]), i+1] <- 1
   
}
active[is.na(active)] <- 0
passive[is.na(passive)] <- 0
active_c[is.na(active_c)] <- 0
passive_c[is.na(passive_c)] <- 0

speed_c[is.na(speed_c[,1]), 1] <- speed_c[is.na(speed_c[,1]), 2]

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

speed.tmp <- active.tmp <- passive.tmp <- NULL
for (i in 1:ncol(speed)){
   speed.tmp <- cbind(speed.tmp, aggregate(speed_c[, i], by = list(t = g), mean, na.rm = TRUE)[, 2])
   active.tmp <- cbind(active.tmp, aggregate(active_c[, i], by = list(t = g), sum)[, 2])
   passive.tmp <- cbind(passive.tmp, aggregate(passive_c[, i], by = list(t = g), sum)[, 2])
}
speed_c <- speed.tmp
active_c <- active.tmp 
passive_c <- passive.tmp

dimnames(speed) <- list(t = aggregate(g, by = list(g), mean)[,1], tow = (1:ncol(speed))-1)
dimnames(active) <- dimnames(speed)
dimnames(passive) <- dimnames(speed)
speed[is.na(speed)] <- 0

dimnames(speed_c) <- dimnames(speed)
dimnames(active_c) <- dimnames(speed)
dimnames(passive_c) <- dimnames(speed)

# Round off data:
data$t <- round(data$t / thin)
data <- aggregate(data["width"], by = data[c("t", "tow", "comparative")], mean, na.rm = TRUE)

# Add data offset:
data$t <- data$t - min(data$t)

data <- data[c("t", "width", "tow", "comparative")]

data <- as.list(data)
data$speed <- speed
data$active <- active
data$passive <- passive
data$speed_c <- speed_c
data$active_c <- active_c
data$passive_c <- passive_c

# Initialize parameters:
parameters <- list(alpha = mean(aggregate(data$width, by = data["tow"], mean)[,2]),
                   tow_effect = aggregate(data$width, by = data["tow"], mean)[,2] - mean(data$width),
                   log_sigma_tow = log(sd(aggregate(data$width, by = data["tow"], mean)[,2])),
                   log_sigma_process = log(sd(data$width) / 3),
                   log_sigma_obs = log(sd(data$width) / 3),
                   log_sigma_outlier = log(sd(data$width) / 3),
                   log_rho = 0,           
                   mu_logit_outlier = -2, 
                   log_sigma_logit_outlier = -2,
                   logit_p_outlier = rep(-2, length(unique(data$tow))),
                   lambda = matrix(0, nrow = max(data$t)+1, ncol = length(unique(data$tow))),
                   lambda_c = matrix(0, nrow = max(data$t)+1, ncol = length(unique(data$tow))),
                   log_a = 0)
 
parameters$alpha_c <- 0
parameters$log_sigma_tow_c <- -1
parameters$tow_effect_c <- aggregate(data$width, by = data["tow"], mean)[,2] - mean(data$width) / 3 
parameters$log_sigma_process_c = log(sd(data$width) / 5)
                   
                                   
compile("Wingspread_hierarchical_comparative.cpp")
dyn.load(dynlib("Wingspread_hierarchical_comparative"))
  
# Create TMB object:
obj <- MakeADFun(data       = data[c("t", "width", "tow", "comparative")],
                 parameters = parameters,
                 random     = c("tow_effect", "logit_p_outlier", "lambda", "lambda_c", "tow_effect_c"),
                 DLL        = "Wingspread_hierarchical_comparative")

# Fit model:
theta <- optim(obj$par, obj$fn, control = list(trace = 3, maxit = 400))$par 
#theta <- optim(theta, obj$fn, control = list(trace = 3, maxit = 400))$par 

obj$par <- theta

rep  <- sdreport(obj)
fixed <- summary(rep, "fixed")
random <- summary(rep, "random")

lambda <- random[rownames(random) == "lambda", 1]
dim(lambda) <- dim(data$speed)
lambda_c <- random[rownames(random) == "lambda_c", 1]
dim(lambda_c) <- dim(data$speed)

tow_effect <- random[rownames(random) == "tow_effect", 1]
dbarplot(tow_effect, width = 1)
tow_effect_c <- random[rownames(random) == "tow_effect_c", 1]
dbarplot(tow_effect_c, width = 1)

p_outlier <- 1/(1+exp(-random[grep("logit_p_outlier", rownames(random)), 1]))
dbarplot(p_outlier, width = 1)

clg()
for (i in 1:20){
   mu <- fixed["alpha", 1] + tow_effect[i] + exp(fixed["log_sigma_process", 1]) * lambda[, i]
   mu_c <- mu + fixed["alpha_c", 1] + tow_effect_c[i] + exp(fixed["log_sigma_process_c", 1]) * lambda_c[, i]
  
   windows()
   dbarplot(mu, width = 1, border = NA, ylim = c(0, 12))
   points(data$t[(data$tow == (i-1)) & (data$comparative == 0)], 
          data$width[(data$tow == (i-1)) & (data$comparative == 0)], pch = 21, bg = "grey")
   lines(1:length(mu), mu, lwd = 2, col = "black")
   lines(1:length(mu), mu_c, lwd = 2, col = "red")
   points(data$t[(data$tow == (i-1)) & (data$comparative == 1)], 
          data$width[(data$tow == (i-1)) & (data$comparative == 1)], pch = 21, bg = "red")          
}
 
theta <- c(alpha = 7.1734622,
           log_sigma_tow = -0.1990291, 
           log_sigma_process = 0.5300337,
           log_sigma_obs = -1.3120309  
           log_sigma_outlier = 1.7054412,
           log_rho = 2.0557487, 
           mu_logit_outlier = -2.0354599,  
           log_sigma_logit_outlier = -0.2247754,    
           log_a = -0.9736141)  
              
           alpha 6.886347495    
           log_sigma_tow -0.950748456 
           log_sigma_process  0.436784586  
           log_sigma_obs   -1.052550875  
           log_sigma_outlier  1.588815929  
           log_rho  2.101084462     
           mu_logit_outlier -1.656305959  
           log_sigma_logit_outlier -2.737243195     
           log_a  -0.664533434       
           alpha_c  0.009173774                                 
           log_sigma_tow_c   -0.880440319   
           log_sigma_process_c  0.444171526
              
                             
                   