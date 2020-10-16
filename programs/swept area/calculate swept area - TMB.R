library(gulf.data)
library(TMB)

year <- 2020
thin <- 5

x <- read.scsset(year = year, valid = 1)

tows <- x$tow.id
data <- list()
data$t <- NULL
data$width <- NULL
data$tow <- NULL
data$comparative <- NULL
speed <- data
speed$speed <- NULL
res <- NULL
for (i in 1:30){#nrow(x)){
   print(x$tow.id[i])

   e <- read.esonar(x[i, ])
   e$time <- time2sec(time(e), time(x[i,], "touchdown"))

   start.time <- 0
   stop.time <- time2sec(time(x[i,], "stop"), time(x[i,], "touchdown"))

   # Store reference times:
   res <- rbind(res, data.frame(tow = i-1,
                                tow.id = x$tow.id[i],
                                start.time = start.time,
                                stop.time = stop.time))

   # Prepare vessel speed variables:
   e$tow <- i - 1
   tmp <- e[(e$time >= -60) & e$time <= (stop.time + 60), c("time", "tow", "speed")]
   tmp <- aggregate(tmp["speed"], by = tmp[c("time", "tow")], mean)
   tmp$t <- tmp$time
   speed <- rbind(speed, tmp[c("t", "tow", "speed")])

   e <- e[!is.na(e$doormaster), ]
   e <- e[(e$time >= -60) & e$time <= (stop.time + 60), ]
   e <- e[order(e$time), ]
   e$time <- round(e$time)

   data$t <- c(data$t, e$time)
   data$width <- c(data$width, wingspread(e))
   data$tow <- c(data$tow, e$tow)
}

# Round off data and speed matrices:
t <- round(as.numeric(rownames(speed)))
g <- round(t / thin)
tmp <- NULL
for (i in 1:ncol(speed)) tmp <- cbind(tmp, aggregate(speed[, i], by = list(t = g), mean, na.rm = TRUE)[, 2])
speed <- tmp
dimnames(speed) <- list(t = aggregate(g, by = list(g), mean)[,1], tow = (1:ncol(speed))-1)
speed[is.na(speed)] <- 0

# Round off data:
data$t <- round(data$t / thin)
data <- aggregate(data["width"], by = data[c("t", "tow")], mean, na.rm = TRUE)

# Add data offset:
data$t <- data$t - min(data$t)

data <- data[c("t", "width", "tow")]
data <- as.list(data)
data$speed <- speed

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
obj <- MakeADFun(data       = data[c("t", "width", "tow")],
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


