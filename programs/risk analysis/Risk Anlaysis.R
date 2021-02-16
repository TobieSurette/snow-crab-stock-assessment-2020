library(gulf.data)

# Define output language:
language <- language("en")

# Define constants:
Brecov <- 9970   # Recovery biomass limit reference pont.
Busr   <- 41371  # Upper stock reference biomass.
TAC    <- 31410  # Total allowable catch for sGSL.
n      <- 100000 # Number of random samples.
#ER     <- 0.4

# Bias specifications:
range <- FALSE      # Whether to treat bias.scs as a uniform range of [0, bias.scs]
bias.scs  <- 0.10   # Negative bias applied to snow crab survey from 2019 and 2020 estimates.
bias.rec  <- 0.30   # Negative bias applied to recruitment predictions.
if (range) bias.scs  <- bias.scs * runif(n) # Simulate bias for snow crab survey.

# Define biomass estimates:
BMMGE95.2020.mu    <- 77748.1  # Estimated kriged commercial biomass.
BMMGE95.2020.sigma <- 5397.4
BREC.2021.mu       <- 79870    # Projected R-1 recruitment from the Bayesian projection model.
BREC.2021.sigma    <- 15980

# Define function for simulating from a log-normal distribution with specified mean and error:
rlnorm <- function(n, mu, sigma){
   s <- sqrt(log(((sigma^2)/(mu^2)) + 1))
   xbar <- log(mu) - (s^2)/2
   return(stats::rlnorm(n, xbar, s))
}

# Landings and biomass stats:
# Warning! Landings and residual biomass estimates have the same year, but total biomass is offset by one year
data <- data.frame(year = 1997:2020,
                   landings = c(17.66, 13.86, 15.52, 19.18, 18.51, 26.18, 21.16, 31.66, 36.08, 29.12, 26.87, 24.46, 23.64, 9.549, 10.71, 21.96, 26.05, 24.44, 25.91, 21.71, 43.656, 24.260, 31.707, 28.156),
                   MMGE95SC345.mu = c(27.6882, 28.2949, 31.1769, 9.9793, 17.6121, 13.0600, 26.9933, 21.2590, 23.4963, 19.6210, 26.8285, 20.9811, 10.4538, 15.4901, 33.6790,
                                      25.6145, 27.0918, 23.8632, 24.1063, 24.3094, 14.6504, 21.4315, 20.291, 19.1073),
                   MMGE95SC345.sigma = c(3.1779, 3.8521, 3.4005, 1.751, 2.1014, 1.2433, 2.6788, 2.1584, 2.5455, 1.5852, 1.937, 1.6179, 0.9669, 1.3447, 2.8553, 2.1802, 2.7868,
                                         1.9003, 2.078, 1.855, 1.3788, 2.3042, 1.830, 1.5582),
                   MMGE95.mu = c(64.5184, 64.5184, 57.8125, 56.7565, 50.621, 60.3283, 79.2275, 84.4475, 103.1457, 82.5652, 73.6453, 66.3714, 52.9209, 31.0153, 35.9294,
                                 62.8407, 74.7775, 66.709, 67.9896, 58.9269, 98.3942, 65.7376, 80.746, 79.06550),
                   MMGE95.sigma = c(5.6785, 5.6785, 6.6617, 4.9687, 4.8133, 5.7457, 6.0774, 5.8931, 5.699, 4.8234, 4.2417, 3.3922, 3.0653, 1.8656, 2.0665, 3.6529, 5.3264, 6.8484,
                                    4.3836, 4.0608, 6.0042, 4.578, 5.302936, 5.364855))

# Initialize variables:
MMGE95SC345 <- matrix(NA, nrow = n, ncol = nrow(data))
colnames(MMGE95SC345)  <- data$year
MMGE95 <- MMGE95SC345
mortality <- MMGE95SC345

# Loop over years:
for (j in 1:nrow(data)){
   # Simulate biomass:
   if (data$year[j] %in% c(2019, 2020)) scale <- (1 - bias.scs) else scale <- 1                    # Define bias by year.
   MMGE95SC345[,j] <- rlnorm(n, scale * data$MMGE95SC345.mu[j], scale * data$MMGE95SC345.sigma[j]) # Residual biomass.
   if (data$year[j] %in% c(2020)) scale <- (1 - bias.scs) else scale <- 1                          # Define bias by year.
   MMGE95[,j]      <- rlnorm(n, scale * data$MMGE95.mu[j], scale * data$MMGE95.sigma[j])           # Total biomass.

   # Calculate mortality:
   mortality[,j] <- 1 - ((data$landings[j] + MMGE95SC345[,j]) / MMGE95[,j])
}
BMMGE95.2020 <- rlnorm(n, (1 - bias.scs) * BMMGE95.2020.mu, (1 - bias.scs) * BMMGE95.2020.sigma) # Simulate biomass for current year.
BREC.2021    <- rlnorm(n, (1 - bias.rec) * BREC.2021.mu, (1 - bias.rec) * BREC.2021.sigma)       # Projected R-1 mu and sigma.

# Average 5-year survival value:
S <- mean(apply(1-mortality[, (ncol(mortality)-4):ncol(mortality)], 1, mean))

# Define vector of catch options:
catch <- c(seq(0, 100, by = 0.25) * 1000 , quota)
catch <- c(catch[catch < quota], quota, catch[catch > quota])

# Calculate remaining biomass for 2021:
BREM.2021 <- repvec(BMMGE95.2020 * S, ncol = length(catch)) - repvec(catch, nrow = n)

# Projected probability of remaining biomass being below Blim:
Plim <- apply(BREM.2021 < Brecov, 2, function(x) sum(x) / length(x))

# Projected probability of total biomass being below Busr:
Pusr <- apply((repvec(BREC.2021, ncol = length(catch)) + BREM.2021) < Busr, 2, function(x) sum(x) / length(x))

# Probability of exceeding ER in 2021:
#ER.2021 <- repvec(catch, nrow = n) / repvec(BMMGE95.2020, ncol = length(catch))  # Calculate simulated exploitation rates:
#PER <- apply(ER.2021 > ER, 2, function(x) sum(x) / length(x))

# Define summary table:
tab <- data.frame(catch = catch,
                  P.lim = Plim,
                  P.usr = Pusr,
                  mu    = apply(repvec(BREC.2021, ncol = length(catch)) + BREM.2021, 2, mean),
                  lci   = apply(repvec(BREC.2021, ncol = length(catch)) + BREM.2021, 2, quantile, p = 0.025),
                  uci   = apply(repvec(BREC.2021, ncol = length(catch)) + BREM.2021, 2, quantile, p = 0.975))

# Probability of exceeding ER plot:
if (language == "english"){
   ylab <- "Probability"
   xlab <- "Catch options (x 1000t)"
   legend.str <- c("Brem < Blim", "B < Busr")
}
if (language == "french"){
   ylab <- "Probabilité"
   xlab <- "Niveau de capture (x 1000t)"
   legend.str <- c("Bres < Blim", "B < Bnrs")
}
if (language == "bilingual"){
   ylab <- "Probability / Probabilit?"
   xlab <- "Catch options / Niveau de capture (x 1000t)"
   legend.str <- c("Brem/Bres < Blim", "B < Busr/Bnrs")
}

clg()
dev.new(width = 11, height = 7)
plot(tab$catch / 1000, tab$P.lim, xlab = xlab, ylab = ylab, cex.lab = 1.5, type = "n", lwd = 2, col = "red", yaxs = "i", ylim = c(0, 1.05), cex.axis = 1.25, las = 1)
grid()
lines(tab$catch / 1000, tab$P.usr, xlab = xlab, ylab = ylab, cex.lab = 1.5, lwd = 3, col = "blue", lty = "dashed")
legend("bottomright", legend = legend.str, bg = "white", lwd = 2, col = c("black", "blue"), cex = 1.5, lty = c("solid", "dashed"))
box()
axis(2, at = seq(0.1, 0.9, by = 0.2), cex.axis = 1.25, las = 1)
lines(rep(approx(tab$P.lim, tab$catch, 0.5)$y, 2) / 1000, c(0, 0.5), lwd = 1.5, col = "black", lty = "solid")

points(approx(tab$P.lim, tab$catch, 0.5)$y / 1000, 0.5, pch = 21, bg = "black", cex = 1.5)
text(approx(tab$P.lim, tab$catch, 0.5)$y / 1000, 0.25, paste0("= ", round(approx(tab$P.lim, tab$catch, 0.5)$y), " t"), pos = 4, cex = 1.4)
lines(rep(approx(tab$P.usr, tab$catch, 0.5)$y, 2) / 1000, c(0, 0.5), lwd = 1.5, col = "blue", lty = "dashed")
points(approx(tab$P.usr, tab$catch, 0.5)$y / 1000, 0.5, pch = 21, bg = "blue", cex = 1.5)
text(approx(tab$P.usr, tab$catch, 0.5)$y / 1000, 0.25, paste0("= ", round(approx(tab$P.usr, tab$catch, 0.5)$y), " t"), pos = 4, cex = 1.4)
