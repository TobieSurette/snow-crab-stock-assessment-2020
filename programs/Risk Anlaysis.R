# Converted risk analysis program (from Analytica)

language <- language("en")

# Define constants and
Brecov <- 9970  # Recovery biomass.
Busr   <- 41371 # Upper stock reference biomass.
#ER <- 0.406    # Exploitation rate for this year.
n <- 50000      # Number of random samples.
TAC <- 31410
pbias <- 0.00   # Bias to add into analysis

# BMMGE95.2019.mu <- (1-pbias) * 79065.50       # Estimated commercial biomass
# BMMGE95.2019.sigma <- (1-pbias) * 5364.855
BMMGE95.2020.mu <- (1-pbias) * 77748.1       # Estimated commercial biomass
BMMGE95.2020.sigma <- (1-pbias) * 5397.4

BREC.2021.mu    <- 81250         # Projected recruitment R-1 from the Bayesian model.
BREC.2021.sigma <- 16020
ER <- TAC / BMMGE95.2020.mu
quota <- TAC

# Reparameterize log-normal distribution:
rlnorm <- function(n, mu, sigma){
   s <- sqrt(log(((sigma^2)/(mu^2)) + 1))
   xbar <- log(mu) - (s^2)/2
   return(stats::rlnorm(n, xbar, s))
}

#2018:
#  variable polygon     area mean.sample sd.sample n.sample     mean       sd  lowerCI  upperCI      mean.CV    mad.CV     sd.CV
#1      COM    gulf 57842.85    81030.25 115592.91      354 80671.86 5302.936 70777.08 91554.34 -0.020367050 1.0922650 1.7312318
#2  COMSC12    gulf 57842.85    58504.79  94899.41      354 59561.91 4338.845 51510.38 68508.41 -0.023559313 0.8783964 1.4660781
#3 COMSC345    gulf 57842.85    22525.47  48161.35      354 21437.56 2311.083 17265.03 26312.70 -0.005818429 0.3394293 0.7297618

#2019:
#  variable polygon     area mean.sample sd.sample n.sample     mean       sd  lowerCI  upperCI      mean.CV    mad.CV    sd.CV
#1      COM    gulf 57842.85    81007.09 112930.89      352 79065.50 5364.855 69071.53 90090.73 -0.008609671 1.0453785 1.740335
#2  COMSC12    gulf 57842.85    59982.09  99286.08      352 58995.30 4760.665 50214.73 68862.82 -0.015845260 0.8995601 1.579778
#3 COMSC345    gulf 57842.85    21025.01  39789.18      352 20290.99 1830.448 16939.94 24108.74 -0.000148860 0.3171853 0.549708

# Landings and biomass stats:
# Warning! Landings and residual biomass estimates have the same year, but total biomass is offset by one year
data <- data.frame(year = 1997:2020,
                   landings = c(17.66, 13.86, 15.52, 19.18, 18.51, 26.18, 21.16, 31.66, 36.08, 29.12, 26.87, 24.46, 23.64, 9.549, 10.71, 21.96, 26.05, 24.44, 25.91, 21.71, 43.656, 24.260, 31.707, 28.045),
                   MMGE95SC345.mu = c(27.6882, 28.2949, 31.1769, 9.9793, 17.6121, 13.0600, 26.9933, 21.2590, 23.4963, 19.6210, 26.8285, 20.9811, 10.4538, 15.4901, 33.6790,
                                      25.6145, 27.0918, 23.8632, 24.1063, 24.3094, 14.6504, 21.4315, 20.291, 19.1073),
                   MMGE95SC345.sigma = c(3.1779, 3.8521, 3.4005, 1.751, 2.1014, 1.2433, 2.6788, 2.1584, 2.5455, 1.5852, 1.937, 1.6179, 0.9669, 1.3447, 2.8553, 2.1802, 2.7868,
                                       1.9003, 2.078, 1.855, 1.3788, 2.3042, 1.830, 1.5582),
                   MMGE95.mu = c(64.5184, 64.5184, 57.8125, 56.7565, 50.621, 60.3283, 79.2275, 84.4475, 103.1457, 82.5652, 73.6453, 66.3714, 52.9209, 31.0153, 35.9294,
                                 62.8407, 74.7775, 66.709, 67.9896, 58.9269, 98.3942, 65.7376, 80.746, 79.06550),
                   MMGE95.sigma = c(5.6785, 5.6785, 6.6617, 4.9687, 4.8133, 5.7457, 6.0774, 5.8931, 5.699, 4.8234, 4.2417, 3.3922, 3.0653, 1.8656, 2.0665, 3.6529, 5.3264, 6.8484,
                                    4.3836, 4.0608, 6.0042, 4.578, 5.302936, 5.364855))

# Calculate survivorship rate:
MMGE95SC345 <- matrix(NA, nrow = n, ncol = nrow(data))
MMGE95 <- MMGE95SC345
survivorship <- MMGE95SC345
for (i in 1:n){
   if ((i %% 1000) == 0) print(i)
   for (j in 1:nrow(data)){
      MMGE95SC345[i,j] <- rlnorm(1, data$MMGE95SC345.mu[j], data$MMGE95SC345.sigma[j])
      MMGE95[i,j] <- rlnorm(1, data$MMGE95.mu[j], data$MMGE95.sigma[j])
      survivorship[i,j] <- (data$landings[j] + MMGE95SC345[i,j]) / MMGE95[i,j]
   }
}
colnames(MMGE95SC345)  <- data$year
colnames(MMGE95)       <- data$year
colnames(survivorship) <- data$year

# 5-year survivorship value:
S.sim <- apply(survivorship[, (ncol(survivorship)-4):ncol(survivorship)], 1, mean)
S <- mean(S.sim)

# Simulate biomass for 2020:
BMMGE95.2020 <- rnorm(n, BMMGE95.2020.mu, BMMGE95.2020.sigma)

# Define vector of catch options:

#catch <- c(seq(18, 80, by = 0.25) * 1000 , quota, 37058, 53859)
catch <- c(seq(18, 100, by = 0.25) * 1000 , quota, 44800, 72440)
catch <- c(catch[catch < quota], quota, catch[catch >= quota])

# Calculate remaining biomass for 2021:
BREM.2021 <- repvec(BMMGE95.2020 * S, ncol = length(catch)) - repvec(catch, nrow = n)

# Probability of remaining biomass being below Blim in 2020:
Plim <- apply(BREM.2021 < Brecov, 2, function(x) sum(x) / length(x))
names(Plim) <- catch
plot(catch, Plim)

# Probability of exceeding ER in 2019:
ER.2021 <- repvec(catch, nrow = n) / repvec(BMMGE95.2020, ncol = length(catch))  # Calculate simulated exploitation rates:
PER <- apply(ER.2021 > ER, 2, function(x) sum(x) / length(x))
names(PER) <- catch
plot(catch, PER)

# R-1 2021 mu and sigma:
BREC.2021 <- rnorm(n, BREC.2021.mu, BREC.2021.sigma)

# Probability of exceeding Busr in 2019:
Pusr <- apply((BREC.2021 + BREM.2021) < Busr, 2, function(x) sum(x) / length(x))
names(Pusr) <- catch
plot(catch, Pusr)

# Define summary table:
tab <- data.frame(catch = catch,
                  P.lim = Plim,
                  P.usr = Pusr,
                  P.ER = PER,
                  mu = apply(BREC.2021 + BREM.2021, 2, mean),
                  lci = apply(BREC.2021 + BREM.2021, 2, quantile, p = 0.025),
                  uci = apply(BREC.2021 + BREM.2021, 2, quantile, p = 0.975))

rownames(tab) <- NULL

ref.tab <- tab[(nrow(tab)-2):nrow(tab), ]
tab <- tab[1:(nrow(tab)-3), ]

# Probability of exceeding ER plot:
if (language == "english"){
   ylab <- "Probability"
   xlab <- "Catch options (x 1000t)"
   legend.str <- c("Brem < Blim", "B < Busr")
}
if (language == "french"){
   ylab <- "Probabilit?"
   xlab <- "Niveau de capture (x 1000t)"
   legend.str <- c("Bres < Blim", "B < Bnrs")
}
if (language == "bilingual"){
   ylab <- "Probability / Probabilit?"
   xlab <- "Catch options / Niveau de capture (x 1000t)"
   legend.str <- c("Brem/Bres < Blim", "B < Busr/Bnrs")
}
windows(width = 11, height = 7)
plot(tab$catch / 1000, tab$P.ER, xlab = xlab, ylab = ylab, cex.lab = 1.5, type = "n", lwd = 2, col = "red", yaxs = "i", ylim = c(0, 1.05), cex.axis = 1.25, las = 1)
grid()
#lines(tab$catch, tab$P.ER, xlab = xlab, ylab = ylab, cex.lab = 1.5, lwd = 3, col = "red")
lines(tab$catch / 1000, tab$P.lim, xlab = xlab, ylab = ylab, cex.lab = 1.5, lwd = 3, col = "black", lty = "solid")
lines(tab$catch / 1000, tab$P.usr, xlab = xlab, ylab = ylab, cex.lab = 1.5, lwd = 3, col = "blue", lty = "dashed")
legend("bottomright", legend = legend.str, bg = "white", lwd = 2, col = c("black", "blue"), cex = 1.5, lty = c("solid", "dashed"))
box()
axis(2, at = seq(0.1, 0.9, by = 0.2), cex.axis = 1.25, las = 1)
#lines(c(par("usr")[1], approx(tab$P.ER, tab$catch, 0.5)$y), c(0.5, 0.5), lwd = 2, col = "red", lty = "dashed")
#lines(rep(approx(tab$P.ER, tab$catch, 0.5)$y, 2), c(0, 0.5), lwd = 1.5, col = "red", lty = "solid")
#points(approx(tab$P.ER, tab$catch, 0.5)$y, 0.5, pch = 21, bg = "red", cex = 1.5)
#text(approx(tab$P.ER, tab$catch, 0.5)$y, 0.25, paste0("= ", round(quota), " t"), pos = 4, cex = 1.4)

#lines(c(approx(tab$P.ER, tab$catch, 0.5)$y, approx(tab$P.lim, tab$catch, 0.5)$y), c(0.5, 0.5), lwd = 2, col = "black", lty = "dashed")
lines(rep(approx(tab$P.lim, tab$catch, 0.5)$y, 2) / 1000, c(0, 0.5), lwd = 1.5, col = "black", lty = "solid")
points(approx(tab$P.lim, tab$catch, 0.5)$y / 1000, 0.5, pch = 21, bg = "black", cex = 1.5)
text(approx(tab$P.lim, tab$catch, 0.5)$y / 1000, 0.25, paste0("= ", round(approx(tab$P.lim, tab$catch, 0.5)$y), " t"), pos = 4, cex = 1.4)

#lines(c(approx(tab$P.lim, tab$catch, 0.5)$y, approx(tab$P.usr, tab$catch, 0.5)$y), c(0.5, 0.5), lwd = 2, col = "blue", lty = "dashed")
lines(rep(approx(tab$P.usr, tab$catch, 0.5)$y, 2) / 1000, c(0, 0.5), lwd = 1.5, col = "blue", lty = "dashed")
points(approx(tab$P.usr, tab$catch, 0.5)$y / 1000, 0.5, pch = 21, bg = "blue", cex = 1.5)
text(approx(tab$P.usr, tab$catch, 0.5)$y / 1000, 0.25, paste0("= ", round(approx(tab$P.usr, tab$catch, 0.5)$y), " t"), pos = 4, cex = 1.4)

# Build reduced table for export:
tab <- rbind(tab, ref.tab)
values <- sort(c(seq(20000, 50000, by = 1000), ref.tab$catch))
index <- match(values, tab$catch)
tab[index, ]
