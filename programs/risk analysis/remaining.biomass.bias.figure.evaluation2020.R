# Landings and biomass stats:
# Landings and residual biomass estimates have the same year, but total biomass is offset by one year
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

# Define input constants:
year <- 2016 # Year of survey residual biomass.
S    <- 0.7  # Year-to-year survival rate.
F    <- 0.0  # Additional fraction of landings mortality on residuals.

i <- data$year == year
landings <- 1000 * data$landings[i]
B   <- 1000 * data$MMGE95.mu[i]       # Total biomass in previous year.
BREM.mu <- 1000 * data$MMGE95SC345.mu[i]  # Total remaining biomass in year.
BREM.sigma <- 1000 * 1.96 * data$MMGE95SC345.sigma[i]

# Define estimation bias ranges:
bias.y1 <- seq(-0.4, 0.4, len = 500)
bias.y2 <- seq(-0.4, 0.4, len = 400)

# Initialize matrix of remaining biomasses to be predicted:
BREM <- matrix(NA, nrow = length(bias.y1), ncol = length(bias.y2))

# Calculate remaining biomass for each estimation bias value pair:
for (i in 1:length(bias.y1)){
   for (j in 1:length(bias.y2)){
      BREM[i,j] <- (1 + bias.y2[j]) * ((B / (1+bias.y1[i])) * S - (1+F) * landings)
   }
}

# Define lines which yield observed remaining biomass in 2020:
y20 = data.frame(mu = (BREM.mu / ((B / (1+bias.y1)) * S - (1+F) * landings)) - 1,
                 lci = ((BREM.mu-BREM.sigma) / ((B / (1+bias.y1)) * S - (1+F) * landings)) - 1,
                 uci = ((BREM.mu+BREM.sigma) / ((B / (1+bias.y1)) * S - (1+F) * landings)) - 1)

# Plot results:
filled.contour(bias.y1 * 100, bias.y2 * 100, BREM,
               plot.axes = {axis(1);
                            axis(2);
                            lines(100*bias.y1, 100*y20$mu, col = "blue", lwd = 2);
                            lines(100*bias.y1, 100*y20$lci, col = "blue", lwd = 1, lty = "dashed");
                            lines(100*bias.y1, 100*y20$uci, col = "blue", lwd = 1, lty = "dashed");
                            abline(0, 1, lty = "dashed", col = "red", lwd = 2);
                            grid();
                  },
               xlab = paste0(year-1, " total biomass bias (%)"), ylab = paste0(year, " residual biomass bias (%)"))


