# Define constants:
quota    <- 32100.59  # 2020 quota.
S        <- 0.70      # Year-to-year survival rate.
qbias    <- 0.1       # Fraction of un-caught quota.
B2019    <- 79065.50  # Total biomass in 2019.
BREM2020 <- 19107     # Total remaining biomass in 2020.

# Define estimation bias ranges:
bias19 <- seq(0, 0.4, len = 500)
bias20 <- seq(0, 0.4, len = 400)

# Initialize matrix of remaining biomasses to be predicted:
BREM <- matrix(NA, nrow = length(bias19), ncol = length(bias20))

# Calculate remaining biomass for each estimation bias value pair:
for (i in 1:length(bias19)){
   for (j in 1:length(bias20)){
      BREM[i,j] <- (1 + bias20[j]) * ((B2019 / (1+bias19[i])) * S - (1-qbias) * quota)
   }
}

# Define lines which yield observed remaining biomass in 2020:
y20 = data.frame(mu = (BREM2020 / ((B2019 / (1+bias19)) * S - (1-qbias) * quota)) - 1,
                 lci = ((BREM2020-3000) / ((B2019 / (1+bias19)) * S - (1-qbias) * quota)) - 1,
                 uci = ((BREM2020+3000) / ((B2019 / (1+bias19)) * S - (1-qbias) * quota)) - 1)

# Plot results:
filled.contour(bias19 * 100, bias20 * 100, BREM,
               plot.axes = {axis(1);
                            axis(2);
                            lines(100*bias19, 100*y20$mu, col = "blue", lwd = 2);
                            lines(100*bias19, 100*y20$lci, col = "blue", lwd = 1, lty = "dashed");
                            lines(100*bias19, 100*y20$uci, col = "blue", lwd = 1, lty = "dashed");
                            abline(0, 1, lty = "dashed", col = "red", lwd = 2);
                  },
               xlab = "2019 bias (%)", ylab = "2020 bias (%)")


