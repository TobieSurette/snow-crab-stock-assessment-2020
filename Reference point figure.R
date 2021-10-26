library(gulf.data)
library(gulf.graphics)

Blim <- 9970   # Recovery biomass limit reference pont.
Busr <- 41371  # Upper stock reference biomass.
Flim <- 0.346  # Limit reference point for fishing removal rate.

# Data from 1997-2020:
x <- data.frame(year = 1997:2020,
                landings = c(17.66, 13.86, 15.52, 19.18, 18.51, 26.18, 21.16, 31.66, 36.08, 29.12, 26.87, 24.46,
                             23.64, 9.549, 10.71, 21.96, 26.05, 24.44, 25.91, 21.71, 43.656, 24.260, 31.707, 28.156),
                com = c(64.5184, 64.5184, 57.8125, 56.7565, 50.621, 60.3283, 79.2275, 84.4475, 103.1457, 82.5652,
                        73.6453, 66.3714, 52.9209, 31.0153, 35.9294, 62.8407, 74.7775, 66.709, 67.9896, 58.9269,
                        98.3942, 65.7376, 80.746, 79.06550),
                com.sigma = c(5.6785, 5.6785, 6.6617, 4.9687, 4.8133, 5.7457, 6.0774, 5.8931, 5.699, 4.8234, 4.2417,
                              3.3922, 3.0653, 1.8656, 2.0665, 3.6529, 5.3264, 6.8484, 4.3836, 4.0608, 6.0042, 4.578,
                              5.302936, 5.364855))

# Commercial confidence intervals:
x$com.lci <- x$com - 1.96 * x$com.sigma
x$com.uci <- x$com + 1.96 * x$com.sigma

# Exploitation rate:
x$er.lci <- x$landings / x$com.uci
x$er <- x$landings  / x$com
x$er.uci <- x$landings / x$com.lci

x <- x[-1, ]

# Base plot:
plot(c(0, 120), c(0, 100), type ="n", xlab = "", ylab = "", xaxs = "i", yaxs = "i", cex.axis = 1.1)

# Confidence interval lines:
for (i in 1:nrow(x)){
   lines(c(x$com.lci[i], x$com.uci[i]), 100 * c(x$er[i], x$er[i]))
   lines(c(x$com[i], x$com[i]), 100 * c(x$er.lci[i], x$er.uci[i]))
}

# Reference points:
vline(Blim / 1000, lwd = 2.5, col = "red")
vline(Busr / 1000, lwd = 2.5, col = "green")
hline(100 * Flim, lower = Busr / 1000, col = "blue", lwd = 2.5)
text(Blim / 1000 + 3, 80, expression('B'[lim]*' = 9970 t'), pos = 1, cex = 1.25, srt = 90)
text(Busr / 1000 + 3, 80, expression('B'[usr]*' = 41371 t'), pos = 1, cex = 1.25, srt = 90)
text(108, 1 + 100 * Flim, expression('F'[lim]*' = 34.6%'), pos = 3, cex = 1.25)

# Plot points and annotations:
pos <- c(4, 4, 3, 2, 1, 4, 1, 4, 4, 4, 1, 2, 3, 4, 2, 1, 1, 2, 2, 1, 2, 1, 1)
points(x$com, 100 * x$er, pch = 21, cex = 1.25, bg = "grey")
for (i in 1:length(pos)){
   if (pos[i] == 1) delta <- c(1,1)
   if (pos[i] == 2) delta <- c(-1,1)
   if (pos[i] == 3) delta <- c(-1,-1)
   if (pos[i] == 4) delta <- c(1,-1)
   delta[1] <- 4 * delta[1]
   delta[2] <- 2.5 * delta[2]
   text(x$com[i] + delta[1], 100 * x$er[i] + delta[2], x$year[i], offset = 0)
}
grid()

# Axis labels:
mtext("Commercial biomass (x1000 t)", 1, 2.5, cex = 1.25)
mtext("Exploitation rate (%)", 2, 2.5, cex = 1.25)

box()
