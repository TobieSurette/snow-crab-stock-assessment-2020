library(gulf.data)
library(gulf.graphics)

x <- seq(0, 150000, len = 1000)
y <- TAC(x, species = "snow crab")
plot(range(x) / 1000, 100 * c(0, 0.5),
     type = "n", xlab = "", ylab = "", xaxs = "i", yaxs = "i")
grid()
lines(x / 1000, 100 * y, lwd = 2)
box()

mtext("Commercial biomass (x1000 t)", 1, 2.5, cex = 1.25)
mtext("Exploitation rate (%)", 2, 2.5, cex = 1.25)

Blim <- 9970   # Recovery biomass limit reference pont.
Busr <- 41371  # Upper stock reference biomass.
Flim <- 0.346  # Limit reference point for fishing removal rate.

vline(Blim / 1000, lwd = 2, col = "red")
vline(Busr / 1000, lwd = 2, col = "green")
hline(100 * Flim, lower = Busr / 1000, col = "blue", lwd = 2)

text(Blim / 1000 + 3, 25, expression('B'[lim]*' = 9970 t'), pos = 1, cex = 1.25, srt = 90)
text(Busr / 1000 + 3, 20, expression('B'[usr]*' = 41371 t'), pos = 1, cex = 1.25, srt = 90)
text(125, 100 * Flim, expression('F'[lim]*' = 34.6%'), pos = 3, cex = 1.25)
text(125, 100 * 0.45, expression('TE'[max]*' = 45%'), pos = 3, cex = 1.25)
