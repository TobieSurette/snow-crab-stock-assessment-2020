#source("U:/Trawl Analyses/PLM Functions/PLM Kernel Functions.R")

graphics.off()

windows(width = 8.5, height = 11)
m <- kronecker(matrix(1:3, ncol = 1), matrix(1, nrow = 4, ncol = 4))
m <- rbind(0, cbind(0, m, 0), 0, 0)
layout(m)
par(mar = c(0, 0, 0, 0))

# Transition kernel with fixed domain:
k <- 7 # Number of curves (odd number only)
plot(c(-1.1, 1.1), c(0, 1.1), type = "n", xaxs = "i", yaxs = "i", xlab = "", ylab = "", xaxt = "n")
grid()
x <- seq(-1.1, 1.1, len = 10000)
s <- seq(-3, 3, len = k)
tmp <- colorRamp(c("red", "white", "blue"))(seq(0, 1, len = length(s))) / 255
col <- NULL
for (i in 1:nrow(tmp)) col[i] <- rgb(tmp[i,1], tmp[i,2], tmp[i,3])
col[((length(s)-1)/2)+1] <- "black"
for (i in 1:length(s)){
   y <- transition(x, range = c(-1, 1), s = s[i], deg = 3)
   lines(x, y, lwd = 2, lty = "solid", col = col[i])
   text(x[which.max(y)], 1.05*y[which.max(y)], paste0("s = ", s[i]), cex = 1.0)
}
box()
mtext("Acceleration", 2, 2.5, cex = 1.5)

# Sigmoid kernel with fixed domain:
plot(c(-1.1, 1.1), c(0, 1.1), type = "n", xaxs = "i", yaxs = "i", xlab = "", ylab = "", xaxt = "n")
grid()
x <- seq(-1.1, 1.1, len = 10000)
for (i in 1:length(s)){
   y <- sigmoid(x, range = c(-1, 1), s = s[i], deg = 4)
   lines(x, y, lwd = 2, lty = "solid", col = col[i])
}
box()
mtext("Speed", 2, 2.5, cex = 1.5)
legend("topleft", legend = paste("s = ", s), lwd = 2, col = col)

# Sigmoid kernel with fixed domain:
plot(c(-1.1, 1.1), c(0, 1.8), type = "n", xaxs = "i", yaxs = "i", xlab = "", ylab = "", xaxt = "n")
grid()
x <- seq(-1.1, 1.1, len = 10000)
for (i in 1:length(s)){
   y <- plm(x, range = c(-1, 1), s = s[i], deg = 5)
   lines(x, y, lwd = 2, lty = "solid", col = col[i])
}
box()
mtext("Position", 2, 2.5, cex = 1.5)
legend("topleft", legend = paste("s = ", s), lwd = 2, col = col)
mtext("x", 1, 2.5, cex = 1.5)
axis(1)


# Graph basis functions:
skewness <- 0
windows(width = 8.5, height = 11)
m <- kronecker(matrix(1:15, ncol = 3), matrix(1, nrow = 4, ncol = 4))
m <- rbind(0, cbind(0, m, 0), 0, 0)
layout(m)

par(mar = c(0, 0, 0, 0))
p <- 1 / (1+exp(-skewness))
xlim <- c(-1.1*2*(1-p), 1.1*2*p)
x <- seq(xlim[1], xlim[2], len = 1000)
for (i in 1:5){
   plot(range(x), c(0, 1), type = "n", xlab = "", ylab = "",
        xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n",
        xlim = xlim, ylim = c(0, 1.2))
   grid()
   lines(x, transition(x, degree = i-2, skewness = skewness), lwd = 2, col = "blue")
   if (i > 1) lines(c(0, 0), par("usr")[1:2], col = "red", lty = "dashed")
   if (i == 1) lines(c(0,0), c(0, 1), lwd = 2, col = "blue")
   text(-0.8, 1, paste0("deg(f) = ", i-2), font = 2)
   box()
   if (i == 1) mtext("Kernel", 3, 1.0, cex = 1.25)
   if (i %in% 1:5) axis(2, at = seq(0, 1, by = 0.2), cex.axis = 0.8)
   if (i == 5) axis(1, at = round(seq(-1., 1., by = 0.25)*100)/100, cex.axis = 0.8, padj = -0.9)
   if (i == 3) mtext("Value", 2, 2.5, cex = 1.5)
   if (i == 2)   text(0, 0.65, bquote(f(x) == frac(1,2)))
   if (i == 3){  text(-0.7, 0.65, bquote(f(x) == 1 + x));  text(0.70, 0.65, bquote(f(x) == 1 - x)) }
   if (i == 4)   text(0.0, 0.9, bquote(f(x) == frac(3 * (1 - x^2),4)))
   if (i == 5) { text(-0.7, 0.55, bquote(f(x) == -2 * x ^ 3 - 3 * x ^ 2 + 1));  text(0.70, 0.55, bquote(f(x) == 2 * x ^ 3 - 3 * x ^ 2 + 1)) }
}
for (i in 1:5){
   plot(range(x), c(0, 1), type = "n", xlab = "", ylab = "",
        xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n",
        xlim = xlim, ylim = c(0, 1.2))
   grid()
   lines(x, sigmoid(x, degree = i-1, skewness = skewness), lwd = 2, col = "blue")
   text(-0.8, 1, paste0("deg(f) = ", i-1), font = 2)
   if (i > 1) lines(c(0, 0), par("usr")[1:2], col = "red", lty = "dashed")
   box()
   if (i == 1) mtext("Sigmoid", 3, 1.0, cex = 1.25)
   if (i == 5) axis(1, at = round(seq(-1., 1., by = 0.25)*100)/100, cex.axis = 0.8, padj = -0.9)

   if (i == 1){ text(-0.7, 0.1, bquote(f(x) == 0));  text(0.70, 1.1, bquote(f(x) == 1)) }
   if (i == 2){ text(-0.1, 0.60, bquote(f(x) == frac(x+1, 2))) }
   if (i == 3){ text(-0.6, 0.65, bquote(f(x) == frac((x+1)^2,2)));
                text(0.65, 0.50, bquote(f(x) == - frac((x-1)^2, 2) + 1)) }
   if (i == 4){ text(-0.5, 0.65, bquote(f(x) == frac(-x^3 + 3 * x + 2, 4))) }
   if (i == 5){ text(-0.65, 0.65, bquote(f(x) == frac(-x^4 - 2 * x ^ 3 + 2 * x + 1,2)), cex = 0.8);
                text(0.65, 0.65, bquote(f(x) == frac(x^4 - 2 * x ^ 3 + 2 * x + 1,2)), cex = 0.8) }
}
mtext("x", 1, 3.0, cex = 1.5)
for (i in 1:5){
   plot(range(x), c(0, 1), type = "n", xlab = "", ylab = "",
        xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n",
        xlim = xlim, ylim = c(0, 1.2))
   grid()
   lines(x, plm(x, degree = i, skewness = skewness), lwd = 2, col = "blue")
   text(-0.8, 1, paste0("deg(f) = ", i), font = 2)
   if (i > 1) lines(c(0, 0), par("usr")[1:2], col = "red", lty = "dashed")
   box()
   if (i == 1) mtext("PLM", 3, 1.0, cex = 1.25)
   if (i == 5) axis(1, at = round(seq(-1.0, 1.0, by = 0.25)*100)/100, cex.axis = 0.8, padj = -0.9)

   if (i == 1){ text(-0.7, 0.1,   bquote(f(x) == 0));  text(0.45, 0.75, bquote(f(x) == x)) }
   if (i == 2){ text(-0.1, 0.5,   bquote(f(x) == frac((x + 1)^2, 4))) }
   if (i == 3){ text(-0.5, 0.25,  bquote(f(x) == frac((1 + x)^3, 6))); text(0.5, 0.80, bquote(f(x) == frac((1 - x)^3, 6) + x)) }
   if (i == 4){ text(-0.25, 0.55, bquote(f(x) == frac(- x ^4 + 6 * x ^ 2 + 8 * x + 3, 16))) }
   if (i == 5){ text(-0.55, 0.67, bquote(f(x) == frac(- 2 * x ^ 5 - 5 * x ^ 4 + 10 * x ^ 2 + 10 * x + 3, 20)), cex = 0.8);
                text(0.55, 0.33,  bquote(f(x) == frac(  2 * x ^ 5 - 5 * x ^ 4 + 10 * x ^ 2 + 10 * x + 3, 20)), cex = 0.8)}
}

