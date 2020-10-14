library(gulf)

x <- read.scset(year = 1997:2019, valid = 1)
x <- x[x$month > 6, ]
x <- x[substr(x$tow.id,2,2) != "C", ]
x <- x[x$swept.area.method != "average", ]

res <- aggregate(x["swept.area"], by = x["year"], mean)
res$sigma <- aggregate(x["swept.area"], by = x["year"], sd)[, 2]
res$n <- aggregate(x["swept.area"], by = x["year"], length)[, 2] 
res$lower <- res$swept.area - 1.96 * res$sigma / sqrt(res$n)
res$upper <- res$swept.area + 1.96 * res$sigma / sqrt(res$n)

plot(c(min(res$year)-0.5, max(res$year)+0.5), c(2000, 3500), yaxs = "i", xaxs = "i", xlab = "", ylab = "", xaxt = "n")
grid()
dbarplot(res[, 2], res[, 1], ylim = c(2000, 3500), width = 1, add = TRUE)

mtext("Year", 1, 2.5, cex = 1.5)
mtext(expression(paste("Mean swept area (m"^"2", ")")), 2, 2.1, cex = 1.5)
              
for (i in 1:nrow(res)){
   lines(rep(res$year[i],2), c(res$lower[i], res$upper[i]), lwd = 2)
   lines(c(res$year[i]-0.13, res$year[i]+0.13), rep(res$lower[i], 2), lwd = 2) 
   lines(c(res$year[i]-0.13, res$year[i]+0.13), rep(res$upper[i], 2), lwd = 2) 
}
box()
axis(1, at = seq(min(res$year), 2019, by = 4), cex.lab = 0.7)
axis(1, at = seq(min(res$year)+2, 2019, by = 4), cex.lab = 0.7)



