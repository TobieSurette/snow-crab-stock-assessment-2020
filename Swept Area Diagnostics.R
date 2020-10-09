s <- read.scset(year = 2007:2019)
#s <- read.scset(year = 2019)
s <- s[(s$season != "summer") & (s$valid == 1), ]
s <- s[substr(s$tow.id,2,2) != "C", ]
s <- s[s$swept.area.method == "model", ]

s$distance <- distance(s$longitude.start, s$latitude.start, s$longitude.end, s$latitude.end, pairwise = FALSE)
s <- s[s$distance > 0.25 & s$distance < 0.4, ]

s$width <- s$swept.area / (1000 * s$distance)

boxplot(s$width ~ s$year, ylim = c(0, 14))

res <- aggregate(list(mean = s[, "width"]), by = s["year"], mean, na.rm = TRUE) 
res$sd <- aggregate(list(x = s[, "width"]), by = s["year"], sd, na.rm = TRUE)$x  
res$n <- aggregate(list(x = s[, "width"]), by = s["year"], length)$x  

res$lowerCI <- res$mean - 1.96 * res$sd / sqrt(res$n)
res$upperCI <- res$mean + 1.96 * res$sd / sqrt(res$n)

plot(res$year, res$mean, ylim = c(7.8, 9.2), ylab = "Mean trawl width (meters)", xlab = "Year", cex.lab = 1.4)
grid()
for (i in 1:nrow(res)){
   lines(rep(res$year[i],2), c(res$lowerCI[i], res$upperCI[i]))
}


