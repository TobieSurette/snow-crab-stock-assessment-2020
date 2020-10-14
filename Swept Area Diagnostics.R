library(gulf.data)

years <- 2017:2020
# Compile stats:
res <- NULL
for (i in 1:length(years)){
   s <- read.scsset(year = years[i], valid = TRUE)
   s <- s[as.numeric(substr(s$date, 6, 7)) %in% 7:10, ]
   s$year <- year(s)

   s$duration <- NA
   if (unique(year(s)) %in% 2010:2016) s$duration <- as.numeric(time(s, "end") - time(s, "start"))
   if (unique(year(s)) %in% 2017:2019) s$duration <- as.numeric(time(s, "end") - time(s, "touchdown"))
   if (unique(year(s)) %in% 2020)      s$duration <- as.numeric(time(s, "stop") - time(s, "touchdown"))

   res <- rbind(res, s[c("year", "duration", "swept.area")])
}
res$duration[res$duration > 30] <- NA
res$duration[res$duration <= 0] <- NA
boxplot(swept.area ~ year, data = res, ylim = c(2500, 3200))
boxplot(duration ~ year, data = res, ylim = c(4.75, 5.8))

s <- read.scsset(2020, valid = 1)
b <- read.scsbio(2020)
a <- unique(b[c("date", "tow.id")])

match(a, read.scsset(2020, valid = 1))
catch(b, category = category(), by = c("date", "tow.id"))


catch(s) <- catch(b, category = "TM", by = c("date", "tow.id"))

catch(s) <- catch(b, category = category(), by = c("date", "tow.id"))

#s <- read.scset(year = 2019)
#s <- s[(s$season != "summer") & (s$valid == 1), ]
s <- s[substr(s$tow.id,2,2) != "C", ]
s <- s[s$swept.area.method == "model", ]

s$distance <- distance(s$longitude.start, s$latitude.start, s$longitude.end, s$latitude.end, pairwise = FALSE)
s <- s[s$distance > 0.25 & s$distance < 0.4, ]

s$width <- s$swept.area / (1000 * s$distance)

boxplot(s$width ~ s$year, ylim = c(0, 14))

res    <- aggregate(list(mean = s[, "width"]), by = s["year"], mean, na.rm = TRUE)
res$sd <- aggregate(list(x = s[, "width"]), by = s["year"], sd, na.rm = TRUE)$x
res$n  <- aggregate(list(x = s[, "width"]), by = s["year"], length)$x

res$lowerCI <- res$mean - 1.96 * res$sd / sqrt(res$n)
res$upperCI <- res$mean + 1.96 * res$sd / sqrt(res$n)

plot(res$year, res$mean, ylim = c(7.8, 9.2), ylab = "Mean trawl width (meters)", xlab = "Year", cex.lab = 1.4)
grid()
for (i in 1:nrow(res)){
   lines(rep(res$year[i],2), c(res$lowerCI[i], res$upperCI[i]))
}


