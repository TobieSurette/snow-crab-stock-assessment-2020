

s <- read.scset(year = 2019, valid = 1, swept.area.method = "model")
s <- s[substr(s$tow.id,2,2) != "C", ]

e <- read.esonar(s)

r <- summary(e)

s$distance <- r$distance[match(s$tow.id, r$tow.id)]
s$duration <- duration(s)

mean(s$distance, na.rm = TRUE) * 1000
mean(s$duration, na.rm = TRUE) 
mean(s$swept.area, na.rm = TRUE) 
mean(s$swept.area / s$distance, na.rm = TRUE) / 1000
sd(s$swept.area / s$distance, na.rm = TRUE) / 1000



