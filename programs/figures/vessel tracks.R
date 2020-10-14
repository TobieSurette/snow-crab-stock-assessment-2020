library(gulf.data)

s <- read.scsset(2020, valid = 1)
s <- s[s$tow.id != "GP276F", ]

plot(c(-500, 500), c(-500, 500), type = "n", xlab = "x(meters)", ylab = "y(meters)", xaxs = "i", yaxs = "i")
grid()
for (i in 1:nrow(s)){
   e <- read.esonar(s[i, ])
   e <- trim(e, range = c(time(s[i, ], "touchdown"), time(s[i, ], "liftoff")))
   y <- 1000*deg2km(lon(e), lat(e))
   y[,1] <- y[,1] - y[1,1]
   y[,2] <- y[,2] - y[1,2]
   t <- time2min(time(e), time(s[i, ], "touchdown"))
   stop <- time2min(time(s[i, ], "stop"), time(s[i, ], "touchdown"))
   index <- t <= stop
   lines(y[index,1], y[index,2], col = "grey")
   lines(y[!index,1], y[!index,2], col = "red")
}
points(0, 0, pch = 21, bg = "grey50")
