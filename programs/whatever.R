library(gulf.data)


p <- read.csv("data/raw/scs.passive.phase.swept.area.2017-2019.csv")

# Calculate correction factors:
s <- read.scsset(2017:2019, valid = 1, survey = "regular")
s$year <- year(s)

ix <- match(s[c("year", "tow.id")], p[c("year", "tow.id")])
s$passive <- p$passive.swept.area[ix]

s18 <- s[s$year == 2018, ]
s19 <- s[s$year == 2019, ]

ix <- match(substr(s19$tow.id, 3, 5), substr(s18$tow.id, 3, 5))
s19$passive18 <- s18$passive[ix]

s <- s19

f <- (s$swept.area + s$passive) / (s$swept.area + s$passive18)

ix <- !is.na(f)
s$swept.area[ix] <- f[ix] * s$swept.area[ix]

z <- s
