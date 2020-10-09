

zone <- read.csv("U:/Snow Crab/Stock Assessment 2019/NARW static closure zone 2019.csv", header = TRUE)

p <- as.polygon(zone$long.dec, zone$lat.dec)

s <- read.scset(year = 2000:2019, valid = 1)
b <- read.scbio(year = sort(unique(s$year)))

index <- which(in.polygon(p, longitude(s), latitude(s)))

s <- s[index, ]
index <- match(b[c("year", "tow.id")], s[c("year", "tow.id")])

b <- b[!is.na(index), ]

b <- b[which(is.category(b, category = "COM")), ]
b$mossy <- (b$shell.condition >= 4) + 1 - 1

t <- table(b$year, b$shell.condition)


p <- apply(t[, 4:5], 1, sum) / apply(t, 1, sum)

b$yearf <- as.factor(b$year)
model <- glm(mossy ~ yearf, data = b, family = binomial)
p <- predict(model, newdata = data.frame(yearf = unique(b$yearf)), se.fit = TRUE)
p$lci <- p$fit - 1.96 * p$se.fit
p$uci <- p$fit + 1.96 * p$se.fit

p$mu <- exp(p$fit) / (1 + exp(p$fit))
p$lci <- exp(p$lci) / (1 + exp(p$lci))
p$uci <- exp(p$uci) / (1 + exp(p$uci))

names(p$mu) <- sort(unique(s$year))
names(p$lci) <- sort(unique(s$year))
names(p$uci) <- sort(unique(s$year))

dbarplot(p$mu, width = 1)
grid()
dbarplot(p$mu, width = 1, add = TRUE)
mtext("Year", 1, 2.5, cex = 1.4)
mtext("Proportion", 2, 2.5, cex = 1.4)
box()

