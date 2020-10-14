years <- 1997:2019

b <- read.scbio(year = years)
b <- b[b$tow.id != "", ]
b <- b[b$month >= 7, ]
b <- b[substr(b$tow.id,2,2) != "C", ]

s <- read.scset(year = years, valid = 1)
s <- s[s$month >= 7, ]
s <- s[substr(s$tow.id,2,2) != "C", ]

tmp <- aggregate(list(number.sampled = is.category(b, "MMGE95")), by = b[c("year", "tow.id")], function(x) length(which(x)))
index <- match(tmp[c("year", "tow.id")], s[c("year", "tow.id")])
s$number.sampled <- 0
s$number.sampled[index] <- tmp$number.sampled 
s$density <- 1000000 * s$number.sampled / s$swept.area

res <- aggregate(list(number.sampled = is.category(b, "MMGE95")), by = b["year"], function(x) length(which(x)))
res$number.tows <- aggregate(list(number.tows = s$year), by = s["year"], length)[, 2]
res$number.per.tow <- round(res$number.sampled / res$number.tows, 1)
res$weight.sampled <- aggregate(list(x = is.category(b, "MMGE95") * weight(b, hard.shelled = TRUE, units = "kg")), by = b["year"], function(x) sum(x[!is.na(x)]))$x
res$mean.weight <- round(1000 * res$weight.sampled / res$number.sampled, 1)
res$density <- aggregate(list(x = s$density), by = s["year"], mean)$x

vars <- c("year", "number.sampled", "number.per.tow", "mean.weight", "density")
res[vars]


