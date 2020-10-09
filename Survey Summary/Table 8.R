#setwd("U:/Snow Crab/Survey 2018")
years <- 1997:2019
language <- "english"
language <- tolower(language)
jpeg <- TRUE
path <- "U:/Snow Crab/Stock Assessment 2019/"

b <- read.scbio(year = years)
b <- b[b$tow.id != "", ]
b <- b[b$month >= 7, ]
b <- b[substr(b$tow.id,2,2) != "C", ]

s <- read.scset(year = years, valid = 1)
s <- s[s$month >= 7, ]
s <- s[substr(s$tow.id,2,2) != "C", ]

tmp <- aggregate(list(total.sampled = is.category(b, "F")), by = b[c("year", "tow.id")], function(x) length(which(x)))
tmp$immature.sampled <- aggregate(list(x = is.category(b, "FI")), by = b[c("year", "tow.id")], function(x) length(which(x)))$x
tmp$mature.sampled <- aggregate(list(x = is.category(b, "FM")), by = b[c("year", "tow.id")], function(x) length(which(x)))$x
tmp$pubescent.sampled <- aggregate(list(x = is.category(b, "FIGNO")), by = b[c("year", "tow.id")], function(x) length(which(x)))$x
tmp$immature.sampled <- tmp$immature.sampled - tmp$pubescent.sampled
tmp$sum <- tmp$immature.sampled + tmp$pubescent.sampled + tmp$mature.sampled

index <- match(tmp[c("year", "tow.id")], s[c("year", "tow.id")])
s$total.sampled <- 0
s$immature.sampled <- 0
s$pubescent.sampled <- 0
s$mature.sampled <- 0
s$total.sampled[index] <- tmp$total.sampled 
s$immature.sampled[index] <- tmp$immature.sampled 
s$pubescent.sampled[index] <- tmp$pubescent.sampled 
s$mature.sampled[index] <- tmp$mature.sampled 
s$density <- 1000000 * s$total.sampled / s$swept.area
s$immature.density <- 1000000 * s$immature.sampled / s$swept.area
s$pubescent.density <- 1000000 * s$pubescent.sampled / s$swept.area
s$mature.density <- 1000000 * s$mature.sampled / s$swept.area

res <- aggregate(list(total = is.category(b, "F")), by = b["year"], function(x) length(which(x)))
res$immature <- aggregate(list(x = is.category(b, "FI")), by = b["year"], function(x) length(which(x)))$x 
res$pubescent <- aggregate(list(x = is.category(b, "FIGNO")), by = b["year"], function(x) length(which(x)))$x 
res$mature <- aggregate(list(x = is.category(b, "FM")), by = b["year"], function(x) length(which(x)))$x
res$immature <- res$immature - res$pubescent
res$number.tows <- aggregate(list(number.tows = s$year), by = s["year"], length)[, 2]

res$immature.per.tow <- round(res$immature / res$number.tows, 1)
res$pubescent.per.tow <- round(res$pubescent / res$number.tows, 1)
res$mature.per.tow <- round(res$mature / res$number.tows, 1)
res$total.per.tow <- res$immature.per.tow + res$pubescent.per.tow + res$mature.per.tow

res$number.sampled <- aggregate(list(x = is.category(b, "F")), by = b["year"], function(x) sum(x[!is.na(x)]))$x
res$density <- round(aggregate(list(x = s$density), by = s["year"], mean)$x)

res$immature.density <- round(aggregate(list(x = s$immature.density), by = s["year"], mean)$x)
res$pubescent.density <- round(aggregate(list(x = s$pubescent.density), by = s["year"], mean)$x)
res$mature.density <- round(aggregate(list(x = s$mature.density), by = s["year"], mean)$x)

res[setdiff(names(res), c("number.tows", "number.sampled"))]

#res[vars]
clg()
if (jpeg) jpeg(file = paste0(path, "Female Annual Tow Catches by Maturity ", language,".jpg"), width = 8.5*480, height = 6*480, res = 75 * 7) else windows(width = 8.5, height = 6)
dbarplot(res[c("immature.per.tow", "pubescent.per.tow", "mature.per.tow")], 
         res$year, legend = FALSE, col = c("grey90", "grey50", "grey20"), 
         xlab = "", ylab = "", cex.lab = 1.5, xaxt = "n", width = 1, ylim = c(0, 70), xlim = c(1996.5, 2019.5), xaxs = "i")
axis(1, at = years, las = 2)
if (language == "french"){
   mtext("Crabes par trait", 2, 2.4, cex = 1.25)
   mtext("Année", 1, 3.5, cex = 1.5)
   legend("top", legend = c("Adulte", "Adolescente", "Immature"), horiz = TRUE, pch = 22, pt.cex = 3, pt.bg = rev(c("grey90", "grey50", "grey20")), cex = 1.15)
}else{
   mtext("Crab per tow", 2, 2.4, cex = 1.25)
   mtext("Year", 1, 3.5, cex = 1.5)
   legend("top", legend = c("Adult", "Adolescent", "Immature"), horiz = TRUE, pch = 22, pt.cex = 3, pt.bg = rev(c("grey90", "grey50", "grey20")), cex = 1.15)
}
if (jpeg) dev.off()




