#setwd("U:/Snow Crab/Survey 2018")
years <- 1997:2019
language <- "french"
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

tmp <- aggregate(list(total.sampled = is.category(b, "M")), by = b[c("year", "tow.id")], function(x) length(which(x)))
index <- match(tmp[c("year", "tow.id")], s[c("year", "tow.id")])
s$total.sampled <- 0
s$total.sampled[index] <- tmp$total.sampled 
s$density <- 1000000 * s$total.sampled / s$swept.area


res <- aggregate(list(immature = is.category(b, "MI")), by = b["year"], function(x) length(which(x)))
res$mature <- aggregate(list(x = is.category(b, "MM")), by = b["year"], function(x) length(which(x)))$x
res$number.tows <- aggregate(list(number.tows = s$year), by = s["year"], length)[, 2]

res$immature.per.tow <- round(res$immature / res$number.tows, 1)
res$mature.per.tow <- round(res$mature / res$number.tows, 1)
res$total.per.tow <- res$immature.per.tow + res$mature.per.tow

res$number.sampled <- aggregate(list(x = is.category(b, "M")), by = b["year"], function(x) sum(x[!is.na(x)]))$x
res$density <- round(aggregate(list(x = s$density), by = s["year"], mean)$x)

res[setdiff(names(res), "number.sampled")]

#res[vars]

clg()
if (jpeg) jpeg(file = paste0(path, "Male Annual Tow Catches by Maturity ", language,".jpg"), width = 8.5*480, height = 6*480, res = 75 * 7) else windows(width = 8.5, height = 6)
dbarplot(res[c("immature.per.tow", "mature.per.tow")], res$year, legend = FALSE,  
         col = c("grey90", "grey50"), xlab = "", ylab = "", cex.lab = 1.25, xaxt = "n", width = 1, 
         xlim = c(1996.5, 2019.5), xaxs = "i")
axis(1, at = years, las = 2)
if (language == "french"){
   mtext("Crabe par trait", 2, 2.4, cex = 1.25)
   mtext("Année", 1, 3.5, cex = 1.25)
   legend("top", legend = c("Adulte", "Adolescent"), horiz = TRUE, pch = 22, pt.cex = 3, pt.bg = c("grey50", "grey90"), cex = 1.15)
}else{
   mtext("Crab per tow", 2, 2.4, cex = 1.25)
   mtext("Year", 1, 3.5, cex = 1.25)
   legend("top", legend = c("Adult", "Adolescent"), horiz = TRUE, pch = 22, pt.cex = 3, pt.bg = c("grey50", "grey90"), cex = 1.15)
}
if (jpeg) dev.off()