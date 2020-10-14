library(gulf)

years <- 2012:2013
z <- read.scset(year = years, valid = 1)
z <- z[substr(z$tow.id,2,2) != "C", ]
z <- z[z$month >= 7, ]

x <- NULL
for (i in 1:length(years)){
   tmp <- read.sccat(year = years[i])
   x <- rbind(x, tmp[setdiff(names(tmp), "comment")])
}
x <- x[(substr(x$tow.id,2,2) != "C"), ]
x <- x[x$month >= 7, ]
x$species[x$species %in% c(2520, 2521)] <- 2527

species <- c(10, 12, 23, 40, 41, 42, 43, 201, 202, 300, 301, 302, 304, 313, 340, 350, 410, 512, 620, 626, 1823, 2527, 4210, 6400, 6500)
x <- x[(x$species %in% species), ]

z[as.character(species)] <- 0
for (i in 1:length(species)){
   xx <- x[x$species == species[i], ]
   if (nrow(xx) > 0){
      index <- match(xx[c("year", "tow.id")], z[c("year", "tow.id")])
      z[index, as.character(species[i])] <- xx$number.caught
   }
}
z[as.character(species)] <- 1000000 * z[as.character(species)] / repvec(z$swept.area, ncol = length(species))

species <- species
tmp <- NULL
year <- years[1]
for (j in 1:length(species)){
   res <- data.frame(tow.id = unique(substr(z$tow.id, 3, 5)), stringsAsFactors = FALSE)
   res[as.character(years)] <- 0
   for (i in 1:length(years)){
      xx <- data.frame(tow.id = z$tow.id[z$year == years[i]], number.caught = z[z$year == years[i], as.character(species[j])], stringsAsFactors = FALSE)
      xx$tow.id <- substr(xx$tow.id, 3, 5)
      res[match(xx$tow.id, res$tow.id), as.character(years[i])] <- xx$number.caught
   }
   delta <- log(res[,as.character(year+1)]) - log(res[,as.character(year)])
   delta <- delta[which(is.finite(delta) & !is.na(delta))]
   tmp <- rbind(tmp, c(mean(delta), mean(delta) - 1.96 * sd(delta) / sqrt(length(delta)), mean(delta) + 1.96 * sd(delta) / sqrt(length(delta))))
}


clg()
windows()
m <- rbind(0, cbind(0, matrix(1, nrow = 5, ncol = 6), 0), 0, 0)
layout(m)
par(mar = c(0,0,0,0))
plot(c(0.5, nrow(tmp)+0.5), c(0, 3), type = "n", xaxt = "n", yaxs = "i", xlab = "", ylab = "")
grid()
rownames(tmp) <- NULL
index <- which(0 >= tmp[, 3] | 0 <= tmp[, 2])
dbarplot(exp(tmp[index, 1]), index,  col = "grey", ylim = c(-2, 2), width = 1, add = TRUE)
dbarplot(exp(tmp[setdiff(1:nrow(tmp),index), 1]), setdiff(1:nrow(tmp),index),  col = "white", ylim = c(-2, 2), width = 1, add = TRUE)
for (i in 1:length(index)){
   lines(rep(index[i], 2), exp(tmp[index[i], 2:3]), lwd = 2)
   lines(c(index[i] - 0.15, index[i] + 0.15), exp(rep(tmp[index[i], 2], 2)), lwd = 2)
   lines(c(index[i] - 0.15, index[i] + 0.15), exp(rep(tmp[index[i], 3], 2)), lwd = 2)
   #text(index[i], exp(tmp[index[i], 3]+0.05), species.str(species[index[i]], source = "gulf"), pos = 4, srt = 90, offset = 0, cex = 0.65)
}
#text(setdiff(1:nrow(tmp),index), exp(tmp[setdiff(1:nrow(tmp),index), 1]+0.05), species.str(species[setdiff(1:nrow(tmp),index)], source = "gulf"), pos = 4, srt = 90, offset = 0, cex = 0.65)
lines(par("usr")[1:2], c(1,1), lwd = 2, col = "red")
axis(1, at = 1:nrow(tmp), labels = species.str(species, source = "gulf"), las = 2, cex.axis = 0.75)
#mtext("Species", 1, 1.0, cex = 1.5)
mtext("Catch density ratio", 2, 2.0, cex = 1.5)
mtext(paste0(year+1, " versus ", year), 3, 1.0, cex = 1.5)
box()






