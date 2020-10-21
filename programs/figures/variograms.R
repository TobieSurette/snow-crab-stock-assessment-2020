library(gulf.data)
library(gulf.spatial)

# Define years:
years <- 2011:2020
category <- "COM"

# Calculate empirical variograms:
v <- vector("list", length(years))
for (i in 1:length(years)){
   # Read set data:
   s <- read.scsset(year = years[i], valid = 1)
   s <- s[survey(s) == "regular", ]

   # Read biological data:
   b <- read.scsbio(year = years[i], valid = 1)
   b <- b[survey(b) == "regular", ]
   b$tow.id <- tow.id(b)
   import(s, fill = 0) <- catch(b, category = category, weight = TRUE, hard.shelled = TRUE, units = "kg")
   s[vars] <- 1000000 * s[category] / repvec(s$swept.area, ncol = length(vars))

   v[[i]] <- variogram(s, variable = category, lag = 3, max.distance = 75, fit = TRUE, inits = list(range = 20))
}
names(v) <- years

# Plot single-year variograms:
clg()
dev.new(width = 8.5, height = 10)

if (length(years) %% 2 == 0) n <- length(years) / 2 else n <- (length(years)+1) / 2
m <- cbind(outer(rep(1:n, each = 3), rep(1, 4)), 0, outer(rep((n+1):(2*n), each = 3), rep(1, 4)))
m <- rbind(0, cbind(0, m, 0), 0)
layout(m)

for (i in 1:length(v)){
   par(mar = c(0, 0, 0, 0))
   plot(v[[i]], xaxt = "n", xlab = "", ylab = "", yaxt = "n", scale = 10^6, ylim = c(0, 1.3 * max(v[[i]]$empirical$semi.variance)/1000000))
   axis(2, at = , labels = )
   if (i %in% c(n, 2*n)) axis(1, cex.axis = 1.25)
   text(mean(par("usr")[1:2]), par("usr")[3] + 0.94 * diff(par("usr")[3:4]), years[i], cex = 1.5, font = 2)
   #if (i == 2) mtext(parse(text = paste("Semi-variance~(10^{", 5, "})")), 2, 2.5, cex = 1.5, adj = +4)
   if (i == round((n+0.01)/2)) mtext("Semi-variance", 2, 2.5, cex = 1.5, at = mean(par("usr")[3:4]))
}
mtext("Distance (km)", 1, 3.0, at = -10, cex = 1.5)

# Calculate 3-year variograms:
w <- list()
for (i in 3:length(v)){
   res <- data.frame(start.distance = seq(0, 72, by = 3))
   res$end.distance <- res$start.distance + 3
   res[,3:5] <- NA
   for (k in 3:1){
      vv <- v[[i-k+1]]
      index <- match(vv$empirical$start.distance, res$start.distance)
      res[index, k+2] <- vv$empirical$semi.variance / vv$var
   }
   index <- match(vv$empirical$start.distance, res$start.distance)
   res$h <- NA
   res$n <- NA
   res[index, "h"] <- vv$empirical$h
   res[index, "n"] <- vv$empirical$n
   res$semi.variance <- vv$var * apply(res[, 3:5], 1, mean, na.rm = TRUE)
   if (length(which(is.na(res$h) | is.na(res$n)) > 0)) res <- res[-which(is.na(res$h) | is.na(res$n)), ]
   w[[i-2]] <- v[[i]]
   w[[i-2]]$empirical$semi.variance <- res$semi.variance
   w[[i-2]] <- fit.variogram(w[[i-2]], distance.exponent = 1)
}

# Plot three-year variograms:
dev.new(width = 8.5, height = 10)
if (length(w) %% 2 == 0) n <- length(w) / 2 else n <- (length(w)+1) / 2
m <- cbind(outer(rep(1:n, each = 3), rep(1, 4)), 0, outer(rep((n+1):(2*n), each = 3), rep(1, 4)))
m <- rbind(0, cbind(0, m, 0), 0)
layout(m)

for (i in 1:length(w)){
   par(mar = c(0, 0, 0, 0))
   plot(w[[i]], xaxt = "n", xlab = "", ylab = "", yaxt = "n", scale = 10^6, ylim = c(0, 1.3 * max(w[[i]]$empirical$semi.variance)/1000000), show.nugget = FALSE, show.sill = FALSE)
   axis(2, at = , labels = )
   if (i %in% c(n, 2*n)) axis(1, cex.axis = 1.25)
   text(mean(par("usr")[1:2]), par("usr")[3] + 0.94 * diff(par("usr")[3:4]), years[i+2], cex = 1.5, font = 2)
   #if (i == 2) mtext(parse(text = paste("Semi-variance~(10^{", 5, "})")), 2, 2.5, cex = 1.5, adj = +4)
   if (i == round((n+0.01)/2)) mtext("Semi-variance", 2, 2.5, cex = 1.5, at = mean(par("usr")[3:4]))
}
mtext("Distance (km)", 1, 3.0, at = -10, cex = 1.5)

