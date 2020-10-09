clg()

# Define years:
years <- 2010:2019

# Read set data:
s <- read.scset(year = years, valid = 1)
s <- s[s$season == "fall", ]

# Read biological data:
b <- read.scbio(year = years, valid = 1)

vars <- c("COM")

s <- summary(s, category = vars, weight = TRUE, hard.shelled = TRUE, units = "kg")
s[vars] <- 1000000 * s[vars] / repvec(s$swept.area, ncol = length(vars))  # 

# Calculate empirical variograms:
v <- matrix(list(), nrow = length(years), ncol = length(vars))
for (j in 1:length(vars)){
   for (i in 1:length(years)){
      ss <- s[s$year == years[i], ] 
      trim <- NULL
      if (years[i] == 2012 & (vars %in% c("COM", "COMSC345", "COMSC12"))) trim <- -order(ss[, vars[1]])[nrow(ss)-1] 
      if (years[i] %in% 2013 & (vars %in% c("COM", "COMSC345", "COMSC12"))) trim <- -which.max(ss[, vars[1]])
      v[[i,j]] <- variogram(ss, variable = vars[j], lag = 3, max.distance = 75, fit = TRUE, inits = list(range = 20), trim = trim)
   }
}
names(v) <- years

# Plot single-year variograms:
clg()
windows(width = 8.5, height = 10)

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
   w[[i-2]] <- fit(w[[i-2]], distance.exponent = 1)
}

# Plot three-year variograms:
windows(width = 8.5, height = 10)
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

range <- c(8.984, 16.04, 44.04, 29.47, 59.26)
names(range) <- 2012:2016
c0 <- c(1.182E+6, 1.767E-3, 1.486E+6, 1.204E+6, 2.595E+6)
names(c0) <- 2012:2016
c <- c(1.806E+6, 2.439E+6, 6.836E+5, 5.957E+5, 2.915E+6)
names(c) <- 2012:2016

rownames(v) <- years
for (i in 1:length(range)){
   index <- rownames(v) == names(range)[i]
   if (sum(index) == 1){
      v[[which(index), 1]]$range <- range[i]
      v[[which(index), 1]]$nugget <- c0[i]
      v[[which(index), 1]]$sill <- c0[i] + c[i]
   }
}
v[["2017",1]]$range <- Inf
# Plot single-year variograms:
clg()
windows(width = 8.5, height = 7)

if (length(years) %% 2 == 0) n <- (length(years)-2) / 2 else n <- (length(years)-1) / 2
m <- cbind(outer(rep(1:n, each = 3), rep(1, 4)), 0, outer(rep((n+1):(2*n), each = 3), rep(1, 4)))
m <- rbind(0, cbind(0, m, 0), 0)
layout(m)

for (i in 3:length(v)){
   par(mar = c(0, 0, 0, 0))
   plot(v[[i]], xaxt = "n", xlab = "", ylab = "", yaxt = "n", scale = 10^6, ylim = c(0, 1.3* max(v[[i]]$empirical) / 1000000))
   axis(2, at = , labels = )
   if (i %in% c(5, 8)) axis(1, cex.axis = 1.25)
   text(mean(par("usr")[1:2]), par("usr")[3] + 0.94 * diff(par("usr")[3:4]), years[i], cex = 1.5, font = 2)
   #if (i == 2) mtext(parse(text = paste("Semi-variance (10^{", 5, "})")), 2, 2.5, cex = 1.5, adj = +4)
   if (i == 4) mtext(parse(text = paste("Semi-variance~(10^{", 6, "})")), 2, 2.5, cex = 1.5, at = mean(par("usr")[3:4]))
}
mtext("Distance (km)", 1, 3.0, at = -10, cex = 1.5)


range <- c(41.91, 19.42, 39.35, 29.49, 47.38)
names(range) <- 2012:2016
c0 <- c(1.977E+6, 7.814E+5, 1.020E+6, 8.773E+5, 3.308E+6)
names(c0) <- 2012:2016
c <- c(1.045E+6, 3.298E+6, 6.988E+5, 6.326E+5, 2.024E+6)
names(c) <- 2012:2016

names(w) <- years[3:length(years)]
for (i in 1:length(range)){
   index <- names(w) == names(range)[i]
   if (sum(index) == 1){
      print(21)
      w[[which(index)]]$range <- range[i]
      w[[which(index)]]$nugget <- c0[i]
      #w[[which(index)]]$sill <- c0[i] + c[i]
   }
}

clg()
windows(width = 8.5, height = 7)

if (length(years) %% 2 == 0) n <- (length(years)-2) / 2 else n <- (length(years)-1) / 2
m <- cbind(outer(rep(1:n, each = 3), rep(1, 4)), 0, outer(rep((n+1):(2*n), each = 3), rep(1, 4)))
m <- rbind(0, cbind(0, m, 0), 0)
layout(m)

for (i in 1:length(w)){
   par(mar = c(0, 0, 0, 0))
   plot(w[[i]], xaxt = "n", xlab = "", ylab = "", yaxt = "n", scale = 10^6, ylim = c(0, 1.3* max(w[[i]]$empirical) / 1000000))
   axis(2, at = , labels = )
   if (i %in% c(3, 6)) axis(1, cex.axis = 1.25)
   text(mean(par("usr")[1:2]), par("usr")[3] + 0.94 * diff(par("usr")[3:4]), years[i+2], cex = 1.5, font = 2)
   #if (i == 2) mtext(parse(text = paste("Semi-variance (10^{", 5, "})")), 2, 2.5, cex = 1.5, adj = +4)
   if (i == 2) mtext(parse(text = paste("Semi-variance~(10^{", 6, "})")), 2, 2.5, cex = 1.5, at = mean(par("usr")[3:4]))
}
mtext("Distance (km)", 1, 3.0, at = -10, cex = 1.5)


