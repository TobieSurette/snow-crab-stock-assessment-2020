
sex <- 2
if (sex == 1){
   xlim = c(0, 140) 
   ylim = c(0, 1.25) 
}else{
   xlim = c(0, 95)
   ylim = c(0, 2.5)
}
files <- paste0("Y:/research/groundfish/", c("rvt896l.new", "rvt901l.new"))
len <- read.rvlen(file = files, species = 2526)

files <- paste0("Y:/research/groundfish/", c("rvt896s.new", "rvt901s.new"))
set <- read.rvset(file = files)
set <- set[set$experiment == 1, ]
     
f <- freq(len, by = c("year", "sex", "set.number"))
fvars <- setdiff(names(f), c("year", "sex", "set.number"))

f$sex[f$sex == 0] <- sex
f <- f[f$sex == sex, ]

index <- match(f[c("year", "set.number")], set[c("year", "set.number")])
set[fvars] <- 0
set[index, fvars] <- f[fvars]
set[fvars] <- 1.75 * set[fvars] / repvec(set$distance, ncol = length(fvars))

res <- aggregate(set[fvars], by = set[c("year", "stratum")], mean)

area <- stratum.info()$trawlable.units
area <- area / sum(area)
names(area) <- stratum.info()$stratum

res[fvars] <- res[fvars] * repvec(area[as.character(res$stratum)], ncol = length(fvars)) 
res <- aggregate(res[fvars], by = res["year"], sum)

windows(height = 11, width = 8.5)
m <- kronecker(matrix(1:3, ncol = 1), matrix(1, nrow = 4, ncol = 5))
m <- rbind(0, cbind(0, m, 0), 0)
layout(m)
par(mar = c(0,0,0,0))
plot(xlim, ylim, xaxt = "n",  xaxs = "i", yaxs = "i", type = "n"); grid();
dbarplot(res[1, fvars], width = 1, xaxt = "n",  xaxs = "i", add = TRUE, border = "grey40")
lines(c(95,95), par("usr")[3:4], col = "red", lty = "dashed", lwd = 1)
box()
mtext("Crab per tow", 2, 2.5, at = mean(ylim))
plot(xlim, ylim, xaxt = "n", xaxs = "i", yaxs = "i", type = "n"); grid();
dbarplot(res[2, fvars], width = 1, xaxt = "n", xlim = c(0, 140), xaxs = "i", add = TRUE, border = "grey40")
lines(c(95,95), par("usr")[3:4], col = "red", lty = "dashed", lwd = 1)
box()
mtext("Crab per tow", 2, 2.5, at = mean(ylim))
plot(xlim, ylim, xaxt = "n", xaxs = "i", yaxs = "i", type = "n"); grid();
dbarplot(res[2, fvars] /  (res[1, fvars]), width = 1, xaxs = "i", add = TRUE, border = "grey40")
lines(c(95,95), par("usr")[3:4], col = "red", lty = "dashed", lwd = 1)
lines(par("usr")[1:2], c(1,1), col = "red", lwd = 2)
mtext("Ratio 2019 vs 2018", 2, 2.5, at = 1)
box()
axis(1)
mtext("Carapace width (mm)", 1, 2.5)