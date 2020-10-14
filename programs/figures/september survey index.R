library(gulf)

# Calculate length-weight coefficients for female snow crab:
bio <- read.rvbio(year = 2016:2018, species = 2526)
tmp <- read.rvbio(file = '//DFNBE1CwpFSP002/Hd2/research/groundfish/rvt901b.new', species = 2526)
bio <- rbind(bio, tmp)
index <- (bio$sex == 2) & (!is.na(bio$length)) & (bio$length > 0) & (bio$weight > 0) & !is.na(bio$weight) & !is.na(bio$maturity) & (bio$maturity %in% c(1, 2)) 
yy <- bio$weight[index]
xx <- bio$length[index]
m <- lm(log(yy) ~ log(xx))
r <- residuals(m)
index <- (r >= -1) & (r <= 1)
xx <- xx[index]
yy <- yy[index]
plot(log(xx), log(yy))
m <- lm(log(yy) ~ log(xx))
hist(residuals(m), n = 100, col = "grey")
plot(m)

# Calculate proportion of immature and mature male snow crab by weight:
b <- read.scbio(year = 2001:2019)
b <- b[b$sex == 1 & !is.na(b$carapace.width) & (b$carapace.width >= 95), ]
b$weight <- weight(b)
b <- b[!is.na(b$weight), ]
index <- is.category(b, "MI")
res <- aggregate(list(immature = b$weight[index]), by = b[index, "year", drop = FALSE], sum)
index <- is.category(b, "MM")
res$mature <- aggregate(list(x = b$weight[index]), by = b[index, "year", drop = FALSE], sum)$x
res$p <- res$mature / (res$immature + res$mature)

# RV length-frequency to determine annual proportion by sex by weight:
len <- read.rvlen(year = 2001:2018, species = 2526)
tmp <- read.rvlen(file = '//DFNBE1CwpFSP002/Hd2/research/groundfish/rvt901l.new', species = 2526)
len <- rbind(len, tmp)
vars <- c("year", "vessel.code", "cruise.number", "set.number", "species", "sex") 
f <- freq(len, by = vars, scale = TRUE)
fvars <- setdiff(names(f), vars)
lens <- repvec(as.numeric(fvars), nrow = nrow(f))
index <- f$sex %in% c(0, 1, 3, 9)
w <- lens * NA
w[index, ] <- as.matrix(f[index, fvars]) * exp(-8.230136 + 3.098 * log(lens[index, ])) / 1000
index <- f$sex %in% c(2)
w[index, ] <- as.matrix(f[index, fvars]) * exp(-7.639 + 2.922 * log(lens[index, ])) / 1000    
colnames(w) <- fvars
f$weight95 <- apply(w[,fvars[as.numeric(fvars) >= 95]], 1, sum)
f$weight <- apply(w[,fvars], 1, sum)
rs <- aggregate(list(male95 = (f$sex == 1) * f$weight95),  by = f[setdiff(vars, "sex")], sum)
rs$weight <- aggregate(list(x = f$weight),  by = f[setdiff(vars, "sex")], sum)$x
rs$p <- rs$male95 / rs$weight

# Apply proportions to RV catches
vars <- setdiff(vars, "sex")
cat <- read.rvcat(year = 2001:2018, species = 2526)
tmp <- read.rvcat(file = '//DFNBE1CwpFSP002/Hd2/research/groundfish/rvt901c.new', species = 2526)
cat <- rbind(cat, tmp)
index <- match(rs[vars], cat[vars])
cat$p <- NA
cat$p[index] <- rs$p
tmp <- aggregate(list(p = cat$p), by = cat["year"], mean, na.rm = TRUE)
index <- is.na(cat$p)
cat$p[index] <- tmp$p[match(cat$year[index], tmp$year)]
cat$weight.legal <- cat$weight.caught * cat$p
cat$weight.commercial <- cat$weight.legal * res$p[match(cat$year, res$year)]

# Merge with tow data:
set <- read.rvset(year = 2001:2018)
tmp <- read.rvset(file = '//DFNBE1CwpFSP002/Hd2/research/groundfish/rvt901s.new')
set <- rbind(set, tmp)
set <- set[set$experiment %in% c(1, 5), ]

set <- merge.catch(set, cat, var = "weight.commercial")
set <- set[!(set$stratum %in% c(401:403, 420, 421, 428, 432, 435, 440:442)), ]
area <- sum(stratum.info(stratum = unique(set$stratum))$trawlable.units)
set$weight.commercial <- (1.75 / set$distance) * set$weight.commercial * area
set$weight.commercial <- set$weight.commercial / 1000

set$time <- (time(set) - date(set)) / 3600
set$test <- set$weight.commercial
set$test[(set$time < 7) | (set$time > 19)] <- set$test[(set$time < 7) | (set$time > 19)] / 1.424755 

#r <- smean(set, var = "weight.commercial", by = "year")
r <- smean(set, var = "test", by = "year")

index <- r$year == 2003
r[index, setdiff(names(r), "year")] <- NA

language <- "english"
jpeg <- TRUE
if (jpeg){
   jpeg(file = paste0("U:/Snow Crab/Stock Assessment 2019/RV Index ", min(set$year), "-", max(set$year), " ", language, ".jpeg"),
        width = 8 * 480, height = 8 * 480, res = 8*75)  
}else{
   windows(width = 8.5)
}
plot(c(2000.25, 2019.75), c(0, 8000), type = "n", xaxs = "i", yaxs = "i", xaxt = "n", xlab = "", ylab = "")
#dbarplot(r$mean, r$year, xlim = c(2000.25, 2017.75), xaxs = "i", xaxt = "n", xlab = "", ylim = c(0, 8000))
grid()
#dbarplot(r$mean, r$year, add = TRUE)
axis(1, at = seq(2001, 2019, by = 3))
#/, at = seq(2001, 2017, by = 2), las = 2)
#axis(1, at = seq(2002, 2017, by = 2), las = 2)
if (language == "english"){
   mtext("Year", 1, 3.5, cex = 1.5)
   mtext("Commercial snow crab trawlable biomass (tonnes)", 2, 2.5, cex = 1.2)
}else{
   mtext("Année", 1, 3.5, cex = 1.5)
   mtext("Biomasse chalutable de crabe commercial (tonnes)", 2, 2.5, cex = 1.2)
}
for (i in 1:nrow(res)){
   lines(rep(r$year[i], 2), c(r$lower.cl[i], r$upper.cl[i]), lwd = 2)
   lines(c(r$year[i]-0.16, r$year[i]+0.16), c(r$lower.cl[i], r$lower.cl[i]), lwd = 2)
   lines(c(r$year[i]-0.16, r$year[i]+0.16), c(r$upper.cl[i], r$upper.cl[i]), lwd = 2)                            
}
points(r$year, r$mean, pch = 21, bg = "grey", cex = 1.25)
box()
if (jpeg) dev.off()

(r$mean[19] - r$mean[18]) / r$mean[18]







y <- read.rvbio(year = year, species = 2526)
y <- y[y$sex == 1 & !is.na(y$sex), ] 
y$chela[(y$chela < 1) | (y$chela > 40)] <- NA

tmp <- as.polygon(c(37.39531, 31.61437, 38.38633, 49.78305, 75.05404, 89.09348, 102.47224, 109.07903, 123.28364, 104.28911, 82.48668, 37.39531),
                  c(36.145976, 10.119323, 8.740031, 11.798462, 19.954280, 25.111635, 29.129575, 32.188006, 38.064992, 41.183393, 41.063455, 36.145976))
index <- in.polygon(tmp, y$length, y$chela)                  
y <- y[which(!index), ]

plot(jitter(y$length, amount = 0.5), jitter(y$chela, amount = 0.5), pch = 21, bg = "grey", cex = 0.5)
y$carapace.width <- y$length
y$chela.height <- y$chela
m <- is.mature.scbio(y)

points(jitter(y$length[m], amount = 0.5), jitter(y$chela[m], amount = 0.5), pch = 21, bg = "red", cex = 0.5)
points(jitter(y$length[!m], amount = 0.5), jitter(y$chela[!m], amount = 0.5), pch = 21, bg = "green", cex = 0.5)


model <- gam(m ~ s(length), data = y, family = binomial)

p <- predict(model, newdata = list(length = 40:140))
p <- exp(p) / (1+ exp(p))


p <- predict(model, newdata = list(length = y$length[is.na(m)]))
p <- exp(p) / (1+ exp(p))
m[is.na(m)] <- p
y$mature <- m


y$weight <- exp(-8.230136 + 3.098 * log(y$carapace.width))

vars <- c("year", "vessel.code", "cruise.number", "set.number" )
res <- aggregate(list(weight.caught = (y$carapace.width >= 95) * y$mature * y$weight / 1000), by = y[vars], sum)

s <- read.rvset(year = year, experiment = 1)

index <- match(res[vars], s[vars])
s$weight.caught <- 0
s$weight.caught[index] <- res$weight.caught
s$weight.caught <- (1.75 / s$distance) * s$weight.caught  
 
s$weight.caught <- s$weight.caught * sum(stratum.info()$trawlable.units) / 1000
a <- smean(s, var = "weight.caught", by = "year")

diff(a$mean) / a$mean[1]














m[is.na(m) & y$length >= 95, ] <- TRUE


y$length >= 95
aggregate


