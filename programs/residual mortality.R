library(gulf.data)
library(gulf.stats)
library(gulf.graphics)

sex = 1

# Define mature recruitment data:
s <- read.scsset(years, survey = "regular", valid = 1)
s$grid <- deg2grid(lon(s), lat(s))
b <- read.scsbio(years, survey = "regular", sex = sex)
b$tow.id <- tow.id(b)
if (sex == 1) b$maturity <- morphometric.maturity(b) else b$maturity <- is.mature(b)
b <- b[which(b$carapace.width >= 35), ]
ix <- which(b$maturity)
import(s, fill = 0) <- freq(b[ix, ], by = key(s), step = 1)
fvars <- names(s)[gsub("[0-9.]", "", names(s)) == ""]
s[fvars] <- 1000000 * s[fvars] / repvec(s$swept.area, ncol = length(fvars))
f_rec <- NULL
for (i in 1:length(years)){
  ss <- s[year(s) == years[i], c("grid", fvars)]
  if (years[i] <= 2011) ss <- aggregate(ss[fvars], by = ss["grid"], mean) 
  tmp <- apply(ss[fvars], 2, mean)
  tmp[setdiff(as.character(seq(3, xlim[2]-20, by = step)), names(tmp))] <- 0
  tmp <- tmp[order(as.numeric(names(tmp)))]
  tmp <- t(tmp)
  rownames(tmp) <- years[i]
  f_rec <- rbind(f_rec, tmp)
}


# Define mature residual data:
s <- read.scsset(years, survey = "regular", valid = 1)
s$grid <- deg2grid(lon(s), lat(s))
b <- read.scsbio(years, survey = "regular", sex = sex)
b$tow.id <- tow.id(b)
if (sex == 1) b$maturity <- morphometric.maturity(b) else b$maturity <- is.mature(b)
b <- b[which(b$carapace.width >= 35), ]
ix <- which(b$maturity & !is.new.shell(b))
import(s, fill = 0) <- freq(b[ix, ], by = key(s), step = 1)
fvars <- names(s)[gsub("[0-9.]", "", names(s)) == ""]
s[fvars] <- 1000000 * s[fvars] / repvec(s$swept.area, ncol = length(fvars))
f_res <- NULL
for (i in 1:length(years)){
  ss <- s[year(s) == years[i], c("grid", fvars)]
  if (years[i] <= 2011) ss <- aggregate(ss[fvars], by = ss["grid"], mean) 
  tmp <- apply(ss[fvars], 2, mean)
  tmp[setdiff(as.character(seq(3, xlim[2]-20, by = step)), names(tmp))] <- 0
  tmp <- tmp[order(as.numeric(names(tmp)))]
  tmp <- t(tmp)
  rownames(tmp) <- years[i]
  f_res<- rbind(f_res, tmp)
}

# Calculate proportion of old-shelled males:
f_rec <- f_rec[, colnames(f_rec) %in% colnames(f_res)]
p <- f_res[2:nrow(f_res), ] / f_rec[1:(nrow(f_rec)-1), ]
p <- p[, colnames(p) %in% as.character(40:140)]
p[p > 1] <- 1

# Plot results:
clg()
colorbar(n = 10, caption = "Proportion")
image(years[-1], as.numeric(colnames(p)), 
      p, ylim = c(55, 130), 
      col = colorRampPalette(c("white", "black"))(100), xlab = "", ylab = "", xaxt = "n")
hline(95, lwd = 1.5, col = "red", lty = "dashed")
mtext("Carapace width (mm)", 2, 2.25, cex = 1.25)
axis(1, at = 2007:2020, labels = paste0(years[-1], " vs ", years[-length(years)]), las = 2)
mtext("Year", 1, 6.5, cex = 1.25)

