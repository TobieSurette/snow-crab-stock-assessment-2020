


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
  f_res <- rbind(f_res, tmp)
}


# Define mature old shell data:
s <- read.scsset(years, survey = "regular", valid = 1)
s$grid <- deg2grid(lon(s), lat(s))
b <- read.scsbio(years, survey = "regular", sex = sex)
b$tow.id <- tow.id(b)
if (sex == 1) b$maturity <- morphometric.maturity(b) else b$maturity <- is.mature(b)
b <- b[which(b$carapace.width >= 35), ]
ix <- which(b$maturity & (b$shell.condition %in% 4:5))
import(s, fill = 0) <- freq(b[ix, ], by = key(s), step = 1)
fvars <- names(s)[gsub("[0-9.]", "", names(s)) == ""]
s[fvars] <- 1000000 * s[fvars] / repvec(s$swept.area, ncol = length(fvars))
f_old <- NULL
for (i in 1:length(years)){
  ss <- s[year(s) == years[i], c("grid", fvars)]
  if (years[i] <= 2011) ss <- aggregate(ss[fvars], by = ss["grid"], mean) 
  tmp <- apply(ss[fvars], 2, mean)
  tmp[setdiff(as.character(seq(3, xlim[2]-20, by = step)), names(tmp))] <- 0
  tmp <- tmp[order(as.numeric(names(tmp)))]
  tmp <- t(tmp)
  rownames(tmp) <- years[i]
  f_old<- rbind(f_old, tmp)
}

# Calculate proportion of old-shelled males:
f_res <- f_res[, colnames(f_res) %in% colnames(f_old)]
p <- f_old / f_res
p <- p[, !(colnames(p) %in% vars)]
p <- p[, colnames(p) %in% as.character(50:140)]

# Plot results:
clg()
colorbar(n = 10, caption = "Proportion")
image(years, as.numeric(colnames(p)), 
      p, ylim = c(55, 130), 
      col = colorRampPalette(c("white", "black"))(100), xlab = "", ylab = "")
hline(95, lwd = 1.5, col = "red", lty = "dashed")
mtext("Year", 1, 2.5, cex = 1.25)
mtext("Carapace width (mm)", 2, 2.25, cex = 1.25)

