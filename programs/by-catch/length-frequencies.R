library(gulf.data)
library(gulf.graphics)

years <- 2010:2020
species <- "plaice"
xlim <- c(0, 45)
ylim = c(0, 50)
by <- 10

clg()
m <- kronecker(matrix(1:(2*round(length(years) / 2)),ncol = 2), matrix(1, nrow = 5, ncol = 5))
m <- rbind(0, cbind(0, m, 0), 0, 0)
layout(m)
par(mar = c(0,0,0,0))

for (i in 1:length(years)){
   if (years[i] >= 2018) step <- 0.2 else step = 1
   x <- read.scsset(years[i], valid = 1, survey = "regular")
   y <- read.scslen(years[i], species = species)
   x$groundfish.sample <- is.groundfish.sample(x)
   import(x) <- freq(y, step = step, by = "tow.id")
   fvars <- names(x)[gsub("[0-9.]", "", names(x)) == ""]

   x[which(x$groundfish.sample & is.na(x[,fvars[1]])), fvars] <- 0 # Fill-in true zero observations.

   # Extract total number caught:
   z <- read.scscat(years[i], survey = "regular", species = species)
   import(x, "number.caught") <- z
   x$number.caught[is.na(x$number.caught)] <- 0

   # Number caught fix:
   n <- as.numeric(apply(x[fvars], 1, sum, na.rm = TRUE))
   ix <- which((n > 0) & (x$number.caught == 0))
   x$number.caught[ix] <- n[ix]

   # Scale catches:
   ix <- which(n > 0)
   x[ix, fvars] <- repvec(x$number.caught[ix] / n[ix], ncol = length(fvars)) * x[ix, fvars]
   x[fvars] <- x[fvars] / repvec(x$swept.area, ncol = length(fvars))
   if (step == 1) x[fvars] <- x[fvars] / 5
   r <- apply(x[x$groundfish.sample, fvars], 2, mean)  # Average length-frequency.

   gbarplot(57842.8 * r, xlim = xlim, xaxs = "i", ylim = ylim, grid = TRUE, xaxt = "n", yaxt = "n")

   if (i == 3) mtext("Population (millions)", 2, 2.5, at = 0, cex = 1.25)
   if (i == 1) axis(2)
   if (i %in% 2:6) axis(2, at = seq(0, ylim[2]-by, by = by))

   text(par("usr")[1] + 0.9 * diff(par("usr")[1:2]), par("usr")[3] + 0.8 * diff(par("usr")[3:4]), years[i], cex = 1.25)
   box()

   if (i %in% c(6, length(years))) axis(1)
}

mtext("Length(cm)", 1, 2.5, cex = 1.25)


