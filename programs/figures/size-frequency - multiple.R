library(gulf.data)
library(gulf.graphics)
library(gulf.spatial)

# Load survey data:
years <- 2005:2020
sex <- 2
if (sex == 1) file <- "males" else file <- "females"
survey <- "regular"
jpeg <- TRUE
language <- "english"
disaggregate <- FALSE  # Whether to disaggregate early survey by sampling grid.

if (jpeg){
   file <- paste0("results/figures/size-frequency/", "size-frequency ", file, " ", min(years),"-", max(years), " - ", language, ".jpg")
   jpeg(file = file, width = 11 * 480, height = 8.5 * 480, res = 8.5 * 75)
}else{
   dev.new(height = 8.5, width = 11)
}

m <- kronecker(matrix(1:16, ncol = 4), matrix(1, ncol = 3, nrow = 3))
m <- rbind(0, cbind(0, 0, m, 0), 0, 0)
m <- rbind(m, 0)
layout(m)
par(mar = c(0,0,0,0))
if (sex == 1){
   category <- c("MI", "MM")
   xlim <- c(10, 140)
   ylim <- c(0, 500)
   xat <- 10 * (1:14)
   yat <- 100 * (0:round(ylim[2]/100))
}else{
   category <- c("FI", "FM")
   xlim <- c(10, 85)
   ylim <- c(0, 900)
   xat <- 10 * (1:9)
   yat <- 100 * (0:round(ylim[2]/100))
}

for (i in 1:length(years)){
   print(years[i])

   # Load set data:
   s <- read.scsset(year = years[i], valid = 1)
   s <- s[survey(s) == survey, ]

   # Load biological data:
   b <- read.scsbio(year = years[i])
   b <- b[survey(b) == survey, ]
   b <- b[b$date %in% unique(s$date), ]
   index <- gulf.utils::match(b[c("date", "tow.number")], s[c("date", "tow.number")])
   b <- b[!is.na(index), ]
   index <- gulf.utils::match(b[c("date", "tow.number")], s[c("date", "tow.number")])
   b$tow.id <- s$tow.id[index]

   # Length-frequency from biolgical data:
   f <- freq(b, by = c("date", "tow.id"), category = category)

   res <- NULL
   for (j in 1:length(category)){
      ss <- s
      import(ss, fill = 0) <- f[f$category == category[j], ]
      fvars <- names(ss)[gsub("[-0-9.]", "", names(ss)) == ""]
      ss[, fvars] <- 1000000 * ss[, fvars] / repvec(ss$swept.area, ncol = length(fvars))

      # Calculate mean densities:
      res <- rbind(res, apply(ss[fvars], 2, mean))
   }
   res <- t(res)
   colnames(res) <- category

   plot(xlim, ylim, type = "n", xaxs = "i", yaxs = "i", xaxt = "n", xlab = "", yaxt = "n", ylab = "")
   grid()

   gbarplot(res, width = 1, col = c("black", "grey90"), add = TRUE, legend = FALSE, lwd = 0.5, border = c("black", "grey65"))

   if (sex == 1) lines(c(95, 95), par("usr")[3:4], lwd = 1, col = "brown3", lty = "dashed")

   if (i == 1) axis(2, at = yat, las = 2)
   if (i %in% 2:4) axis(2, at = yat[1:(length(yat)-1)], las = 2)
   if (i %in% c(4,8,12)){
      if (sex == 1) las = 2 else las = 0
      axis(1, at = xat[seq(1, length(xat)-1, by = 2)], las = las)
      axis(1, at = xat[seq(2, length(xat)-1, by = 2)], las = las)
   }
   if (i %in% c(16)){
      if (sex == 1) las = 2 else las = 0
      axis(1, at = xat[seq(1, length(xat), by = 2)], las = las)
      axis(1, at = xat[seq(2, length(xat), by = 2)], las = las)
   }

   text(par("usr")[1] + 0.85 * diff(par("usr")[1:2]),
        par("usr")[3] + 0.9 * diff(par("usr")[3:4]),
        years[i], cex = 1.50, font = 2)

   if (language == "french"){
      if (i == 2) mtext(expression("Densité (nombre / km"^2*")"), 2, 3, at = 0, cex = 1.45)
      if (i == 8) mtext("Largeur de carapace (mm)", 1, ifelse(sex == 1, 3.5, 3), at = max(xat), cex = 1.45)
   }else{
      if (i == 2) mtext(expression("Density (number / km"^2*")"), 2, 3, at = 0, cex = 1.45)
      if (i == 8) mtext("Carapace width (mm)", 1, ifelse(sex == 1, 3.5, 3), at = max(xat), cex = 1.45)
   }
   box()
}

if (jpeg) dev.off()

