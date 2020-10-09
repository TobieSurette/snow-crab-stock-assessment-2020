library(gulf)

# Load survey data:
years <- 2017:2019
disaggregate <- TRUE
sex <- 1
language <- "english"
path <- "U:/Snow Crab/Stock Assessment 2019/"
comparative <- FALSE

b <- read.scbio(year = years)
b <- b[b$month >= 7, ]
s <- read.scset(year = years, valid = 1)
s <- s[s$month >= 7, ]

tmp <- s[s$year == 2019 & substr(s$tow.id,2,2) == "C", ]
s <- s[substr(s$tow.id,3,5) %in% substr(tmp$tow.id,3,5), ]
b <- b[substr(b$tow.id,3,5) %in% substr(tmp$tow.id,3,5), ]

if (comparative){
   s <- s[(s$year %in% 2017:2018) | substr(s$tow.id,2,2) == "C", ]
   b <- b[(b$year %in% 2017:2018) | substr(b$tow.id,2,2) == "C", ]
}else{
   s <- s[(s$year %in% 2017:2019) & substr(s$tow.id,2,2) != "C", ]
   b <- b[(b$year %in% 2017:2019) & substr(b$tow.id,2,2) != "C", ]
}
 
z <- read.csv("U:/Snow Crab/Stock Assessment 2019/Touchdown and Swept Area Results/GP Swept Area - Tilt 2019.csv", header = TRUE, stringsAsFactors = FALSE)
index <- match(z[c("year", "tow.id")], s[c("year", "tow.id")])
z <- z[!is.na(index),] 
index <- match(z[c("year", "tow.id")], s[c("year", "tow.id")])
s$swept.area[index] <- z$swept.area

b$cw <- round(b$carapace.width)

vars <- c("year", "tow.id")
index <- match(b[vars], s[vars])
b <- b[!is.na(index), ]
index <- match(b[vars], s[vars])
s$grid <- DFOgrid.str(longitude(s), latitude(s))
b$grid <- s$grid[index]
b$mature <- is.mature(b)

b <- b[b$carapace.width > 0 & !is.na(b$carapace.width), ]
b <- b[!is.na(b$sex), ]

windows(width = 8.5, height = 11)

#if (sex == 1) jpeg(file = "Length Frequency Males 2017-2019.jpg", width = 11 * 480, height = 8.5 * 480, res = 8.5 * 75)
#if (sex == 2) jpeg(file = "Length Frequency Females 2017-2019.jpg", width = 11 * 480, height = 8.5 * 480, res = 8.5 * 75)

m <- kronecker(matrix(1:3, ncol = 1), matrix(1, ncol = 5, nrow = 5))
m <- rbind(0, cbind(0, m, 0), 0)
m <- rbind(m, 0)
layout(m)

par(mar = c(0,0,0,0))
if (sex == 1){
   cvars <- c("TMI", "TMM")
   xlim <- c(10, 140)
   xat <- 10 * (1:14)
   yat <- 100 * (0:6)
}else{
   cvars <- c("TFI", "TFM")
   xlim <- c(10, 85)
   xat <- 10 * (1:9)
   yat <- 100 * (0:6)
}
for (i in 1:length(years)){
   print(years[i])
   index <- which(b$sex == sex & b$year == years[i])
   vars <- c("year", "tow.id", "grid")
   f <- freq(b[index, ], category = cvars, by = vars)
   f <- f[!is.na(f$year), ]
   fvars <- setdiff(names(f), c(vars, "category"))
  
   fi <- f[f$category == cvars[1], ]
   fm <- f[f$category == cvars[2], ]
    
   # Merge frequencies:
   si <- s[s$year == years[i], ]
   si[fvars] <- 0
   sm <- si
   index <- match(fi[vars], si[vars])
   si[index, fvars] <- fi[fvars]
   index <- match(fm[vars], sm[vars])
   sm[index, fvars] <- fm[fvars]
  
   # Calculate densities:
   si[, fvars] <- 1000000 * si[, fvars] / repvec(si$swept.area, ncol = length(fvars))
   sm[, fvars] <- 1000000 * sm[, fvars] / repvec(sm$swept.area, ncol = length(fvars))
   
   # Disaggregate data using 10x10 minute grid:
   if (disaggregate & (years[i] <= 2010)){
      si <- aggregate(si[, fvars], by = si[c("year", "grid")], mean) 
      sm <- aggregate(sm[, fvars], by = sm[c("year", "grid")], mean)
   }
   
   # Calculate mean densities:
   ti <- apply(si[fvars], 2, mean)
   tm <- apply(sm[fvars], 2, mean)
   
   # Combine results:
   t <- cbind(ti, tm)
   colnames(t) <- c("Immature", "Mature")
   
   plot(range(xat), range(yat), type = "n", xaxs = "i", yaxs = "i", 
        xaxt = "n", xlab = "", yaxt = "n", ylab = "", 
        xlim = xlim, ylim = c(0, max(yat)))
   
   grid()
   
   dbarplot(t, width = 1, col = c("black", "grey90"), add = TRUE, legend = FALSE, lwd = 0.5, border = c("black", "grey65"))

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
      if (i == 2) mtext(expression("Densité (nombre / km"^2*")"), 2, 3, at = mean(yat), cex = 1.45)
      if (i == 3) mtext("Largeur de carapace (mm)", 1, ifelse(sex == 1, 3.5, 3), at = mean(xat), cex = 1.45)  
   }else{
      if (i == 2) mtext(expression("Density (number / km"^2*")"), 2, 3, at = mean(yat), cex = 1.45)
      if (i == 3) mtext("Carapace width (mm)", 1, ifelse(sex == 1, 3.5, 3), at = mean(xat), cex = 1.45)    
   }
   box()
}
axis(1)

