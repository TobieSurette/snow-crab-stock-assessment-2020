library(gulf.data)
library(gulf.graphics)
library(gulf.spatial)

# Load survey data:
year <- 2019
sex <- 1
survey <- "regular"
jpeg <- TRUE
language <- "french"
disaggregate <- FALSE  # Whether to disaggregate early survey by sampling grid.
if (sex == 1){
   category <- c("MI", "MM")
   file <- "males"
   xlim <- c(0, 140)
   ylim <- c(0, 600)
   legend <- "topright"
}else{
   category <- c("FI", "FM")
   file <- "females"
   xlim <- c(0, 90)
   ylim <- c(0, 900)
   legend <- "topleft"
}

# Load set data:
s <- read.scsset(year = year, valid = 1)
s <- s[survey(s) == survey, ]

# Load biological data:
b <- read.scsbio(year = year)
b <- b[survey(b) == survey, ]
b <- b[b$date %in% unique(s$date), ]
index <- gulf.utils::match(b[c("date", "tow.number")], s[c("date", "tow.number")])
b <- b[!is.na(index), ]
index <- gulf.utils::match(b[c("date", "tow.number")], s[c("date", "tow.number")])
b$tow.id <- s$tow.id[index]
f <- freq(b, by = c("date", "tow.id"), category = category)

res <- NULL
for (i in 1:length(category)){
   ss <- s
   import(ss, fill = 0) <- f[f$category == category[i], ]
   fvars <- names(ss)[gsub("[-0-9.]", "", names(ss)) == ""]
   ss[, fvars] <- 1000000 * ss[, fvars] / repvec(ss$swept.area, ncol = length(fvars))

   # Disaggregate data using 10x10 minute grid:
   index <- which(year(ss) <= 2010)
   if (disaggregate & (length(index) > 0)){
      tmp <- ss[index, ]
      #ss <- ss[-index, ]
      tmp$grid <- deg2grid(lon(tmp), lat(tmp))
      tmp <- aggregate(tmp[,fvars], by = ss[c("date", "grid")], mean)

      # Concatenate with ss:
      ss <- tmp
   }

   # Calculate mean densities:
   res <- rbind(res, apply(ss[fvars], 2, mean))
}
res <- t(res)
colnames(res) <- category

# Plot results:
if (jpeg){
   file <- paste0("results/figures/size-frequency/", "size-frequency ", file, " ", unique(year(s)), " - ", language, ".jpg")
   jpeg(file = file, width = 8.5 * 480, height = 8.5 * 480, res = 8.5 * 75)
}else{
   dev.new(height = 8.5, width = 8.5)
}

plot(xlim, ylim, type = "n", xaxs = "i", yaxs = "i", xaxt = "n", xlab = "", yaxt = "n", ylab = "")
grid()
gbarplot(res, width = 1, col = c("black", "grey90"), legend = FALSE, add = TRUE, lwd = 0.5, border = c("black", "grey65"))
axis(1)
axis(2)
lines(c(95, 95), par("usr")[3:4], lwd = 1, col = "brown3", lty = "dashed")
if (language == "french"){
   mtext(expression("Densité (nombre / km"^2*")"), 2, 2.0, at = par("usr")[3] + 0.5 * diff(par("usr")[3:4]), cex = 1.45)
   mtext("Largeur de carapace (mm)", 1, 2.5, at = par("usr")[1] + 0.5 * diff(par("usr")[1:2]), cex = 1.45)
}else{
   mtext(expression("Density (number / km"^2*")"), 2, 2.0, at = par("usr")[3] + 0.5 * diff(par("usr")[3:4]), cex = 1.45)
   mtext("Carapace width (mm)", 1, 2.5, at = par("usr")[1] + 0.5 * diff(par("usr")[1:2]), cex = 1.45)
}
legend(legend,
       legend = rev(category(colnames(res), language = language)),
       pch = 22,
       pt.bg = rev(c("black", "grey90")),
       col  = rev(c("black", "grey65")),
       bg = "white", pt.cex = 2, cex = 1.25)

if (jpeg) dev.off()
