library(gulf)

jpeg <- TRUE
language <- "french"
language <- tolower(language)

# Read survey data:
years <- 1997:2020
s <- read.scsset(year = years)
s <- s[(s$season != "summer") & (s$valid == 1), ]
s <- s[substr(s$tow.id,2,2) != "C", ]
s$julian <- julian(date(s))

res <- matrix(0, nrow = length(years), ncol = length(min(s$julian):max(s$julian)))
rownames(res) <- years
colnames(res) <- min(s$julian):max(s$julian)

for (i in 1:length(years)){
   t <- table(s$julian[s$year == years[i]])
   res[i, names(t)] <- t
}
res <- res[nrow(res):1, ]

clg()
width <- 8.5
height <- 5
ratio <- width / height
if (jpeg){
   jpeg(filename = paste0("U:/Snow Crab/Stock Assessment 2019/Survey Activity ", min(years), "-", max(years), " ", language, ".jpeg"), width = ratio * 480 * 8, height = 480 * 8, res = 75 * 8)
}else{
   windows(width = width, height = height)
}

if (language == "english") caption = "# tows" else caption = "# traits"
colorbar(seq(0, 20, by = 5), labels = c("0", "5", "10", "15", "20+"), caption = caption, smooth = TRUE, width = 0.125)
if (language == "english"){
   xlab <- "Month"; ylab <- "Year"
}else{
   xlab <- "Mois";  ylab <- "Ann?e"
}
plot(c(181.5, 292), c(1996.5, 2017.5), type = "n",
     xlab = "", ylab = "", cex.lab = 1.35,
     xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n")
mtext(xlab, 1, 2.75, cex = 1.5)
mtext(ylab, 2, 2.75, cex = 1.5)

image(as.numeric(colnames(res)), years, t(res),
      zlim = range(res),
      breaks = seq(0, 22, by = 1),
      col = colorRampPalette(c("white", "black"))(22), add = TRUE)
box()
axis(2, at = years, labels = rev(years), las = 2, cex.axis = 0.7)
for (i in 1:length(years)) lines(par("usr")[1:2], c(years[i]-0.5, years[i]-0.5), col = "grey50", lwd = 0.5)
at <- c(1, 1 + cumsum(c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30)))
for (i in 1:length(at)) lines(c(at[i]-0.5, at[i]-0.5), par("usr")[3:4], col = "grey50", lwd = 1.5)
if (language == "english") labels <- c("July", "August", "September", "October") else labels <- c("Juillet", "Ao?t", "Septembre", "Octobre")
axis(1, at = c(197.5, 228.5, 259.0, 282.5), labels = labels, cex.axis = 1)

if (jpeg) dev.off()
