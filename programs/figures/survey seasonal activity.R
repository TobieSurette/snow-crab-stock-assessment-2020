library(gulf.data)
library(gulf.graphics)

years <- 1997:2020
language <- language("en")
format <- "jpg"
path <- "results/figures/"
if (language == "french") path <- paste0(path, "français/")
if (language == "english") path <- paste0(path, "english/")
if (language == "french")  file <- paste0("tableaux activité du relevé ", min(years), "-", max(years))
if (language == "english") file <- paste0("survey activity table ", min(years), "-", max(years))

# Read survey data:
s <- read.scsset(year = years, valid = 1, survey = "regular")
s$julian <- julian(date(s)) # Day of year.
res <- table(s$julian, by = year(s))

# Open output device:
gdevice(format, file = paste0(path, file), width = 8.5, height = 5)

if (language == "english") caption = "# tows" else caption = "# traits"
colorbar(seq(0, 20, by = 5), labels = c("0", "5", "10", "15", "20+"), caption = caption, smooth = TRUE, width = 0.125)
if (language == "english") {xlab <- "Month"; ylab <- "Year"} else {xlab <- "Mois";  ylab <- "Année"}

plot(c(181.5, 292), c(1996.5, 2020.5), type = "n", xlab = "", ylab = "", cex.lab = 1.35, xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n")
mtext(xlab, 1, 2.75, cex = 1.5)
mtext(ylab, 2, 2.75, cex = 1.5)

image(as.numeric(rownames(res)), years, res,
      zlim = range(res),
      breaks = 0:(length(years)-1),
      col = colorRampPalette(c("white", "black"))(length(years)-1), add = TRUE)
box()
axis(2, at = years, labels = rev(years), las = 2, cex.axis = 0.7)
for (i in 1:length(years)) lines(par("usr")[1:2], c(years[i]-0.5, years[i]-0.5), col = "grey50", lwd = 0.5)
at <- c(1, 1 + cumsum(c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30)))
for (i in 1:length(at)) lines(c(at[i]-0.5, at[i]-0.5), par("usr")[3:4], col = "grey50", lwd = 1.5)
if (language == "english") labels <- c("July", "August", "September", "October") else labels <- c("Juillet", "Ao?t", "Septembre", "Octobre")
axis(1, at = c(197.5, 228.5, 259.0, 282.5), labels = labels, cex.axis = 1)

if (format != "") dev.off()
