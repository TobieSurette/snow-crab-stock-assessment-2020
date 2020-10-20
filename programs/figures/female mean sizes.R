library(gulf.data)

years <- 1997:2020
language <- "french"
jpeg <- TRUE

res <- NULL
vars <- c("mature", "primiparous", "multiparous", "pubescent")
for (i in 1:length(years)){
   print(years[i])
   # Read biological data:
   b <- read.scsbio(year = years[i], sex = 2)
   b <- b[survey(b) == "regular", ]

   # Maturity classifications:
   b$mature <- is.mature(b)
   b$multiparous <- is.multiparous(b)
   b$primiparous <- !b$multiparous
   b$pubescent <- !b$mature & (b$gonad.colour == 3)
   b$pubescent[is.na(b$pubescent)] <- FALSE

   # Calculate summary statistics:
   for (j in 1:length(vars)){
      index <- which(b[, vars[j]] & !is.na(b$carapace.width))
      tmp <- data.frame(year = years[i],
                        maturity = vars[j],
                        mu = mean(b$carapace.width[index]),
                        n = length(index),
                        sigma = sd(b$carapace.width[index]),
                        stringsAsFactors = FALSE)
      tmp$lci <- tmp$mu - 1.96 * tmp$sigma / sqrt(tmp$n)
      tmp$uci <- tmp$mu + 1.96 * tmp$sigma / sqrt(tmp$n)
      res <- rbind(res, tmp)
   }
}

xlab <- "Year"
ylab <- "Carapace width (mm)"
legend <- vars
if (language == "french"){
   xlab <- "Année"
   ylab <- "Largeur de carapace (mm)"
   legend = c("mature", "primipare", "multipare", "pubère")
}

# Plot results:
if (jpeg){
   file <- paste0("results/figures/mean sizes/", "female mean sizes ", min(years),"-", max(years), " - ", language, ".jpg")
   jpeg(file = file, width = 8.5 * 480, height = 8.5 * 480, res = 8.5 * 75)
}else{
   dev.new(height = 8.5, width = 8.5)
}

# Plot female mean sizes:
plot(range(res$year), c(40, 70), type = "n", xlab = xlab, ylab = ylab, cex.lab = 1.5)
grid()
cols <- c("black", "blue", "red", "green")
lty <- c("solid", "dashed", "dotted", "3564")
for (i in 1:length(vars)){
   r <- res[res$maturity == vars[i], ]
   lines(r$year, r$mu, lwd = 2, col = cols[i], lty = lty[i])
   for (j in 1:length(r$year)){
       lines(rep(r$year[j], 2), c(r$lci[j], r$uci[j]), lwd = 1.5, col = cols[i], lty = "solid")
       lines(c(r$year[j]-0.15, r$year[j]+0.15), rep(r$lci[j], 2), lwd = 1.5, col = cols[i], lty = "solid")
       lines(c(r$year[j]-0.15, r$year[j]+0.15), rep(r$uci[j], 2), lwd = 1.5, col = cols[i], lty = "solid")
   }
}
legend("topright",
       legend = legend,
       col = cols,
       lwd = 2, lty = lty,
       bg = "white", cex = 1.25)

if (jpeg) dev.off()


