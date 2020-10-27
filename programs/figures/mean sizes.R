library(gulf.data)

years <- 1997:2020
language <- "english"
jpeg <- TRUE
tables <- TRUE
sex <- 2
var.str <- ifelse(language == "french", "tailles moyennes", "mean sizes")
language.str <- ifelse(language == "french", "français", "english")
if (sex == 1) if (language == "english") sex.str <- "males" else sex.str <- "mâles"
if (sex == 2) if (language == "english") sex.str <- "females" else sex.str <- "femelles"
file.name <- paste0(var.str, " ", sex.str, " ", min(years),"-", max(years))
path.name <- paste0("/", language.str, "/", var.str, "/")

res <- NULL
if (sex == 2) vars <- c("mature", "primiparous", "multiparous", "pubescent")
if (sex == 1) vars <- c("mature", "commercial", "immature")
for (i in 1:length(years)){
   print(years[i])
   # Read biological data:
   b <- read.scsbio(year = years[i], sex = sex)
   b <- b[survey(b) == "regular", ]

   b$mature <- is.mature(b)
   if (sex == 1){
      # Maturity classifications:
      b$commercial <- is.category(b, "COM")
      b$immature <- !b$mature
   }
   if (sex == 2){
      # Maturity classifications:
      b$multiparous <- is.multiparous(b)
      b$primiparous <- is.primiparous(b)
      b$pubescent <- !b$mature & (b$gonad.colour == 3)
      b$pubescent[is.na(b$pubescent)] <- FALSE
   }

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
if (sex == 1) ylim <- c(20, 120)
if (sex == 2) ylim <- c(40, 70)
if (language == "french"){
   xlab <- "Année"
   ylab <- "Largeur de carapace (mm)"
   legend <- gsub("commercial", "commerciaux", legend)
   legend <- gsub("primiparous", "primipare", legend)
   legend <- gsub("multiparous", "multipare", legend)
   legend <- gsub("pubescent", "pubère", legend)
}

# Plot results:
if (jpeg){
   path <- paste0("results/figures", path.name)
   if (file.exists(path)) dir.create(path)
   jpeg(file = paste0(path, file.name, ".jpg"), width = 8.5 * 480, height = 8.5 * 480, res = 8.5 * 75)
}else{
   dev.new(height = 8.5, width = 8.5)
}

# Plot female mean sizes:
plot(range(res$year), ylim, type = "n", xlab = "", ylab = "", cex.lab = 1.5, yaxs = "i")
grid()
cols <- c("black", "blue", "red", "green")[1:length(vars)]
lty <- c("solid", "dashed", "dotted", "3564")[1:length(vars)]
for (i in 1:length(vars)){
   r <- res[res$maturity == vars[i], ]
   lines(r$year, r$mu, lwd = 2, col = cols[i], lty = lty[i])
   for (j in 1:length(r$year)){
       lines(rep(r$year[j], 2), c(r$lci[j], r$uci[j]), lwd = 1.5, col = cols[i], lty = "solid")
       lines(c(r$year[j]-0.15, r$year[j]+0.15), rep(r$lci[j], 2), lwd = 1.5, col = cols[i], lty = "solid")
       lines(c(r$year[j]-0.15, r$year[j]+0.15), rep(r$uci[j], 2), lwd = 1.5, col = cols[i], lty = "solid")
   }
}
legend(ifelse(sex == 2, "topright", "bottomright"),
       legend = legend,
       col = cols, lwd = 2, lty = lty,
       bg = "white", cex = 1.25)

mtext(xlab, 1, 2.5, cex = 1.25)
mtext(ylab, 2, 2.5, cex = 1.25)

box()

if (jpeg) dev.off()

# Save tables:
res <- sort(res, by = c("maturity", "year"))
res[c("mu", "sigma", "lci", "uci")] <- round(res[c("mu", "sigma", "lci", "uci")], 1)
if (language == "french"){
   names <- names(res)
   names <- gsub("year", "annee", names)
   names <- gsub("maturity", "maturite", names)
   names <- gsub("lci", "int.conf.bas", names)
   names <- gsub("uci", "int.conf.haut", names)
   names(res) <- names
}

if (tables){
   path <- paste0("results/tables/", language.str, "/", path.name)
   if (file.exists(path)) dir.create(path)
   write.csv(res, file = paste0(path, file.name, ".csv"), row.names = FALSE)
}

