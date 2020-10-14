years <- 1997:2020

b <- read.scsbio(year = years)
s <- read.scsset(year = years, valid = 1)
s <- s[substr(s$tow.id,2,2) != "C", ]
s <- s[s$month >= 7, ]
s$season <- "fall"

vars <- c("year", "tow.id")
index <- match(b[vars], s[vars])

b <- b[!is.na(index) & !is.na(b$carapace.width) & (b$carapace.width > 0) , ]
b <- b[b$sex == 2, ]

# Maturity classifications:
b$mature <- is.mature(b)
b$multiparous <- is.multiparous(b)
b$primiparous <- is.primiparous(b)
b$pubescent <- !b$mature & (b$gonad.colour == 3)
b$pubescent[is.na(b$pubescent)] <- FALSE

# Calculate summary statistics:
vars <- c("mature", "primiparous", "multiparous", "pubescent")
res <- list()
for (i in 1:length(vars)){
   res[[i]] <- aggregate(list(mu = b$carapace.width * c(NA, 1)[b[, vars[i]] + 1]), by = b["year"], mean, na.rm = TRUE)
   res[[i]]$n <- aggregate(list(n = b$carapace.width * c(NA, 1)[b[, vars[i]] + 1]), by = b["year"], function(x) sum(!is.na(x)))$n
   res[[i]]$sigma <- aggregate(list(n = b$carapace.width * c(NA, 1)[b[, vars[i]] + 1]), by = b["year"], sd, na.rm = TRUE)$n
   res[[i]]$lci <- res[[i]]$mu - 1.96 * res[[i]]$sigma / sqrt(res[[i]]$n)
   res[[i]]$uci <- res[[i]]$mu + 1.96 * res[[i]]$sigma / sqrt(res[[i]]$n)
}
names(res) <- vars

# Plot female mean sizes:
plot(range(res[[i]]$year), c(40, 70), ylim = c(40, 70), type = "n", xlab = "Year", ylab = "Mean size (mm)", cex.lab = 1.5, xaxt = "n", yaxs = "i")
grid()
cols <- c("black", "blue", "red", "green")
lty <- c("solid", "dashed", "dotted", "3564")
for (i in 1:length(res)){
   lines(res[[i]]$year, res[[i]]$mu, lwd = 2, col = cols[i], lty = lty[i])
   for (j in 1:length(res[[i]]$year)){
      lines(rep(res[[i]]$year[j], 2), c(res[[i]]$lci[j], res[[i]]$uci[j]), lwd = 1.5, col = cols[i], lty = "solid")
      lines(c(res[[i]]$year[j]-0.15, res[[i]]$year[j]+0.15), rep(res[[i]]$lci[j], 2), lwd = 1.5, col = cols[i], lty = "solid")
      lines(c(res[[i]]$year[j]-0.15, res[[i]]$year[j]+0.15), rep(res[[i]]$uci[j], 2), lwd = 1.5, col = cols[i], lty = "solid")
   }
}
axis(1, at = seq(1997, 2018, by = 4))
axis(1, at = seq(1999, 2018, by = 4))
legend("topright",
       legend = c("Mature", "Multiparous", "Primiparous", "Pubescent"),
       col = cols,
       lwd = 2, lty = lty,
       bg = "white", cex = 1.25)
box()



