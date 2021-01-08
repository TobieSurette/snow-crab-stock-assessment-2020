library(gulf.data)
library(gulf.graphics)
library(gulf.spatial)
library(glmmTMB)

format <- "pdf"
years <- 2006:2019
var <- "FM"

# Read survey grids:
mif <- scs.survey.grids()

results <- NULL
clg()
file <- paste0("results/figures/english/maps/catch.ratios.", var, ".", min(years),  "-", max(years))
gdevice(format, file = file, height = 11, width = 8.5)
m <- kronecker(matrix(1:15, ncol = 3), matrix(1, nrow = 5, ncol = 5))
m <- rbind(0,0,cbind(0, 0, m, 0),0,0)
layout(m)
par(mar = c(0,0,0,0))
for (j in 1:length(years)){
   print(years[j])
   x <- read.scsset(years[j], valid = 1, survey = "regular")
   y <- read.scsbio(years[j], survey = "regular")
   y$tow.id <- tow.id(y)
   import(x, var = var, fill = 0) <- catch(y, category = var)
   x$density <- x[,var] / x$swept.area
   x$grid <- scs.survey.grid(x)

   # Load following year:
   x2 <- read.scsset(years[j]+1, valid = 1, survey = "regular")
   y2 <- read.scsbio(years[j]+1, survey = "regular")
   y2$tow.id <- tow.id(y2)
   import(x2, var = var, fill = 0) <- catch(y2, category = var)
   x$density <- x[,var] / x$swept.area
   x2$grid <- scs.survey.grid(x2)

   # Match the two data sets:
   x$swept.area2    <- x2$swept.area[gulf.utils::match(x$grid, x2$grid)]
   x$number.caught2 <- x2[gulf.utils::match(x$grid, x2$grid), var]
   nn <- cbind(x[, var], x$number.caught2)
   ss <- cbind(x$swept.area, x$swept.area2)

   # Index for data tobe used:
   ix <- which(apply(nn, 1, sum) > 0 & !is.na(apply(nn, 1, sum)))

   # Perform GLMM fitting:
   k <- nn[ix, 2]
   n <- apply(nn[ix, ], 1, sum)
   y <- k / n
   grid <- as.factor(x$grid[ix])
   off <- log(ss[ix, 2]) - log(ss[ix,1])
  # m <- glmmTMB(y ~ 1 + (1|grid) + offset(off), weights = n, family = "binomial")
   m  <- glmmTMB(cbind(k, n - k) ~ 1 + (1|grid) + offset(off), family = "binomial")

   mu <- predict(m)
   variance <- predict(m, se.fit = TRUE)$se.fit

   results <- rbind(results, coef(summary(m))$cond)

   map.new()
   cols <- colorRampPalette(c("red", "white", "blue"))(1000)
   for (k in 1:length(ix)){
      if (!is.na(x$grid[ix[k]])){
         p <- km2deg(mif[[x$grid[ix[k]]]]$x, mif[[x$grid[ix[k]]]]$y)
         p <- as.polygon(p$longitude, p$latitude)

         delta <- mu[k]
         delta <- delta / 2.5
         delta[delta <= -1] <- -1
         delta[delta >= 1] <- 1

         scale <- 1 / variance[k]
         scale[scale >= 15] <- 15
         scale <- scale / 15
         scale <- 1
         mx <- mean(p[[1]]$x[1:4])
         my <- mean(p[[1]]$y[1:4])
         p[[1]]$x <- sqrt(scale) * (p[[1]]$x - mx) + mx
         p[[1]]$y <- sqrt(scale) * (p[[1]]$y - my) + my

         plot(p, col = cols[round((delta + 1)/ 2 * length(cols))], border = "grey60", lwd = 0.4)
      }
   }
   map("coast", col = "floralwhite", border = "saddlebrown", lwd = 0.4)
   box()

   print(mean(predict(m)))

   # Fishing zones:
   v <- read.gulf.spatial("fishing zone vertices shp", species = 2526, region = "gulf", label = c("12", "12E", "12F", "19"))
   v <- subset(v, label %in% c("12", "12E", "12F", "19"))
   plot(v, add = TRUE)

   # Draw legend:
   r <- ((log(c(0.1, 0.2, 0.5, 0.66666, 1, 1.5, 2, 5, 10)) / 2.5)+1)/2
   legend("bottomleft",
          legend = c("10:1", "5:1", "2:1", "3:2", "1:1", "2:3", "1:2", "1:5", "1:10"),
          pch = 22,  pt.cex = 2.5,
          pt.bg = cols[round(r * 1000)],
          bg = "white", cex = 0.75, lwd = 0.4, col = "grey60",
          title = expression(paste("catch ratio(#)")))
   text(-61.1, 48.9, paste0(years[j], "-", (years[j]+1)), cex = 1.25)
   box()

   if (j == 5){
      mtext("Longitude", 2, 2.0, cex = 1.25)
      mtext("Latitude", 1, 2.0, cex = 1.25)
      map.axis(1:2)
   }
   if (j == 11) map.axis(3:4)

}

dev.off()

rownames(results) <- years
file <- paste0(getwd(), "/results/figures/english/maps/annual.catch.ratios.", var, ".", min(years),  "-", max(years))
gdevice(format, file = file)
gbarplot(100*(exp(results[,1])-1), years, ylim = c(-50, 50), grid = TRUE, yaxs = "i", xaxt = "n")
axis(1, at = years[seq(1, length(years), by = 2)], las = 2)
axis(1, at = years[seq(2, length(years), by = 2)], las = 2)
error.bar(years,
          lower = 100*(exp(results[,1] - 1.96 *  results[,2])-1),
          upper = 100*(exp(results[,1] + 1.96 *  results[,2])-1))
hline(0, col = "red", lwd = 1.5)
mtext("Year-to-year difference (%)", 2, 2.5, cex = 1.25)
mtext("Initial survey year", 1, 3.5, cex = 1.25)
dev.off()

print(results)

