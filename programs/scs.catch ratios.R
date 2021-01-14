library(gulf.data)
library(gulf.graphics)
library(gulf.spatial)
library(glmmTMB)

format <- "pdf"
years <- 2006:2020
var <- c("FI", "FM", "MI", "COM")
ylim <- c(6000, 8000, 16000, 2500)

# Read survey grids:
mif <- scs.survey.grids()
for (i in 1:length(mif)){
   tmp <- km2deg(mif[[i]]$x, mif[[i]]$y)
   mif[[i]]$longitude <- tmp[,1]
   mif[[i]]$latitude <- tmp[,2]
}

clg()
file <- paste0(getwd(), "/results/figures/english/catch ratios/scs.catch ratios.", min(years),  "-", max(years))
gdevice(format, file = file, height = 11, width = 8.5)
k <- kronecker(matrix(1:(2*length(var)), ncol = 2, byrow = TRUE), matrix(1, ncol = 5, nrow = 5))
k <- rbind(0,cbind(0,0,k,0,0),0,0)
layout(k)
par(mar = c(0,0,0,0))
results <- list()
for (j in 1:length(var)){
   results[[j]] <- list()

   x <- read.scsset(years, valid = 1, survey = "regular")
   y <- read.scsbio(years, survey = "regular")
   y$tow.id <- tow.id(y)
   import(x, var = var[j], fill = 0) <- catch(y, category = var[j])
   x$grid <- scs.survey.grid(x)

   # Fix NA grids:
   ix <- which(is.na(x$grid))
   fun <- function(x) return(c(mean(x$longitude[1:4]), mean(x$latitude[1:4])))
   centroids <- lapply(mif, fun)
   lon <- unlist(lapply(centroids, function(x) x[1]))
   lat <- unlist(lapply(centroids, function(x) x[2]))
   d <- distance(lon(x[ix,]), lat(x[ix,]), lon, lat)
   x$grid[ix] <- apply(d, 1, which.min)

   # Define core set of survey grids:
   core <- which(apply(table(year(x), x$grid),2,sum) >= 13)

   # Prepare data for model:
   x$n <- x[, var[j]]
   x$year<- as.factor(year(x))
   x$grid <- as.factor(x$grid)
   x$year.grid <- paste0(x$year, "-", x$grid)

   # Analyze total data:
   m <- glmmTMB(n ~ year + (1|grid) + (1| year.grid) + offset(log(swept.area) - log(1000000)),
                data = x, #[x$grid %in% core, ],
                family = nbinom1)

   # Parse results:
   year_effect <- m$fit$par[1:length(years)]
   year_effect[-1] <- year_effect[1] + year_effect[-1]
   r <- data.frame(year = years,
                   mu = as.numeric(year_effect),
                   sigma = sqrt(diag(m$sdr$cov.fixed)[1:length(years)]))
   r$lower <- r$mu - 1.96 * r$sigma
   r$upper <- r$mu + 1.96 * r$sigma

   cols <- c(rep("grey90", 6), "grey70", rep("grey45", 6), "grey20", "grey20")
   gbarplot(exp(r$mu), years, xaxt = "n", grid = TRUE, ylim = c(0, ylim[j]), col = cols)
   error.bar(years, lower = exp(r$lower), upper = exp(r$upper))
   if (j == 4) axis(1, las = 2, at = years, las = 2)
   mtext(category(var[j]), 2, 2.5, cex = 1.0)
   if (j == 1) mtext("Density(#/km2)", 3, 0.5, cex = 1.0)

   # Annual differences:
   delta <- diff(r$mu)

   s <- ranef(m)$cond$year.grid
   grid_effect <- ranef(m)$cond$grid
   year <- as.numeric(lapply(strsplit(rownames(s), "-"), function(x) x[1]))
   grid <- as.numeric(lapply(strsplit(rownames(s), "-"), function(x) x[2]))
   mu <- matrix(NA, nrow = 355, ncol = length(years))
   for (i in 1:length(years)){
      ix <- intersect(as.numeric(rownames(grid_effect)), grid[year == years[i]])
      mu[ix, i] <- year_effect[i] + grid_effect[as.character(ix), 1] + s[paste0(years[i], "-", ix),1]
   }
   rownames(mu) <- 1:355
   colnames(mu) <- years

   # Store grid inferences:
   results[[j]] <- mu

   ds <- NULL
   n <- NULL
   for (i in 1:(length(years)-1)){
      ds[i] <- sd(mu[, i+1] - mu[, i], na.rm  = TRUE)
      n[i] <- sum(!is.na(mu[, i+1] - mu[, i]))
   }
   ds <- ds / sqrt(n)
   gbarplot(100*(exp(delta)-1), years[-length(years)], col = cols[-1],
            xaxt = "n", yaxt = "n", ylim = c(-60, 60), grid = TRUE, yaxs = "i")
   hline(0, col = "red")
   error.bar(years[-length(ds)], lower = 100*(exp(delta - 1.96 * ds)-1), upper = 100*(exp(delta + 1.96 * ds)-1))
   if (j == 1) axis(4) else axis(4, at = seq(-60, 40, by = 20))
   if (j == 4) axis(1, at = years[-length(years)], labels = paste0(years[-length(years)], "-", years[-1]), las = 2)
   if (j == 2) mtext("Percentage", 4, 2.75, cex = 1.0, at = -70)
   if (j == 1) mtext("Year-to-year difference (%)", 3, 0.5, cex = 1.0)
   box()
}
dev.off()

# Draw map:
clg()
for (j in 1:length(var)){
   file <- paste0(getwd(), "/results/figures/english/catch ratios/catch ratios ", var[j], ".", min(years),  "-", max(years))
   gdevice(format, file = file, height = 11, width = 8.5)
   m <- kronecker(matrix(1:15, ncol = 3), matrix(1, nrow = 5, ncol = 5))
   m <- rbind(0,0,cbind(0, 0, m, 0),0,0)
   layout(m)
   par(mar = c(0,0,0,0))
   for (i in 1:(length(years)-1)){
      mu <- results[[j]]
      d <- mu[,i+1] - mu[,i]
      d <- d[!is.na(d)]
      map.new()
      cols <- rev(colorRampPalette(c("red", "white", "blue"))(1000))
      for (k in 1:length(d)){
         ix <- as.numeric(names(d)[k])
         p <- km2deg(mif[[ix]]$x, mif[[ix]]$y)
         p <- as.polygon(p$longitude, p$latitude)

         delta <- d[k] / 1.0
         delta[delta <= -1] <- -1
         delta[delta >= 1] <- 1
         plot(p, col = cols[round((delta + 1)/ 2 * length(cols))], border = "grey60", lwd = 0.4)
      }
      map("coast", col = "floralwhite", border = "saddlebrown", lwd = 0.4)
      box()

      # Fishing zones:
      v <- read.gulf.spatial("fishing zone vertices shp", species = 2526, region = "gulf", label = c("12", "12E", "12F", "19"))
      v <- subset(v, label %in% c("12", "12E", "12F", "19"))
      plot(v, add = TRUE)

      # Draw legend:
      rr <- ((log(c(0.5, 0.66666, 0.75, 1, 1.333333, 1.5, 2)) / 1.0)+1)/2
      legend("bottomleft",
             legend = c("2:1", "3:2", "4:3", "1:1", "3:4", "2:3", "1:2"),
             pch = 22,  pt.cex = 2.5,
             pt.bg = cols[round(rr * 1000)],
             bg = "white", cex = 0.75, lwd = 0.4, col = "grey60",
             title = expression(paste("catch ratio(#)")))
      text(-61.1, 48.9, paste0(years[i], "-", (years[i]+1)), cex = 1.25)
      box()

      if (i == 5) map.axis(1:2)
      if (i == 11) map.axis(3:4)
   }
   dev.off()
}


