library(gulf.data)
library(gulf.graphics)
library(gulf.spatial)

format <- "pdf"
year <- 2018
language = language("en")
category <- "MGE95" #"FM" #  "MGE35LE95" #"MGE95" #"COM" # "MGE35LE95" #"FI" # "MIGE35LE95"

# Read survey grids:
mif <- scs.survey.grids()
for (i in 1:length(mif)){
   t <- km2deg(mif[[i]]$x, mif[[i]]$y)
   mif[[i]]$longitude <- t[,1]
   mif[[i]]$latitude <- t[,2]
}

clg()

# Load data:
x <- read.scsset(year:(year+1), valid = 1, survey = "regular")
y <- read.scsbio(year:(year+1), survey = "regular")
y$tow.id <- tow.id(y)
import(x, var = category, fill = 0) <- catch(y, category = category)
x$grid <- scs.survey.grid(x)
x$n <- x[, category]

# Fix NA grids:
ix <- which(is.na(x$grid))
if (length(ix) > 0){
   fun <- function(x) return(c(mean(x$longitude[1:4]), mean(x$latitude[1:4])))
   centroids <- lapply(mif, fun)
   lon <- unlist(lapply(centroids, function(x) x[1]))
   lat <- unlist(lapply(centroids, function(x) x[2]))
   d <- distance(lon(x[ix,]), lat(x[ix,]), lon, lat)
   x$grid[ix] <- apply(d, 1, which.min)
}

comparative <- sort(scs.survey.grid(read.scsset(2019, survey = "comparative", valid = 1))) # Comparative survey grid.
#x <- x[x$grid %in% comparative, ]

# Separate by year:
x2 <- x[year(x) == (year+1), ]
x <- x[year(x) == year, ]

xx <- x$n
yy <- x2$n[gulf.utils::match(x$grid, x2$grid)]
ix <- which((xx > 0) & (yy > 0))
nn <- cbind(xx, yy)
ss <- cbind(x$swept.area, x2$swept.area[gulf.utils::match(x$grid, x2$grid)])

print(range(log(yy[ix]) - log(xx[ix])))
map.new()
cols <- colorRampPalette(c("blue", "white", "red"))(1000)
for (k in 1:length(ix)){
   if (!is.na(x$grid[ix[k]])){
      p <- km2deg(mif[[x$grid[ix[k]]]]$x, mif[[x$grid[ix[k]]]]$y)
      p <- as.polygon(p$longitude, p$latitude)
      delta <- log(yy[ix[k]]) - log(xx[ix[k]])
      delta <- delta / 2.5
      delta[delta <= -1] <- -1
      delta[delta >= 1] <- 1

      if (!is.na(sum(nn[ix[k], ]))){
         bx <- rep(0, sum(nn[ix[k],]))
         bx[1:nn[ix[k],2]] <- 1

         off <- log(ss[ix[k],2]) - log(ss[ix[k],1])
         off <- rep(off, length(bx))
         model <- glm(bx ~ 1 + offset(off), family = binomial)
         pv <- 1-coef(summary(model))[, 4] # Use p-value to scale area.
         print(length(bx))
         print(pv)

         scale <- sqrt(pv)
         scale <- sqrt(min(10, length(bx)) / 10)
         mx <- mean(p[[1]]$x[1:4])
         my <- mean(p[[1]]$y[1:4])
         p[[1]]$x <- scale * (p[[1]]$x - mx) + mx
         p[[1]]$y <- scale * (p[[1]]$y - my) + my
         plot(p, col = cols[round((delta + 1)/ 2 * length(cols))], border = "grey60", lwd = 0.4)
      }
   }
}
map("coast", col = "grey90", border = "grey70", lwd = 0.4)
box()

# Fishing zones:
v <- read.gulf.spatial("fishing zone vertices shp", species = 2526, region = "gulf", label = c("12", "12E", "12F", "19"))
v <- subset(v, label %in% c("12", "12E", "12F", "19"))
plot(v, add = TRUE)

# Draw legend:
r <- ((log(c(0.2, 0.5, 0.66666, 1, 1.5, 2, 5)) / 2.5)+1)/2
legend("bottomleft",
       legend = c("5:1", "2:1", "3:2", "1:1", "2:3", "1:2", "1:5"),
       pch = 22,  pt.cex = 2.5,
       pt.bg = cols[round(r * 1000)],
       bg = "white", cex = 0.75, lwd = 0.4, col = "grey60",
       title = ifelse(language == "french", expression(paste("Rapport des captures(#)")), "Catch ratio(#)"))
text(-60.9, 49, paste0(year, "-", (year+1)), cex = 1.25)
box()
map.axis(1:2)

mtext("Longitude", 2, 2.0, cex = 1.25)
mtext("Latitude", 1, 2.0, cex = 1.25)
mtext(category(category, language = language), 3, 0.5, cex = 1.25)


