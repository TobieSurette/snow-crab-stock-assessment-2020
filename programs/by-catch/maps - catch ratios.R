library(gulf.data)
library(gulf.graphics)
library(gulf.spatial)

format <- "pdf"
years <- 2013:2019
fish <- TRUE
if (fish){
   species <- list(40, 23, 10, 41, 201, 42, c(12, 35), 30)
   species.en <- c("American plaice", "Redfish", "Atlantic cod", "Witch flounder", "Thorny skate", "Yellowtail flounder", "White hake", "Halibut")
   species.fr <- c("Plie canadienne", "Sébaste", "Morue franche", "Plie grise", "Raie épineuse", "Limande à queue jaune", "Merluche blanche", "Flétan Atlantique")
   group <- "fish"
   max <- c(30, 50, 20, 10, 10, 5, 10, 10)
}else{
   # Invertebrates:
   species <- list(6300, 6400, 6200, c(6100, 6110:6119, 6121, 6123, 6129, 6135), c(1823, 1827), 6600, 2521, 2527)
   species.en <- c("Basket stars", "Green sea urchins", "Brittle star", "Starfish", "Sea squirts", "Sea cucumbers", "Lesser toad crab", "Greater toad crab")
   species.fr <- c("Cornes de boeuf", "Oursin vert", "Ophuires", "Étoiles de mer", "Tuniciers",  "Cocombres de mer", "Hyas coarctatus", "Hyas araneus")
   group <- "invertebrates"
   max <- c(25, 15, 20, 20, 15, 40, 5, 5)
}

# Read survey grids:
mif <- scs.survey.grids()

for(i in 1:length(species)){
   print(species[i])
   clg()
   gdevice(format, file = paste0(getwd(), "/results/figures/english/maps/by-catch/catch ratios ", species.en[i], ".", min(years),  "-", max(years)), height = 11, width = 8.5)
   m <- kronecker(matrix(1:8, ncol = 2), matrix(1, nrow = 5, ncol = 5))
   m <- rbind(0,0,cbind(0, 0, m, 0),0,0)
   layout(m)
   par(mar = c(0,0,0,0))
   vars <- c("number.caught", "weight.caught")
   for (j in 1:length(years)){
      print(years[j])
      x <- read.scsset(years[j], valid = 1, survey = "regular")
      y <- read.scscat(years[j], survey = "regular", species = species[[i]])
      y <- aggregate(y[vars], by = y[key(x)], sum)
      import(x, var = vars, fill = 0) <- y
      x$density <- x$weight.caught / x$swept.area
      if (years[j] > 2017) x$density <- 1000 * x$density # Catch density (tonnes / km2).
      x$grid <- scs.survey.grid(x)

      # Load following year:
      x2 <- read.scsset(years[j]+1, valid = 1, survey = "regular")
      y2 <- read.scscat(years[j]+1, survey = "regular", species = species[[i]])
      y2 <- aggregate(y2[vars], by = y2[key(x2)], sum)
      import(x2, var = vars, fill = 0) <- y2
      x2$density <- x2$weight.caught / x2$swept.area
      if ((years[j]+1) > 2017) x2$density <- 1000 * x2$density # Catch density (tonnes / km2).
      x2$grid <- scs.survey.grid(x2)

      xx <- x$density
      yy <- x2$density[gulf.utils::match(x$grid, x2$grid)]
      ix <- which((xx > 0) & (yy > 0))
      nn <- cbind(x$number.caught, x2$number.caught[gulf.utils::match(x$grid, x2$grid)])
      print(range(log(yy[ix]) - log(xx[ix])))
      map.new()
      cols <- colorRampPalette(c("red", "white", "blue"))(1000)
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
               bx[1:nn[ix[k],1]] <- 1
               off <- log(x$swept.area[ix[k]]/x2$swept.area[gulf.utils::match(x$grid, x2$grid)][ix[k]])
               off <- rep(off, length(bx))
               model <- glm(bx ~ 1 + offset(off), family = binomial)
               pv <- 1-coef(summary(model))[, 4] # Use p-value to scale area.
               scale <- sqrt(pv)
               mx <- mean(p[[1]]$x[1:4])
               my <- mean(p[[1]]$y[1:4])
               p[[1]]$x <- scale * (p[[1]]$x - mx) + mx
               p[[1]]$y <- scale * (p[[1]]$y - my) + my
            }
            plot(p, col = cols[round((delta + 1)/ 2 * length(cols))], border = "grey60", lwd = 0.4)
         }
      }
      map("coast", col = "floralwhite", border = "saddlebrown", lwd = 0.4)
      box()

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
             title = expression(paste("catch ratio(wt)")))
       text(-60.9, 49, paste0(years[j], "-", (years[j]+1)), cex = 1.25)
       box()

      if (j == 4){
         mtext("Longitude", 2, 2.0, cex = 1.25)
         mtext("Latitude", 1, 2.0, cex = 1.25)
         map.axis(1:2)
      }
      if (j == 5) map.axis(3:4)
   }
   dev.off()
}


