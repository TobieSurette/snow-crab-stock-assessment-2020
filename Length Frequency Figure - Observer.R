library(gulf)

correct <- TRUE

delete.observers <- c("CLAUDE PAQUIN", "MELANIE BELLAVANCE", "CHRISTOPHER LEVESQUE", "ERIC BOUTHILLIER", "JOSH BURKE",
                      "RYAN GRADY", "BILL ELDERSHAW", "DAISY HOLMES", "LESLIE MATTHEWS", "FEDOUSSIA L. SHEEDY", "JULIEN PARISE GAUVIN", "DANIELLE SIMARD")
delete.observers <- c(delete.observers, "DAISY HOLMES", "JEAN-FRANCOIS LAROCQUE", "JORDAN MACLEOD", "VERONIQUE DUGUAY", "ANNE NOBER", "CAMILLE DION", "MOTH TOP", "MELANIE COLETTE", "SOPHIA PERRON")
delete.observers <- unique(delete.observers)
                       
source("C:/gulf package/gulf/R/observer.scobs.R")

year <- 2019

x <- read.scobs(year = year, type = "port")
x$observer <- observer.scobs(x)

if (correct){
   x <- x[!(x$observer %in% delete.observers), ]
   x <- x[which(x$carapace.width >= 90 & x$carapace.width < 150), ]
   x <- x[which(x$chela.height.right > 10 & x$chela.height.right < 45), ]   
   x$month[which(x$trip.number == 'J18020A01')] <- 5
   x$chela.height <- chela.height(x, adjust = TRUE)
}else{
   x$chela.height <- x$chela.height.right
}
res <- summary(x)
index <- which((res$trip$number.sampled.soft / res$trip$number.sampled) >= 0.15)
delete.trips <- res$trip$trip.number[index]
res$trip <- res$trip[order(res$trip$trip.number), ]

# Do at-sea calculations:
x <- read.scobs(year = year, type = "sea")
x <- x[!(x$trip.number %in% delete.trips), ]
if (correct){
   x <- x[!(x$observer %in% delete.observers), ]
   x$zone[x$trip.number == "J18036E01"] <- "12"
   x <- x[which(x$carapace.width >= 40 & x$carapace.width < 150), ]
   x <- x[!is.na(x$chela.height.right), ]
   x <- x[which(x$chela.height.right > 6 & x$chela.height.right < 45), ]
   x <- x[setdiff(1:nrow(x), which(x$carapace.width < 60 & x$chela.height.right > 20)), ]
   x <- x[setdiff(1:nrow(x), which(x$carapace.width > 120 & x$chela.height.right < 18)), ]
   x$chela.height <- chela.height(x, adjust = TRUE)
}else{
   x$chela.height <- x$chela.height.right
}
res <- summary(x)

vars <- c("year", "trip.number", "zone", "weight", "n.trap", "number.caught", "number.sampled" , "number.sampled.soft", "proportion.soft")
res$trip[vars]

trip.number = "Q16022E01"
res$trap[res$trap$trip.number == trip.number, ]
res$trip[res$trip$trip.number == trip.number, ]
x$soft <- is.soft.scobs(x)
x[x$trip.number == trip.number, ]
x[which(x$trip.number == trip.number & x$soft), ]

# Observer length-frequencies: 
x$carapace.width <- round(x$carapace.width)
fvars <- as.character(sort(unique(x$carapace.width)))
x$individual.weight <- weight(x, units = "kg")   
   
freq.scobs <- function(x, category, fvars){
   res <- list() 
      
   # Summary trap table:
   vars <- c("year", "data.type", "trip.number", "trap.number", "zone", "weight")
   res$trap <- aggregate(list(number.sampled = x$trip.number), by = x[vars], function(x) length(x))  
   res$trap[fvars] <- 0
   index <- 1:nrow(x)
   if (!missing(category)) index <- which(is.category(x, category))
   f <- freq(x$carapace.width[index], by = x[index, vars])
   index <- match(f[vars], res$trap[vars])
   res$trap[index, fvars[fvars %in% names(f)]] <- f[fvars[fvars %in% names(f)]]
   res$trap$number.caught <- aggregate(list(x = x$male.total), by = x[vars], function(x) unique(x)[1])$x
   res$trap[fvars] <- res$trap[fvars] * repvec((res$trap$number.caught / res$trap$number.sampled), ncol = length(fvars))
   res$trap$weight.sampled <- aggregate(list(x = x$individual.weight), by = x[vars], function(x) mean(x, na.rm = TRUE) * length(x))$x
   res$trap$weight.caught <- res$trap$weight.sampled * res$trap$number.caught / res$trap$number.sampled                                                               
   res$trap <- sort(res$trap, by = c("trip.number", "trap.number"))
   
   tmp <- res$trap[!is.na(res$trap$number.caught), ]
   
   # Trip summary table:
   vars <- c("year", "data.type", "trip.number", "zone", "weight")
   res$trip <- aggregate(list(n.trap = tmp$trip.number), by = tmp[vars], length)
   res$trip$weight.caught <- aggregate(list(x = tmp$weight.caught), by = tmp[vars], sum, na.rm = TRUE)$x
   res$trip[fvars] <- aggregate(tmp[fvars], by = tmp[vars], sum, na.rm = TRUE)[fvars]
   res$trip[fvars] <- res$trip[fvars] * repvec((res$trip$weight / res$trip$weight.caught), ncol = length(fvars)) 
   res$trip <- res$trip[order(res$trip$trip.number), ]

   vars <- c("year", "zone")
   res$zone <- aggregate(list(weight = res$trip$weight), by = res$trip[vars], sum)
   res$zone[fvars] <- aggregate(res$trip[fvars], by = res$trip[vars], sum)[fvars]

   return(res)
}

res.total <- freq(x, category = "TM", fvars)
res.immature <- freq(x, category = "TMI", fvars)
res.mature <- freq(x, category = "TMM", fvars)

windows(width = 8.5, height = 11)
m <- kronecker(matrix(1:4, ncol = 1), matrix(1, ncol = 3, nrow = 3))
m <- rbind(0, cbind(0, m, 0), 0)
m <- rbind(m, 0)
layout(m)
par(mar = c(0,0,0,0))
language <- "french"
if (language == "english"){
   legend.str <- c("Adult", "Adolescent")
   zone.str <- paste0("Area ", c("12", "19", "12E", "12F"))
   xlab <- "Carapace width (mm)"
   ylab <- "Number sampled"
   zone.x <- 0.9
   lab.cex <- 1.45
}
if (language == "french"){
   legend.str <- c("Adulte", "Adolescent")
   zone.str <- paste0("Zone ", c("12", "19", "12E", "12F"))
   xlab <- "Largeur de carapace (mm)"
   ylab <- "Nombre échantillonnés"
   zone.x <- 0.9  
   lab.cex <- 1.45
}
if (language == "bilingual"){
   legend.str <- c("Adult/Adulte", "Adolescent")
   zone.str <- paste0("Area/Zone ", c("12", "19", "12E", "12F"))
   xlab <- c("Carapace width / Largeur de carapace (mm)")
   ylab <- c("Number sampled / Nombre échantillonnés")
   zone.x <- 0.8
   lab.cex <- 1.25
}
for (i in 1:nrow(res.immature$zone)){
   r <- t(rbind(res.immature$zone[i, fvars], res.mature$zone[i, fvars]))
   colnames(r) <- c("Adolecent", "Mature")
   r <- r / sum(r) # Standardize.
   
   r <- r * sum(x$zone == c("12", "19", "E", "F")[i])
   
   plot(c(60, 150), c(0, 1.15 * max(apply(r, 1, sum))), xaxs = "i", yaxs = "i", xaxt = "n", xlab = "", yaxt = "n", ylab = "")
   
   grid()
   
   dbarplot(r, width = 1, col = c("black", "grey90"), legend = FALSE, add = TRUE, lwd = 0.5, border = c("black", "grey65"))
   legend("topleft", legend = legend.str, pch = 22, pt.cex = 3, pt.bg = rev(c("black", "grey90")), cex = 1.25, bg = "white")
   
   box()
   
   lines(c(95, 95), par("usr")[3:4], lwd = 1, col = "brown3", lty = "dashed")

   if (i == 1) axis(2, las = 2)
   if (i %in% 2:4) axis(2,  las = 2)
  
   if (i == 4){
      axis(1, at = seq(60, 150, by = 20))
      axis(1, at = seq(70, 150, by = 20))
   }

   if (i == 2) mtext(ylab, 2, 3.5, at = 0, cex = lab.cex)
   
   if (i == 4) mtext(xlab, 1, 3, cex = lab.cex)  
   
   text(par("usr")[1] + zone.x * diff(par("usr")[1:2]),
        par("usr")[3] + 0.9 * diff(par("usr")[3:4]),
        zone.str[i], cex = 1.5)
   #if (i %in% c(4,8,12)){
  #    if (sex == 1) las = 2 else las = 0
  #    axis(1, at = xat[seq(1, length(xat)-1, by = 2)], las = las)
  #    axis(1, at = xat[seq(2, length(xat)-1, by = 2)], las = las)
  # }
  # if (i %in% c(16)){
  #    if (sex == 1) las = 2 else las = 0
  #    axis(1, at = xat[seq(1, length(xat), by = 2)], las = las)
   #   axis(1, at = xat[seq(2, length(xat), by = 2)], las = las)
  # }
   
   #text(par("usr")[1] + 0.85 * diff(par("usr")[1:2]), 
   #     par("usr")[3] + 0.9 * diff(par("usr")[3:4]),
   #     years[i], cex = 1.50, font = 2)
        
   #if (i == 2) mtext(expression("Density (number / km"^2*")"), 2, 3, at = 0, cex = 1.45)
   #if (i == 8) mtext("Carapace width (mm)", 1, ifelse(sex == 1, 3.5, 3), at = max(xat), cex = 1.45)  
   #box()
}

m <- aggregate(list(mu = x$carapace.width), by = x["zone"], mean, na.rm = TRUE)
m$sigma <- aggregate(list(x = x$carapace.width), by = x["zone"], sd, na.rm = TRUE)$x
m$n <- aggregate(list(x = x$carapace.width), by = x["zone"], function(x) sum(!is.na(x)))$x

index <- which(is.category(x, "TMMGE95"))
m <- aggregate(list(mu = x$carapace.width[index]), by = x[index, "zone", drop = FALSE], mean, na.rm = TRUE)
m$sigma <- aggregate(list(x = x$carapace.width[index]), by = x[index, "zone", drop = FALSE], sd, na.rm = TRUE)$x
m$n <- aggregate(list(x = x$carapace.width[index]), by = x[index, "zone", drop = FALSE], function(x) sum(!is.na(x)))$x
         






