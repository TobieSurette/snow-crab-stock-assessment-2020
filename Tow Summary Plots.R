library(gulf)

source("C:/gulf package/gulf/R/read.flowmeter.R")

year <- 2019
comparative <- TRUE
acceleration <- FALSE
flowmeter <- TRUE
magnetic <- TRUE

pdf <- FALSE
thin = 10

# Read tow data:
x <- read.scset(year = year, valid = 1)

# Convenience functions:
hline <- function(x, col = "red", lty = "solid", ...) for (i in 1:length(x)) lines(par("usr")[1:2], c(x[i], x[i]), lty = lty, col = col, ...)
vline <- function(x, col = "red", lty = "solid", ...) for (i in 1:length(x)) lines(c(x[i], x[i]), par("usr")[3:4], lty = lty, col = col, ...)

# Load touchdown and liftoff estimates:
if (year == 2019){
   headline <- read.csv(file = paste0("U:/Snow Crab/Stock Assessment 2019/Touchdown and Swept Area Results/Touchdown and Liftoff - Headline ", year, ".csv"), header = TRUE, stringsAsFactors = FALSE)
}else{
   headline <- x[c("year", "tow.id")]
   headline$touchdown <- x$start.time
   headline$liftoff <- "        "
   headline <- headline[x$start.time.logbook != x$start.time , ]
}
tilt <- read.csv(file = paste0("U:/Snow Crab/Stock Assessment 2019/Touchdown and Swept Area Results/Touchdown and Liftoff - Tilt ", year, ".csv"), header = TRUE, stringsAsFactors = FALSE)

# Define tows to be generated:
tows <- x$tow.id

# Functions and definitions:
hline <- function(x, col = "red", lty = "solid", ...) for (i in 1:length(x)) lines(par("usr")[1:2], c(x[i], x[i]), lty = lty, col = col, ...)
vline <- function(x, col = "red", lty = "solid", ...) for (i in 1:length(x)) lines(c(x[i], x[i]), par("usr")[3:4], lty = lty, col = col, ...)
months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")  

suffix <- function(n){
   suffixes <- c("th", "st", "nd", "rd", "th", "th", "th", "th", "th", "th")
   if (n %in% 11:20) return("th")
   if (n %in% c(1:10, 21:31)) return(suffixes[(n %% 10) + 1]) 
   return("")
}
   
clg()
for (i in 320){
   tow.id <- tows[i]
   
   # Read probe data:
   m <- read.minilog(year = year, tow.id = tow.id)
   e <- read.esonar(year = year, tow.id = tow.id)
   s <- read.star.oddi(year = year, tow.id = tow.id)
   t <- read.star.oddi(year = year, tow.id = tow.id, type = "tilt")
   if (flowmeter) fdata <- read.flowmeter(year = year, tow.id = tow.id) else fdata <- NULL
   if (acceleration) a <- read.star.oddi(year = year, tow.id = tow.id, type = "acceleration") else a <- NULL
   
   # Add 'tow.id' field:
   if (length(m) > 0) if (!("tow.id" %in% names(m))) m$tow.id <- tow.id
   if (length(e) > 0) if (!("tow.id" %in% names(e))) e$tow.id <- tow.id
   if (length(s) > 0) if (!("tow.id" %in% names(s))) s$tow.id <- tow.id
   if (length(t) > 0) if (!("tow.id" %in% names(t))) t$tow.id <- tow.id
                                                        
   # Define graphical layout:
   k <- 6
   if (magnetic & (length(t) > 0)){
      if (any(c("comp.head", "comp.4p", "inclination") %in% names(t))) k <- k + 1 else magnetic <- FALSE
   }
   if (flowmeter & (length(fdata) > 0)) k <- k + 1 else flowmeter <- FALSE
   if (acceleration & (length(a) > 0)) k <- k + 1 else acceleration <- FALSE

   f <- kronecker(matrix(1:2, nrow = 1), matrix(1, nrow = 8, ncol = 5))
   f <- rbind(f, 0)
   f <- rbind(f, kronecker(matrix(3:4, ncol = 1), matrix(1, nrow = 4, ncol = 10)))
   f <- rbind(f, kronecker(matrix(5:k, ncol = 1), matrix(1, nrow = 5, ncol = 10))) 
   f <- rbind(0, cbind(0, f, 0), 0, 0)
           
   if (!pdf){ 
      windows(width = 8.5, height = 11) 
   }else{ 
     file <- paste0("U:/Snow Crab/Stock Assessment 2019/Tow Summaries/", year, "/", tow.id, ".pdf")
     pdf(file, width = 8.5, height = 11)
   }   
   
   layout(f)
   par(mar = c(3, 2, 2, 2))   # c(bottom, left, top, right)  c(5, 4, 4, 2) + 0.1.
      
   if (!is.null(m)) if (nrow(m) == 0) m <- NULL
   if (!is.null(e)) if (nrow(e) == 0) e <- NULL
   if (!is.null(s)) if (nrow(s) == 0) s <- NULL
   if (!is.null(t)) if (nrow(t) == 0) t <- NULL
      
   # Define start time:
   start.time <- time.default(paste0(date(x[i,]), " ", x$start.time.logbook[x$tow.id == tow.id], " AST"))
   
   # Define time in minutes from reference start time:
   if (length(m) > 0) m$time <- time2min(time(m), start.time)
   if (length(e) > 0) e$time <- time2min(time(e), start.time)
   if (length(s) > 0) s$time <- time2min(time(s), start.time)
   if (length(t) > 0) t$time <- time2min(time(t), start.time)
   if (length(a) > 0) a$time <- time2min(time(a), start.time)
   if (length(fdata) > 0) fdata$time <- time2min(time(fdata), start.time)
   
   #if (year == 2019 & tow.id == "GP216F"){
   #   if (length(t) > 0) t$time <- t$time + 5
   #   if (length(a) > 0) a$time <- t$time + 5
   #}
   
   # Define set of touchdown and    
   times <- c(start.time = 0,                   
              end.time = time2min(time.default(paste0(date(x[i,]), " ", x$end.time.logbook[x$tow.id == tow.id], " AST")), start.time),
              touchdown.headline = time2min(time.default(paste0(date(x[i,]), " ", headline$touchdown[headline$tow.id == tow.id], " AST")), start.time),     
              touchdown.tilt = time2min(time.default(paste0(date(x[i,]), " ", tilt$touchdown[tilt$tow.id == tow.id], " AST")), start.time),
              liftoff.headline = time2min(time.default(paste0(date(x[i,]), " ", headline$liftoff[headline$tow.id == tow.id], " AST")), start.time),
              liftoff.tilt = time2min(time.default(paste0(date(x[i,]), " ", tilt$liftoff[tilt$tow.id == tow.id], " AST")), start.time))
   times[abs(times) > 20] <- NA
   
   # Rescale depth and pressure:
   depth <- mean(-depth(e$longitude[e$time > 0 & e$time < 5], e$latitude[e$time > 0 & e$time < 5]))
   tmp <- median(e$depth[e$time > 0 & e$time < 5], na.rm = TRUE)
   if (!is.na(tmp)) depth <- tmp 
   
   if (length(m) > 0) m$depth <- depth * (m$depth / mean(m$depth[m$time > 0 & m$time < 5]))
   if (length(s) > 0) s$depth <- depth * (s$pressure / mean(s$pressure[s$time > 0 & s$time < 5]))
   if (length(t) > 0) t$depth <- depth * (t$pressure / mean(t$pressure[t$time > 0 & t$time < 5]))

   # Define plot interval:
   xlim <- c(times[["touchdown.tilt"]]-2, times[["liftoff.tilt"]] + 2)
        
   # Display photo:
   par(mar = c(1, 1, 1, 2))   # c(bottom, left, top, right)  c(5, 4, 4, 2) + 0.1.
   p <- read.scphoto(tow.id = tow.id, year = year) 
   if (!is.null(p)) p <- p[seq(1, nrow(p), by = thin), seq(1, ncol(p), by = thin), ]
   plot(c(0, 1), c(0, 1), type = "n", xlab = "", xaxt = "n", ylab = "", yaxt = "n", xaxs = "i", yaxs = "i")  # Thin-out image:
   if (length(p) > 0) rasterImage(p, 0, 0, 1, 1)
   box()
   tow.number <- x$tow.number[x$tow.id == tow.id]
   str <- paste0("Tow # ", tow.number, ", ", months[x$month[x$tow.id == tow.id]], " ")
   day <- as.character(x$day[x$tow.id == tow.id])
   str <- paste0(str, day, suffix(x$day[x$tow.id == tow.id]), ", ", x$year[x$tow.id == tow.id])
   str <- paste0(str, " ('", tow.id, "')")
      
   mtext(str, 3, 1.15, at = 1.08, adj = 0.5, cex = 1.5, font = 2)
      
   par(mar = c(3, 2, 2, 2))   # c(bottom, left, top, right)  c(5, 4, 4, 2) + 0.1.
        
   # Plot trawl path:      
   if (length(e) > 0){
      tmp <- deg2km(e$longitude, e$latitude)
      e$xkm <- 1000 * (tmp$x - tmp$x[which.min(abs(e$time - 0))])
      e$ykm <- 1000 * (tmp$y - tmp$y[which.min(abs(e$time - 0))])    
      
      index <- e$time >= xlim[1] & e$time <= xlim[2] 
      dim <- c(min(e$xkm[index], e$ykm[index]), max(e$xkm[index], e$ykm[index]))
      dim[1] <- dim[1] - 0.1*diff(dim)
      dim[2] <- dim[2] + 0.1*diff(dim)
      plot(dim, dim, type = "n", xlab = "", ylab = "", xaxs = "i", yaxs = "i")
      grid()
      
      lines(e$xkm[e$time >= times[["touchdown.tilt"]] & e$time <= times[["start.time"]]], e$ykm[e$time >= times[["touchdown.tilt"]] & e$time <= times[["start.time"]]], lwd = 2, col = "red")
      lines(e$xkm[e$time >= times[["end.time"]] & e$time <= times[["liftoff.tilt"]]], e$ykm[e$time >= times[["end.time"]] & e$time <= times[["liftoff.tilt"]]], lwd = 2, col = "red")
      lines(e$xkm[e$time >= times[["start.time"]] & e$time <= times[["end.time"]]], e$ykm[e$time >= times[["start.time"]] & e$time <= times[["end.time"]]], lwd = 2, col = "blue")
      mtext("x (meters)", 1, 2.25, cex = 0.8)
      mtext("y (meters)", 2, 2.25, cex = 0.8)
      location = "topleft"
      text(e$xkm[which.min(abs(e$time - times[["start.time"]]))], e$ykm[which.min(abs(e$time - times[["start.time"]]))], "start", pos = 2)
      text(e$xkm[which.min(abs(e$time - times[["end.time"]]))], e$ykm[which.min(abs(e$time - times[["end.time"]]))], "end", pos = 2)
      points(e$xkm[which.min(abs(e$time - times[["start.time"]]))], e$ykm[which.min(abs(e$time - times[["start.time"]]))], pch = 21, bg = "blue", cex = 1.0)
      points(e$xkm[which.min(abs(e$time - times[["start.time"]]))], e$ykm[which.min(abs(e$time - times[["start.time"]]))], pch = 21, bg = "blue", cex = 1.0)
      points(e$xkm[which.min(abs(e$time - times[["touchdown.tilt"]]))], e$ykm[which.min(abs(e$time - times[["touchdown.tilt"]]))], pch = 21, bg = "red", cex = 1.0)
      points(e$xkm[which.min(abs(e$time - times[["liftoff.tilt"]]))], e$ykm[which.min(abs(e$time - times[["liftoff.tilt"]]))], pch = 21, bg = "red", cex = 1.0)
      
      # Speed and heading plot:       
      par(mar = c(0, 2, 0, 0))   # c(bottom, left, top, right)
      plot(e$time, e$speed, xlim = xlim, ylim = c(0, 3), type = "l", lwd = 2, xaxs = "i", yaxs = "i", xaxt = "n")
      grid()
      mtext("Speed (knots)", 2, 2.25, cex = 0.8)
      vline(times[c("start.time", "end.time")], lwd = 2, col = "blue", lty = "dotted")         # Logbook.
      vline(times[c("touchdown.tilt", "liftoff.tilt")], lwd = 2, col = "green", lty = "solid") # Tilt.
      vline(times[c("touchdown.headline")], lwd = 2, col = "red", lty = "dashed")              # Headline.
      
      legend("topleft", legend = c("logbook", "tilt", "headline"), col = c("blue", "green", "red"), lwd = 2, lty = c("dotted", "solid", "dashed"), bg = "white")
      box()
   }
      
   if (length(e) > 0){             
      par(mar = c(0, 2, 0, 0))   # c(bottom, left, top, right)
      plot(e$time, e$doormaster, xlim = xlim, ylim = c(0, 15), pch = 21, bg = "grey", xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n")
      grid()
      #if (any(colnames(mu) == tow.id)) lines(as.numeric(rownames(mu)) / 60, mu[,colnames(mu) == tow.id], col = "blue", lwd = 2)
      axis(2, at = c(0, 10, by = 5))

      vline(times[c("start.time", "end.time")], lwd = 2, col = "blue", lty = "dotted")         # Logbook.
      vline(times[c("touchdown.tilt", "liftoff.tilt")], lwd = 2, col = "green", lty = "solid") # Tilt.
      vline(times[c("touchdown.headline")], lwd = 2, col = "red", lty = "dashed")              # Headline.
      mtext("Wings (m)", 2, 2.25, cex = 0.8) 
      #if (any(colnames(mu) == tow.id)) legend("topleft", legend = c("eSonar", "Model"), pch = c(21, NA), pt.bg = c("grey", NA), lwd = c(1, 2), col = c("black", "blue"), bg = "white")
       
      box()    
   }
                         
   par(mar = c(0, 2, 0, 0))  
   plot(xlim, c(-max(0, depth-40), -depth - 10), type = "n", lwd = 2, xaxt = "n", xaxs = "i")
   grid() 

   if (length(m) > 0) lines(m$time, -m$depth, pch = 21, col = "black", lwd = 2)
   if (length(s) > 0) lines(s$time, -s$depth, pch = 21, col = "red", lwd = 2)
   if (length(t) > 0) lines(t$time, -t$depth, pch = 21, col = "purple", lwd = 2)
   if (length(e) > 0) points(e$time, -e$depth, pch = 21, bg = "green", cex = 1.25)
      vline(times[c("start.time", "end.time")], lwd = 2, col = "blue", lty = "dotted")         # Logbook.
      vline(times[c("touchdown.tilt", "liftoff.tilt")], lwd = 2, col = "green", lty = "solid") # Tilt.
      vline(times[c("touchdown.headline")], lwd = 2, col = "red", lty = "dashed")              # Headline.
          
   mtext("Depth (m)", 2, 2.25, cex = 0.8)   
   legend("top", horiz = TRUE,
          legend = c("Minilog", "Star Oddi Headline", "Star Oddi Footrope", "E-Sonar"), 
          col = c("black", "red", "purple", "green"), lwd = c(2, 2, 2, 2), bg = "white",
          cex = 0.8)  
   box()    
   
   # Tilt angles plot:
   if (length(t) > 0){
      index <- (t$time >= xlim[1]) & (t$time <= xlim[2])
      plot(t$time[index], t$tilt.x[index], type = "l", lwd = 1, col = "blue", xlim = xlim, xaxs = "i", xaxt = "n")
      grid()
      mtext("Tilt Angle (°)", 2, 2.25, cex = 0.8)
      vline(times[c("start.time", "end.time")], lwd = 2, col = "blue", lty = "dotted")         # Logbook.
      vline(times[c("touchdown.tilt", "liftoff.tilt")], lwd = 2, col = "green", lty = "solid") # Tilt.
      vline(times[c("touchdown.headline")], lwd = 2, col = "red", lty = "dashed")              # Headline.
      hline(0, lwd = 2, lty = "dashed")
      box()      
   }
   
   if (acceleration & (length(t) > 0)){
      index <- (a$time >= xlim[1]) & (a$time <= xlim[2])
      plot(a$time[index], a$Xacc[index], type = "l", lwd = 1, col = "blue", xlim = xlim, xaxs = "i", xaxt = "n")
      grid()
      mtext("Accleration (m/s^2)", 2, 2.25, cex = 0.8)
      vline(times[c("start.time", "end.time")], lwd = 2, col = "blue", lty = "dotted")         # Logbook.
      vline(times[c("touchdown.tilt", "liftoff.tilt")], lwd = 2, col = "green", lty = "solid") # Tilt.
      vline(times[c("touchdown.headline")], lwd = 2, col = "red", lty = "dashed")              # Headline.
      box()       
   }   
   
   if (magnetic & (length(t) > 0)){
      vars <- c("comp.head", "comp.4p", "inclination")
      ylim <- range(t[t$time >= 0 & t$time <= 5, vars])
      plot(xlim, ylim, type = "n", xaxs = "i", xaxt = "n")
      grid()
      if ("comp.head" %in% names(t)) lines(t$time, t$comp.head, col = "blue", lwd = 1)
      #if ("comp.4p" %in% names(t)) lines(t$time, t$comp.4p, col = "green", lwd = 1)
      if ("inclination" %in% names(t)) lines(t$time, t$inclination, col = "red", lwd = 1)
      mtext("Tilt Heading (°)", 2, 2.25, cex = 0.8) 
      vline(times[c("start.time", "end.time")], lwd = 2, col = "blue", lty = "dotted")         # Logbook.
      vline(times[c("touchdown.tilt", "liftoff.tilt")], lwd = 2, col = "green", lty = "solid") # Tilt.
      vline(times[c("touchdown.headline")], lwd = 2, col = "red", lty = "dashed")              # Headline. 
      if (flowmeter){
      
      }
      legend("bottom", horiz = TRUE,
             legend = c("Heading", "Inclination"), 
             col = c("blue", "red"), lwd = c(2, 2, 2, 2), bg = "white", cex = 0.8)  
             
      box()
   }
   
   if (flowmeter){
      vars <- c("velocity.east", "velocity.north", "velocity.up", "Speed.(m/s)")
      ylim <- range(fdata[fdata$time >= 0 & fdata$time <= 5, vars])
      plot(xlim, ylim, type = "n", xaxs = "i", ylim = c(-1.75, 2.0), yaxs = "i", xaxt = "n", yaxt = "n")
      grid()
      if ("velocity.east" %in% names(fdata)) lines(fdata$time, fdata$velocity.east, col = "blue", lwd = 1)
      if ("velocity.north" %in% names(fdata)) lines(fdata$time, fdata$velocity.north, col = "green", lwd = 1)
      if ("velocity.up" %in% names(fdata)) lines(fdata$time, fdata$velocity.up, col = "red", lwd = 1)
      if ("Speed.(m/s)" %in% names(fdata)) lines(fdata$time, fdata$"Speed.(m/s)", col = "black", lwd = 2)
      mtext("Flowmeter (m/s)", 2, 2.25, cex = 0.8) 
      axis(2, at = seq(-1.5, 1.5, by = 0.5)) 
      vline(times[c("start.time", "end.time")], lwd = 2, col = "blue", lty = "dotted")         # Logbook.
      vline(times[c("touchdown.tilt", "liftoff.tilt")], lwd = 2, col = "green", lty = "solid") # Tilt.
      vline(times[c("touchdown.headline")], lwd = 2, col = "red", lty = "dashed")              # Headline.
            
      legend("bottom", horiz = TRUE, 
             legend = c("East", "North", "Up", "Total"), 
             col = c("blue", "green", "red", "black"), lwd = c(2, 2, 2, 2), bg = "white",
             cex = 0.8)  
   
      box()
   }
    
   axis(1, at = round(xlim[1]):round(xlim[2]))
   mtext("Time(min)", 1, 2.4, cex = 1.25)
 
   if (pdf) dev.off()
} 
