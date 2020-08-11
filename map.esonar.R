map.esonar <- function(x, set.card = NULL, variable = NULL, ...){
   # MAP.SONAR - Display an 'esonar' object on a map.

   # Create map axes:
   rx <- range(x$longitude)
   ry <- range(x$latitude)
   dx <- diff(rx)
   dy <- diff(ry)
   gulf.map(xlim = c(rx[1] - dx*0.1, rx[2] + dx*0.1),
            ylim = c(ry[1] - dy*0.1, ry[2] + dy*0.1),
            aspect.adjust = TRUE, ...)

   # Draw points:
   if (is.null(variable)){
      points(x$longitude, x$latitude, pch = 21, bg = "blue", cex = 0.8)
   }else{
      index <- !is.na(x[, variable])
      points(x$longitude[index], x$latitude[index], pch = 21, bg = "blue", cex = 2* 0.8* x[index, variable] / max(x[index, variable]))
   }

   # Plot set card start-end points:
   if (!is.null(set.card)){
      index <- match(x, set.card)
      points(set.card$longitude.start[index], set.card$latitude.start[index], pch = 21, bg = "red")
      points(set.card$longitude.end[index], set.card$latitude.end[index], pch = 21, bg = "red")
   }
}
