verify.esonar <- function(x, key = TRUE, coordinates = TRUE, time = TRUE, match = TRUE, ...){
   # VERIFY.ESONAR - Checks an 'esonar' object for errors.

   msg <- NULL

   if (key)             msg <- c(msg, verify.key(x, key = c("year", "month", "day", "hour", "minute", "second"), ...))
   if (coordinates)     msg <- c(msg, verify.coordinates(x, ...))
   if (time)            msg <- c(msg, verify.time(x, ...))
   if (match)           msg <- c(msg, verify.match(x, ...))
   
   return(msg)
}
