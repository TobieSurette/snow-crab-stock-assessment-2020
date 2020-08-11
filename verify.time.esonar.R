verify.time.esonar <- function(x, ...){
   # VERIFY.TIME - Checks a 'esonar' object time errors.

   msg <- NULL

   # Check time values:
   msg <- c(msg, verify.time.default(x))
   
   index <- which(diff(time(x)) != 1)
   fun <- function(x){ return(max(diff(x))) }
   temp <- aggregate(time(x), by = x["file.name"], fun)
   index <- which(temp[, 2] > 1)
   if (length(index) > 0)
      msg <- paste("E-sonar file '", temp[index, 1], "' contains a missing data block of ", temp[index, 2], " seconds.", sep = "")

   return(msg)
}
