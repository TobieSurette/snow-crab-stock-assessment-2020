verify.coordinates.esonar <- function(x, ...){
   # VERIFY.COORDINATES - Check coordinates for a 'esonar' object.

   msg <- NULL

   if (!is.null(header(x))){
      # Check for missing values:
      vars <- c("year", "month", "day")
      res <- aggregate(x[c("longitude", "latitude")], by = x[vars], function(x) sum(is.na(x)))
      index <- which((res$longitude > 0) | (res$latitude > 0))
      temp <- paste("There are missing coordinates in '", info(x)$tow.id, "' .", sep = "")
      msg <- c(msg, msg.str(res[index, ], msg = temp, key = vars,  ...))

      # Check coordinates:
      n <- dim(x)[1]
      d <- distance(x$longitude[1:(n-1)], x$latitude[1:(n-1)], x$longitude[2:n], x$latitude[2:n], pairwise = FALSE)*1000
      m <- max(d[2:(length(d)-1)], na.rm = TRUE)

      if (m > 50){
         msg <- c(msg, paste("There was a coordinate skip in ", info(x)$tow.id, " whose distance was ", round(m, 1), " meters.", sep = ""))
      }
   }else{
      # Check for missing values:
      vars <- c("year", "month", "day", "tow.id", "tow.number")
      res <- aggregate(x[c("longitude", "latitude")], by = x[vars], function(x) sum(is.na(x)))
      index <- which((res$longitude > 0) | (res$latitude > 0))
      temp <- paste("There are missing coordinates.", sep = "")
      msg <- c(msg, msg.str(res[index, ], msg = temp, key = vars,  ...))

      # Check coordinates:
      n <- dim(x)[1]
      d <- c(NA, distance(x$longitude[1:(n-1)], x$latitude[1:(n-1)], x$longitude[2:n], x$latitude[2:n], pairwise = FALSE))*1000
      fun <- function(x){
         return(max(x[2:(length(x)-1)], na.rm = TRUE))
      }
      temp <- aggregate(d, by = x[c("tow.id", "tow.number")], fun)
      
      index <- (temp[, 3] > 50)
      str <- paste("There was a coordinate skip whose distance was ", round(temp[index, 3], 1), " meters.", sep = "")
      msg <- c(msg, msg.str(temp[index, ], msg = str, key = c("tow.id", "tow.number")),  ...)
   }
   
   return(msg)
}
