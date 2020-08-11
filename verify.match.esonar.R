verify.match.esonar <- function(x, set.card, ...){
   # VERIFY.MATCH.ESONAR - Checks that a 'esonar' object's entries have matches in a target object.

   msg <- NULL

   # Define set card if missing:
   if (missing(set.card)) set.card <- oracle.read.scset(year = unique(x$year))

   # Match indices using various methods:
   res <- as.data.frame(matrix(NA, nrow = dim(x)[1], ncol = 3))
   names(res) <- c("latlon", "file.name", "time")
   res$latlon    <- match(x, set.card, method = "latlon")
   res$file.name <- match(x, set.card, method = "file.name")
   res$time      <- match(x, set.card, method = "time")

   # Check coordinate versus file match:
   if (!is.null(header(x))) vars <- c("year", "month", "day") else vars <- c("year", "month", "day", "tow.number", "file.name")

   index <- which(is.na(res[, 1]) & !is.na(res[, 2]))
   temp <- unique(x[index, vars])
   str <- paste("Coordinate match did not work.", sep = "")
   msg <- c(msg, msg.str(temp, msg = str, key = vars, ...))

   # Check for matches with no file matches:
   index <- which((!is.na(res[, 1]) | !is.na(res[, 3])) & is.na(res[, 2]))
   temp <- unique(x[index, vars])
   str <- paste("File does not exist in set card but there are coordinate or time matches.", sep = "")
   msg <- c(msg, msg.str(temp, msg = str, key = vars, ...))

   # Check for differing matches:
   index <- which(!is.na(res[, 1]) & !is.na(res[, 2]) & (res[, 1] !=  res[, 2]))
   temp <- unique(x[index, vars])
   str <- paste("Coordinate and file macthes are different.", sep = "")
   msg <- c(msg, msg.str(temp, msg = str, key = vars, ...))

   # Check for differing matches:
   index <- which(!is.na(res[, 1]) & !is.na(res[, 3]) & (res[, 1] !=  res[, 3]))
   temp <- unique(x[index, vars])
   str <- paste("Coordinate and time macthes are different.", sep = "")
   msg <- c(msg, msg.str(temp, msg = str, key = vars, ...))

   # Check for differing matches:
   index <- which(!is.na(res[, 2]) & !is.na(res[, 3]) & (res[, 2] !=  res[, 3]))
   temp <- unique(x[index, vars])
   str <- paste("File and time macthes are different.", sep = "")
   msg <- c(msg, msg.str(temp, msg = str, key = vars, ...))

   # Check that all set card entries have a matching Netmind:
   index <- setdiff(1:dim(set.card)[1], unique(res$file.name, na.rm = TRUE))
   str <- paste("Set file has no corresponding E-Sonar file.", sep = "")
   msg <- c(msg, msg.str(set.card[index, ], msg = str, var = "file.name",  ...))

   return(msg)
}
