split.esonar <- function(x, by = "file.name"){
   # SPLIT.ESONAR - Split a 'esonar' object into its component tows.

   if (!is.null(header(x))){
      if (!("FileName" %in% names(x))){
         I <- info(x)
         x$ship.number <- I$ship.number
         x$trip.number <- I$trip.number
         x$tow.number  <- I$tow.number
         x$comments    <- I$comments
         x$duration    <- I$duration
         x$file.name   <- I$file.name
         x$path        <- I$path
         x$tow.id      <- I$tow.id
      }
      res <- list(x)
   }else{
      # Separate 'x' into component parts:
      res <- by(x, x[, by, drop = FALSE], function(x) return(x))

      # Remove 'by' attributes:
      attributes(res) <- NULL
   }

   return(res)
}
