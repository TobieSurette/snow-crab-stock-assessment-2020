info.esonar <- function(x, set.card){
   # INFO.ESONAR - Extract information for an 'esonar' object.

   if (is.null(header(x))) return(NULL)
   
   v <- list()
   v$ship.number <- header(x)$ShipNumber
   v$trip.number <- as.numeric(header(x)$TripNumber)
   v$tow.number  <- as.numeric(header(x)$TowNumber)
   v$comments    <- header(x)$Comments
   v$duration    <- as.numeric(max(time(x)) - min(time(x)))

   # Parse file name and path:
   str <- strsplit(header(x)$file, "/", fixed = TRUE)[[1]]
   v$file.name <- str[length(str)]
   if (length(str) == 1) v$path <- "" else v$path <- paste(str[1:(length(str)-1)], collapse = "/")

   str <- strsplit(tolower(v$file), ".", fixed = TRUE)[[1]]
   str <- toupper(str[1])
   v$tow.id <- str
   v$rows <- dim(x)[1]
   v$columns <- dim(x)[2]
   sensors <- unique(x$sensor)
   sensors <- sensors[sensors != ""]
   v$sensors <- paste("(", paste(sensors, collapse = ", "), ")", sep = "")
   t <- table(diff(time(x)))
   t <- t[t == max(t)]
   v$sampling.frequency <- as.numeric(names(t))
   
   return(v)
}
