truncate.esonar <- function(x, buffer = 0, ...){
   # TRUNCATE.ESONAR - Truncate a 'esonar' object.

   # Check input data:
   year <- unique(x$year)
   if (length(year) != 1) stop("'esonar' object 'x' must contain a single year of data.")
   
   # Load set card:
   tows <- read.scset(year = unique(x$year), print = FALSE)
   tows <- tows[tows$tow.id %in% unique(x$tow.id) , ]
   x <- x[x$tow.id %in% tows$tow.id, ]
   
   # Initialize result vector:
   index <- rep(FALSE, nrow(x))

   # Convert from minutes to seconds:
   if (!is.numeric(buffer) | !(length(buffer) %in% c(1,2)))
      stop("'buffer' must be a numeric scalar or two-element vector.")
   if (length(buffer) == 1) buffer = c(buffer, buffer)
   buffer <- buffer * 60  # Convert to seconds.
   buffer <- abs(buffer)
   
   # Remove data outside the define start and end times:
   for (i in 1:nrow(tows)){
      ii <- which(x$tow.id == tows$tow.id[i])
      t <- time(x[ii, ])
      ii <- ii[(t >= (start.time(tows[i,], ...) - buffer[1])) & (t <= (end.time(tows[i,], ...) + buffer[2]))]
      index[ii] <- TRUE
   }

   # Return subset of the data:
   x <- x[index, ]

   return(x)
}
