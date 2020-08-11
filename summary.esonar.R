summary.esonar <- function(x, year, truncate = TRUE, round = TRUE, ...){
   # SUMMARY.ESONAR - Returns a data summary of an 'esonar' object.

   # Load E-Sonar data:
   if (missing(x) & !missing(year)) x <- read.esonar(year = year)
   
   # Read tow data:
   y <- read.scset(year = unique(x$year), print = FALSE)
   
   # Trim data:
   if (truncate) x <- truncate(x, ...)
  
   # Define aggregating variables:
   vars <- c("year", "month", "day", "tow.id", "tow.number")
   res <- aggregate(list(speed = x$speed), x[vars], mean, na.rm = TRUE)
   res <- cbind(res, aggregate(list(heading = x$heading), x[vars], mean, na.rm = TRUE)["heading"])
   res <- cbind(res, aggregate(list(longitude.start = x$longitude), x[vars], min, na.rm = TRUE)["longitude.start"])
   res <- cbind(res, aggregate(list(latitude.start = x$latitude), x[vars], min, na.rm = TRUE)["latitude.start"])
   res <- cbind(res, aggregate(list(longitude.end = x$longitude), x[vars], max, na.rm = TRUE)["longitude.end"])
   res <- cbind(res, aggregate(list(latitude.end = x$latitude), x[vars], max, na.rm = TRUE)["latitude.end"])
   
   # Attach tow validity
   index <- match.data.frame(res[c("year", "tow.id")], y[c("year", "tow.id")])
   res$valid <- y$valid[index]
   res$tow.number.logbook <- y$tow.number[index]
   
   # Calculate tow distance:
   res$distance       <- distance(res$longitude.start, res$latitude.start, res$longitude.end,        res$latitude.end, pairwise = FALSE)
   res$distance.start <- distance(res$longitude.start, res$latitude.start, y$longitude.start.logbook[index], y$latitude.start.logbook[index], pairwise = FALSE)
   res$distance.end   <- distance(res$longitude.end,   res$latitude.end,   y$longitude.end.logbook[index],   y$latitude.end.logbook[index], pairwise = FALSE)
   
   # Net measurements: 
   res <- cbind(res, aggregate(list(depth = x$depth), x[vars], mean, na.rm = TRUE)["depth"])
   res <- cbind(res, aggregate(list(wingspread = x$doormaster), x[vars], mean, na.rm = TRUE)["wingspread"])
   res <- cbind(res, aggregate(list(headline = x$headline), x[vars], mean, na.rm = TRUE)["headline"])

   # Number of non-NA observations:
   fun <- function(x) return(sum(!is.na(x)))
   res <- cbind(res, aggregate(list(n.depth = x$depth), x[vars], fun)["n.depth"])
   res <- cbind(res, aggregate(list(n.wingspread = x$doormaster), x[vars], fun)["n.wingspread"])
   res <- cbind(res, aggregate(list(n.headline = x$headline), x[vars], fun)["n.headline"])
   res <- cbind(res, aggregate(list(n = x$depth), x[vars], length)["n"])
      
   # Calculate sampling rate:
   x$time <- time2sec(time(x))

   # Calculate length of data interval:
   res$duration <- aggregate(list(x = x$time), x[vars], function(x) return(diff(range(x))))$x + 1
   
   # Calculate data sampling rate:
   fun <- function(x){
      d <- table(diff(x))
      return(as.numeric(names(d[d == max(d)]))[1])
   }
   res <- cbind(res, aggregate(list(sampling.rate = x$time), x[vars], fun)["sampling.rate"])                                             

   # Round-off results:
   rvars <- c("heading", "depth", "wingspread", "headline")
   if (round){
      res[rvars] <- round(res[rvars], 1)
      res[c("speed")] <- round(res[c("speed")], 2)
      res[c("distance", "distance.start", "distance.end")] <- round(res[c("distance", "distance.start", "distance.end")], 3)
   }
     
   # Sort results:
   vars <- c("year", "month", "day", "tow.number", "tow.id")
   res <- sort(res, by = vars)
   rownames(res) <- NULL
   
   return(res)
}
