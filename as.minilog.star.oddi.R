as.minilog.star.oddi <- function(x, ...){
   # AS.MINILOG.STAR.ODDI - Convert 'star.oddi' object to a 'minilog' object.
   
   # Extract Star Oddi header:
   h <- header(x)

   # Define variables to be kept:
   vars <- c("year", "month", "day", "hour", "minute", "second", "temperature", "depth")

   # Create Minilog header:
   mh <- list(ID = "Minilog-TD", Serial.Number = "9999", tow.id = "     ")
   temp <- time(x[1, ])
   temp <- paste0(paste0(substr(temp, 9, 10), "-",  substr(temp, 6, 7), "-", substr(temp, 1, 4)), ",", substr(temp, 12, 19))
   mh$Start.Time <- temp
   temp <- time(x[nrow(x), ])
   temp <- paste0(paste0(substr(temp, 9, 10), "-",  substr(temp, 6, 7), "-", substr(temp, 1, 4)), ",", substr(temp, 12, 19))
   mh$Finish.Time <- temp
   mh$Sample.Period = paste0("00:00:0", min(max(c(0, unique(as.numeric(diff(time(x))))))))
   mh$file.name <- h$file
   mh$survey <- h$survey

   # Strip non-data-frame attributes:
   attributes(x) <- attributes(x)[c("names", "row.names", "class")]

   # Keep relevant variables:
   x <- x[intersect(names(x), vars)]

   # Assign Minilog header:
   attributes(x) <- c(attributes(x), mh)
   
   # Convert to 'minilog' object:
   v <- minilog(x)
   
   return(v)
}
