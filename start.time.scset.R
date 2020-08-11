start.time.scset <- function(x, type = "regular", ...){
   # START.TIME.SCSET - Returns the trawl start time associated with an 'scset' object.

   # Parse 'type' argument:
   type <- match.arg(gsub("[ ._]", "", tolower(type)), c("regular", "logbook", "blackbook", "observed"))
   if (type %in% c("observed", "blackbook")) type <- "logbook"
   
   # Initialize result variable:
   v <- rep(as.POSIXlt(NA), nrow(x))
   
   if (type == "logbook"){
      index <- which(!is.na(x$start.time.logbook) & (x$start.time.logbook != "        "))
      v[index] <- time.default(paste0(as.character(date(x[index,])), " ", x$start.time.logbook[index], " AST"))
   }else{
      index <- which((is.na(x$start.time) | (x$start.time == "        ")) & (!is.na(x$start.time.logbook) | (x$start.time.logbook != "        ")))
      x$start.time[index] <- x$start.time.logbook[index]
      index <- which(!is.na(x$year) & !is.na(x$month) & !is.na(x$day) & !is.na(x$start.time) & (gsub(" ", "", x$start.time) != ""))
      if (length(index) > 0){
         x <- x[index, ]   
         v[index] <- time.default(paste0(as.character(date(x)), " ", x$start.time, " AST"))
      }
   }
   
   return(v)
}
