match.esonar <- function(x, set.card, method = "file.name"){
   # MATCH.ESONAR - Return the set card indices which match a 'esonar' object.

   # Parse 'method' argument:
   method <- tolower(method)
   method <- gsub(".", "", method, fixed = TRUE)
   method <- gsub(" ", "", method, fixed = TRUE)
   method <- match.arg(tolower(method), c("latlong", "filename", "time"))
   
   # Single esonar file:
   if (!is.null(header(x))){
      if (!missing(set.card)){
         # Match 'x' to set card using coordinates:
         if (method == "latlong"){
            index <- which(set.card$year == info(x)$year)
            set.card <- set.card[index, ]
            d <- distance(mean(x$longitude, na.rm = TRUE), mean(x$latitude, na.rm = TRUE), longitude(set.card), latitude(set.card)) * 1000
            d <- as.numeric(d)
            if (d[which.min(d)] > 500) index <- NA else index <- which.min(d)
         }
   
         # Match 'x' to set card using file name:
         if (method == "filename"){
            set.card$file.name <- gsub(" ", "", set.card$file.name)
            temp <- info(x)
            temp <- data.frame(year       = temp$year,
                               tow.number = temp$tow.number,
                               file.name  = temp$file.name,
                               stringsAsFactors = FALSE)
                  
            index <- match(temp, set.card, by = c("year", "tow.number", "file.name"))
         }
      
         # Match 'x' to set card using file name:
         if (method == "time"){
            ts <- touchdown(set.card) + (touchdown(set.card) - liftoff(set.card)) / 2
            tm <- mean(time(x)) - 3*60*60 # Substract 3 hours.
            d <- abs(as.numeric(tm - ts))
            if (d[which.min(d)] > 6) index <- NA else index <- which.min(d)   
         }  
      }    
   }

   # Multiple esonar files:
   if (is.null(header(x))){
      # Match 'x' to set card using coordinates:
      if (method == "latlong"){
          res <- aggregate(x[c("longitude", "latitude")], by = x[c("year", "tow.number", "file.name")], mean)
          d <- distance(res$longitude, res$latitude, longitude(set.card), latitude(set.card)) * 1000
          res$index <- apply(d, 1, which.min)
          res$distance <- apply(d, 1, function(x) x[which.min(x)])
          res$index[res$distance > 500] <- NA
          index <- merge(x, res, by = c("year", "tow.number", "file.name"), names = "index", all.x = TRUE, sort = FALSE)$index
      }
      
      # Match 'x' to set card using file name:
      if (method == "filename"){   
          set.card$file.name <- gsub(" ", "", set.card$file.name)
          set.card$file.name <- unlist(lapply(strsplit(set.card$file.name, ".", fixed = TRUE), function(x) x[[1]]))
          x$file.name <- unlist(lapply(strsplit(x$file.name, ".", fixed = TRUE), function(x) x[[1]]))
          index <- match(x$file.name, set.card$file.name)
      }
      
      # Match 'x' to set card using file name:
      if (method == "time"){   
          ts <- touchdown(set.card) + (touchdown(set.card) - liftoff(set.card)) / 2
          tm <- time(x) - 3*60*60 # Substract 3 hours.
          res <- aggregate(tm, by = x[c("year", "tow.number", "file.name")], mean)
          d <- abs(repvec(res[, 4], ncol = length(ts)) - repvec(ts, nrow = dim(res)[1]))
          res$index <- apply(d, 1, which.min)
          res$distance <- apply(d, 1, function(x) x[which.min(x)])
          res$index[res$distance > 600] <- NA
          index <- merge(x, res, by = c("year", "tow.number", "file.name"), names = "index", all.x = TRUE, sort = FALSE)$index
      }      
   }
   
   return(index)
}
