read.star.oddi <- function(x, file, offset = 0, ...){
   # READ.STAR.ODDI - Read a Star Oddi DST probe file.

   # Define list of files to be read:
   if (missing(file)) file <- star.oddi.file.str(x, ...)

   # Read multiple netmind files and concatenate them:
   if (length(file) > 1){
      x <- NULL
      for (i in 1:length(file)){
          cat(paste(i, ") Reading: '", file[i], "'\n", sep = ""))
          temp <- read.star.oddi(file[i])
          information <- header(temp)
          for (j in 1:length(information)){
             temp[, names(information)[j]] <- information[[names(information)[j]]]
          }

          if (!is.null(x)){
             # Create NA-valued columns if new variables appear:
             index <- setdiff(names(x), names(temp))
             if (length(index) > 0) temp[index] <- NA
             index <- setdiff(names(temp), names(x))
             if (length(index) > 0) x[index] <- NA

             # Order new file properly:
             temp <- temp[names(x)]
          }

          x <- rbind(x, temp)
      }
      temp <- attributes(x)
      temp <- temp[setdiff(names(temp), names(header(x)))]
      attributes(x) <- temp

      return(x)
   }

   if (length(file) == 0) return(NULL)
   
   if.numeric.str <- function(x) return(gsub("[ 0-9.]", "", x) == "")
   
   # Read and parse header info:
   y <- read.table(file = file, nrow = 30, colClasses = "character", comment.char = "", sep = "\n", blank.lines.skip = FALSE)[[1]]
   y <- gsub("#", "", y[substr(y, 1, 1) == "#"], fixed = TRUE)
   #y <- lapply(strsplit(y, "\t"), function(x) if (if.numeric.str(x[1])) x else x[2:length(x)])
   y <- lapply(strsplit(y, "\t"), function(x) x[2:length(x)])
   str <- unlist(lapply(y, function(x) return(gsub(":", "", x[1]))))
   #str <- gsub(" ", ".", unlist(lapply(y, function(x) gsub("(:)", "", x[[1]]))))
   #str <- gsub("..", ".", str, fixed = TRUE)
   #str <- gsub("-", ".", str, fixed = TRUE)
   values <- unlist(lapply(y, function(x) paste(x[2:length(x)], collapse = ", ")))
   header <- list()
   for (i in 1:length(str)){
      header[[i]] <- values[i]
   }
   names(header) <- str
   header$file.name <- file
   
   # Extract channel names:
   index <- grep("Channel [1-9]", str)
   channels <- unlist(header[index])
   if (is.null(channels)){
      index <- grep("Axis", str)
      channels <- unlist(header[index])
   }
   if (length(grep(".ACC", file)) > 0){
      channels <- gsub("Acc. vector (m/s^2), ", "", channels, fixed = TRUE)
      channels <- gsub("Axes X,Y,Z (m/s^2), ", "", channels, fixed = TRUE)
      channels <- lapply(strsplit(channels, ","), function(x) return(x[[1]]))
      channels <- unlist(channels)
   }else{
      channels <- strsplit(channels, "[,()]")
      channels <- unlist(lapply(channels, function(x) x[which.max(nchar(x))]))
      channels <- gsub(" ", "", channels, fixed = TRUE) 
      channels <- gsub("-", ".", channels, fixed = TRUE)  
      channels <- tolower(channels) 
      names(channels) <- NULL
   }
   
   # Read E-Sonar data:
   k <- length(y)
   x <- read.table(file = file, header = FALSE, skip = k, dec = ",", sep = "\t", 
                   colClasses = c("numeric", "character", rep("numeric", length(channels))))
   fields <- c("record", "date", channels)
   names(x) <- fields

   # Parse date fields:
   date <- data.frame(year = as.numeric(substr(x$date, 7, 8)),
                      month = as.numeric(substr(x$date, 4, 5)),
                      day = as.numeric(substr(x$date, 1, 2)),
                      stringsAsFactors = FALSE)

   x$time <- unlist(lapply(strsplit(x$date, "[ ,]"), function(x) x[2]))
   time <- data.frame(hour   = as.numeric(substr(x$time, 1, 2)),
                      minute = as.numeric(substr(x$time, 4, 5)),
                      second = as.numeric(substr(x$time, 7, 8)),
                      stringsAsFactors = FALSE)

   # Auto-correct date and time:
   index <- (date$year < 100)
   date$year[index] <- 2000 + date$year[index]
   index <- is.na(date$year)
   date$year[index] <- unique(date$year[!index])[1]
   index <- is.na(date$month)
   date$month[index] <- unique(date$month[!index])[1]
   index <- is.na(date$day)
   date$day[index] <- unique(date$day[!index])[1]

   # Create result variable:
   v <- cbind(x["record"], date, time, x[setdiff(names(x), c("date", "record"))])

   # Modify time by specified offset:
   if (offset != 0){
      t <- as.matrix(as.POSIXlt(time(v) + offset * 60))
      v$year   <- t[, "year"] + 1900
      v$month  <- t[, "mon"] + 1
      v$day    <- t[, "mday"]
      v$hour   <- t[, "hour"]
      v$minute <- t[, "min"]
      v$second <- t[, "sec"]
   }
   
   # Add tow ID to header:
   if (length(grep("GP[0-9][0-9][0-9]", file)) > 0){
      temp <- unlist(lapply(strsplit(file, "GP"), function(x) x[length(x)]))
      temp <- lapply(strsplit(temp, "/", fixed = TRUE), function(x) x[1])
      tow.id <- paste0("GP", unlist(temp))
      header$tow.id <- tow.id
      
      # Include 'tow.id' as a field:
      v$tow.id <- header$tow.id
   }
   
   # Create 'esonar' object:
   v <- star.oddi(v, header)

   return(v)
}
