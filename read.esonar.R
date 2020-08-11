read.esonar <- function(x, offset = -3*60, ...){
   # READ.ESONAR - Read an E-Sonar file.
   
   # Define list of files to be read:
   file <- esonar.file.str(x, ...)
      
   # Read multiple netmind files and concatenate them:
   if (length(file) == 0) return(NULL)
   if (length(file) > 1){
      x <- NULL
      for (i in 1:length(file)){
          cat(paste(i, ") Reading: '", file[i], "'\n", sep = ""))
          temp <- read.esonar(file[i])
          information <- info(temp)[1:8]
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

   # Define string to month function:
   str2month <- function(x){
      str <- c("january", "february", "march", "april", "may", "june", "july",
               "august", "september", "october", "november", "december")
               
      return(pmatch(tolower(x), str, duplicates.ok = TRUE))
   }
   
   # Read and parse header info:
   y <- read.table(file = file, nrow = 20, colClasses = "character", sep = "\n", blank.lines.skip = FALSE)
   str <- gsub(" ", "", strsplit(y[1, ], ",")[[1]])
   str <- str[str != ""]
   values <- gsub(" ", "", strsplit(y[2, ], ",")[[1]])
   #values <- values[values != ""]
   header <- list()
   for (i in 1:length(str)){
      header[[i]] <- values[i]
   }
   names(header) <- str
   str <- gsub(" ", "", strsplit(y[3, ], ",")[[1]])
   str <- str[str != ""]
   values <- strsplit(y[4, ], ",")[[1]]
   values <- values[values != ""]
   if (is.null(values)) values <- ""
   for (i in 1:length(str)){
      header[[length(header)+1]] <- values[i]
   }
   names(header) <- c(names(header)[1:(length(header)-1)], str)
   header$file.name <- file
   
   fields <- strsplit(y[5,], ",")[[1]] # Split header fields and their values.
   fields <- gsub(" ", "_", fields)
   
   # Read E-Sonar data:
   k <- 5
   x <- read.table(file = file, header = FALSE, skip = k+1, sep = ",", colClasses = "character")
   names(x) <- fields
   
   temp <- table(substr(x[, 1], 1, 3))
   x <- x[substr(x[, 1], 1, 3) == names(temp[temp == max(temp)]), ]
   
   # Parse date fields:
   date <- data.frame(year = as.numeric(paste("", substr(x$GPS_Date, 8, 11), sep = "")),
                      month = str2month(substr(x$GPS_Date, 4, 6)),
                      day = as.numeric(substr(x$GPS_Date, 1, 2)),
                      stringsAsFactors = FALSE)

   # Pad time with zeroes:
   index <- (nchar(x$GPS_Time) == 5)
   x$GPS_Time[index] <- paste("0", x$GPS_Time[index], sep = "")

   time <- data.frame(hour   = as.numeric(substr(x$GPS_Time, 1, 2)),
                      minute = as.numeric(substr(x$GPS_Time, 3, 4)),
                      second = as.numeric(substr(x$GPS_Time, 5, 6)),
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
   v <- cbind(date, time)
   
   # Parse latitude and longitude:
   lon <- -(as.numeric(substr(x$Longitude, 1, 3)) + as.numeric(substr(x$Longitude, 4, 12)) / 60)
   lat <- as.numeric(substr(x$Latitude, 1, 2)) + as.numeric(substr(x$Latitude, 4, 12)) / 60
   v <- cbind(v, data.frame(longitude = lon, latitude = lat))

   # Parse speed variable:
   v$speed <- as.numeric(x$Speed)
   v$heading <- as.numeric(x$Heading)
   v$validity <- x$Validity
   v$transducer <- x$Transducer_Name
   v$sensor <- x$Sensor_Name
   v$value <- as.numeric(x$Sensor_Value)
   v$error.code <- x$Error_Code
   v$hydrophone <- x$Hydrophone
   v$signal.strength <- as.numeric(x$Signal_Strength)
   
   # Parse sensor values into separate columns:
   str <- unique(v$sensor)
   str <- sort(str[str != ""])
   for (i in 1:length(str)){
      v[tolower(str[i])] <- NA
      v[[tolower(str[i])]][v$sensor == str[i]] <- v$value[v$sensor == str[i]]
   }
   
   # Set NULL values to zero:
   vars <- c("depth", "doormaster", "headline")
   v[setdiff(vars, names(v))] <- NA
   temp <- v[vars]
   temp[temp == 0] <- NA
   v[vars] <- temp
   
   # Remove repeating values:
   for (i in 1:length(vars)){
      if (!all(is.na(v[, vars[i]]))){
         index <- which(diff(v[, vars[i]]) == 0)+1
         v[index, vars[i]] <- NA
      }
   }
   
   # Modify time by specified offset:
   if (offset != 0){
      t <- as.character(time(v) + offset * 60)
      
      v$year   <- as.numeric(substr(t, 1, 4))
      v$month  <- as.numeric(substr(t, 6, 7))
      v$day    <- as.numeric(substr(t, 9, 10))
      v$hour   <- as.numeric(substr(t, 12, 13))
      v$minute <- as.numeric(substr(t, 15, 16))
      v$second <- as.numeric(substr(t, 18, 19))
   }
     
   # Create 'esonar' object:
   v <- esonar(v, header)

   # Include 'tow.id' as a field:
   v$tow.id <- info(v)$tow.id
   
   # Remove records with missing time stamp:
   index <- is.na(v$hour) | is.na(v$minute) | is.na(v$second)
   v <- v[!index, ]
   
   return(v)
}
