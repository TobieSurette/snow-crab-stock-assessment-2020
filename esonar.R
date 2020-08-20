#' eSonar Data
#'
#' @description Functions to read, manipulate and analyze eSonar trawl acoustic monitoring  probe data.
#'
#' @param x eSonar data file
#' @param year Numeric value specifying the survey or project year(s).
#' @param survey,project Character string specifying the survey or project name.
#'    The survey may refer to the September multi-species survey (\code{survey = "rv"},
#'    \code{survey = "sep"} or \code{survey = "September"}), the  Northumberland Strait
#'    survey (\code{= "ns"}), the mobile Sentinel survey  \code{= "sentinel"}),
#'    or the snow crab survey (\code{= "sc"} or \code{= "snow crab"}).
#' @param set.number,tow.id Numeric value or strings specifying the survey set number or tow ID.
#' @param set.card Data frame containing the survey tows which specify the Minilog data to be loaded.
#' @param path Logical value specifying whether to include the path in the Minilog files.
#'
#' @examples
#' # eSonar files for the 2020 snow crab survey:
#' locate.esonar(year = 2020)
#'
#' # Use a tow ID to extract file names for the snow crab survey 2006-2012:
#' locate.esonar("GP001", year = 2018:2020)
#'
#' # Use a set card extract for the first ten sets of 2012:
#' x <- read.gulf(year = 2012, survey = "sc")
#' minilog.file(x[1:10, ])
#'
#' x <- read.minilog(tow.id = "GP001F", year = 2010)
#' x <- read.minilog(survey = "rv", year = 2010)
#'
#' # Plot Minilog data:
#' file <- system.file("extdata", "Minilog example.txt", package = "gulf.data")
#' x <- read.minilog(file)
#' plot(x)
#'
#' @seealso \code{\link[gulf.data]{read.minilog}}
#' @seealso \code{\link[gulf.data]{header}}
#'

#' @export
esonar <- function(x, ...) UseMethod("esonar")

#' @export
esonar.default <- function(x, header, ...){
   # Add 'esonar' class tag:
   if (!("esonar" %in% class(x))) class(x) <- c("esonar", class(x))

   # Add header:
   if (!missing(header)) header(x) <- header

   # Assign key:
   key.esonar <- function(x) v <- c("year", "month", "day", "hour", "minute", "second")

   return(x)
}

#' @export
locate.esonar <- function(x, year, tow.id, full.names = TRUE, remove = "test", ...){
   # Parse 'x' argument:
   if (!missing(x)){
      if (is.numeric(x)) year <- x
      if (is.character(x)){
         if (any(file.exists(x))) return(x[file.exists(x)])
         tow.id <- x
      }
      if (is.data.frame(x)) if (("tow.id" %in% names(x)) & missing(tow.id)) tow.id <- x$tow.id
      if (is.data.frame(x)) if (("tow.id" %in% names(x)) & missing(year)) year <- sort(unique(x$year))
   }

   # Load set of file names:
   files <- locate(pattern = "*.csv", keywords = "esonar")

   # Target year:
   if (!missing(year)){
      if (!is.numeric(year)) stop("'year' must be a numeric integer.")
      year <- sort(year)
      index <- NULL
      for (i in 1:length(year)) index <- c(index, grep(year[i], files))
      files <- unique(files[index])
   }

   # Target tow ID:
   if (!missing(tow.id)){
      tow.id <- as.character(tow.id)
      index <- NULL
      for (i in 1:length(tow.id)) index <- c(index, grep(tolower(tow.id[i]), tolower(files)))
   }

   # Remove path:
   if (!full.names) files <- unlist(lapply(strsplit(files, "/", fixed = TRUE), function(x) x[length(x)]))

   # Remove files:
   if (!missing(remove)) remove <- remove[remove != "" & !is.na(remove)]
   if ((length(files) > 0) & (length(remove) > 0)) {
      index <- NULL
      for (i in 1:length(remove)) index <- c(index, grep(tolower(remove[i]), tolower(files)))
      if (length(index) > 0) files <- files[-index]
   }

   # Only keep unique file names:
   files <- unique(files)

   return(files)
}

#' @export read.esonar
read.esonar <- function(x, offset = -3*60, repeats = FALSE, ...){
   # Define list of files to be read:
   file <- locate.esonar(x, ...)

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

   # Read and parse header info:
   y <- read.table(file = file, nrow = 20, colClasses = "character", sep = "\n", blank.lines.skip = FALSE)

   # Define header information:
   header <- NULL
   header[gsub(" ", "", strsplit(y[1, ], ",")[[1]])] <- strsplit(y[2, ], ",")[[1]]
   header[gsub(" ", "", strsplit(y[3, ], ",")[[1]])] <- strsplit(y[4, ], ",")[[1]]
   header <- header[header != ""]
   header["file.name"] <- lapply(strsplit(file, "/"), function(x) x[length(x)])[[1]]
   header["tow.id"] <- toupper(lapply(strsplit(header["file.name"], "[.]"), function(x) x[1])[[1]])

   # Define data field names:
   fields <- gsub(" ", "_", strsplit(y[5,], ",")[[1]]) # Split header fields and their values.

   # Read E-Sonar data:
   x <- read.table(file = file, header = FALSE, skip = 6, sep = ",", colClasses = "character")
   names(x) <- fields

   # Remove lines with no date fields:
   temp <- table(substr(x[, 1], 1, 3))
   x <- x[substr(x[, 1], 1, 3) == names(temp[temp == max(temp)]), ]

   # Parse date fields:
   date <- data.frame(year = as.numeric(paste("", substr(x$GPS_Date, 8, 11), sep = "")),
                      month = match(tolower(substr(x$GPS_Date, 4, 6)), substr(tolower(month.name), 1, 3)),
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

   # Set NULL values to zero, and zeroes to NA:
   vars <- c("depth", "doormaster", "headline")
   v[setdiff(vars, names(v))] <- NA
   temp <- v[vars]
   temp[temp == 0] <- NA
   v[vars] <- temp

   # Remove repeating values:
   if (!repeats){
      for (i in 1:length(vars)){
         if (!all(is.na(v[, vars[i]]))){
            index <- which(diff(v[, vars[i]]) == 0)+1
            v[index, vars[i]] <- NA
         }
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
   v$tow.id <- header[["tow.id"]]

   # Remove records with missing time stamp:
   index <- is.na(v$hour) | is.na(v$minute) | is.na(v$second)
   v <- v[!index, ]

   return(v)
}

#' @export
plot.esonar <- function(x, set.card = NULL){
   # Define time series in minutes:
   time <- as.numeric((time(x) - min(time(x))) / 60)

   layout(matrix(1:4, ncol = 2, nrow = 2))

   # Plot primary sensor profile:
   index <- !is.na(time) & !is.na(x$headline) & (x$headline > 0)
   plot(time[index], x$headline[index],
        type = "l", xlab = "Time(min)", ylab = "Headline(m)",
        col = "blue", ylim = c(0, 50))
   points(time[index], x$headline[index], pch = 21, bg = "blue")

   # Plot secondary sensor profile:
   index <- !is.na(time) & !is.na(x$depth)
   plot(time[index], x$depth[index], type = "l", xlab = "Time(min)", ylab = "Depth(m)", col = "blue")
   points(time[index], x$depth[index], pch = 21, bg = "blue")

   # Plot door spread profile:
   index <- !is.na(time) & !is.na(x$doormaster) & (x$doormaster > 0)
   plot(time[index], x$doormaster[index], type = "l",
        xlab = "Time(min)", ylab = "Door spread(m)",
        ylim = c(0, 30), col = "blue")
   points(time[index], x$doormaster[index], pch = 21, bg = "blue")

   map(x)
}

