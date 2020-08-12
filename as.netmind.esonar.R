as.netmind.esonar <- function(x){
   # AS.NETMIND - Convert an 'esonar' object to a 'netmind' object.

   # Define variables:
   vars <- c("year", "month", "day", "hour", "minute", "second", "longitude", "latitude", "speed", "depth")

   z <- x[vars]
   z$primary <- round(x$headline, 1)
   z$doorspread <- round(x$doormaster, 1)/
   z$depth <- round(x$depth, 1)0
   z$secondary <- NA

   # Build a header for the Netmind object:
   if (!("file.name" %in% names(x))){
      temp <- header(x)
      header <- list()
      header$FileName <- temp$file
      header$"Local Time" <- as.character(time(x[1, ]))
      tow.id <- strsplit(temp$file, "/", fixed = TRUE)[[1]]
      tow.id <- strsplit(tow.id[length(tow.id)], ".", fixed = TRUE)[[1]][1]
      header$Ship <-  paste(" ", as.character(date(x[1, ])), " Trip: ", tow.id, " Tow: tow ",  as.numeric(temp$TowNumber), sep = "")
      header$Trip <-  paste(" ", tow.id, " Tow: tow ",  as.numeric(temp$TowNumber), sep = "")
      header$Tow <-  paste(" tow ",  as.numeric(temp$TowNumber), sep = "")
      header$Comments <- temp$Comments

      # Add header information as attributes:
      attributes(z) <- c(attributes(z), header)
   }else{
      header <- list()
      header$file.name <- paste(unlist(lapply(strsplit(x$file, ".", fixed = TRUE), function(x) x[1])), ".TXT", sep = "")
      header$tow.number <- x$tow.number
      header$tow.id    <- x$tow.id
      header$FileName <- paste(" ", header$file.name, sep = "")
      header$"Local Time" <- paste(" ", as.character(time(x[1, ])), sep= "")

      tow.id <- unlist(lapply(strsplit(x$file, "/", fixed = TRUE), function(x) x[length(x)]))
      tow.id <- unlist(lapply(strsplit(tow.id, ".", fixed = TRUE), function(x) x[1]))
      header$Ship <-  paste(" ", as.character(date(x)), " Trip: ", tow.id, " Tow: tow ",  x$tow.number, sep = "")
      header$Trip <- paste(" ", tow.id, " Tow: tow ",  x$tow.number, sep = "")
      header$Tow <- paste(" tow ",  x$tow.number, sep = "")
      header$Comments <- paste(" ", x$comments, sep = "")
      header <- as.data.frame(header, stringsAsFactors = FALSE)
      header$secondary <- NA
      # Bind formatted header information:
      z <- cbind(z, header)

      temp <- names(z)
      temp[temp == "Local.Time"] <- "Local Time"
      names(z) <- temp
   }

   # Convert to 'netmind':
   class(z) <- "data.frame"
   z <- netmind(z)

   return(z)
}
