star.oddi.file.str <- function(x, year, tow.id, path = TRUE, survey = "sc", sort = TRUE, type = "depth", ...){
   # STAR.ODDI.FILE.STR - Return a list of available Star Oddi file names.

   # Parse 'x' argument:
   if (!missing(x)){
      if (is.numeric(x)) year <- x
      if (is.character(x)){
         index <- file.exists(x)
         if (any(index)) return(x[index])
         if ((length(grep(".", x, fixed = TRUE)) > 0) | (length(grep("/", x, fixed = TRUE)) > 0)) return(x)
         tow.id <- x
      }
      if (is.data.frame(x)) if (("tow.id" %in% names(x)) & missing(tow.id)) tow.id <- x$tow.id
      if (is.data.frame(x)) if (("tow.id" %in% names(x)) & missing(year)) year <- sort(unique(x$year))
   }
     
   # Get minilog path directories:
   if (!missing(year)){
      if (!missing(tow.id)){
         path.str <- star.oddi.path.str(survey = survey, year = year, type = type, tow.id = tow.id, ...)
      }else{
         path.str <- star.oddi.path.str(survey = survey, year = year, type = type, ...)
      }
   }else{
      if (!missing(tow.id)){
         path.str <- star.oddi.path.str(survey = survey, type = type, tow.id = tow.id, ...)
      }else{
         path.str <- star.oddi.path.str(survey = survey, type = type, ...)
      } 
   }

   # Set 'survey' to snow crab survey if tow ID is specified:
   if (!missing(tow.id)) survey <- "sc"

   if (length(path.str) == 0) return(path.str)
   
   # Parse 'survey' argument:
   survey <- tolower(survey)
   survey <- gsub(" ", "", survey, fixed = TRUE)
   survey <- gsub(".", "", survey, fixed = TRUE)
   survey <- match.arg(survey, c("sc", "snowcrab"))

   # Define file string:
   str <- NULL
   if (survey %in% c("sc", "snowcrab")){
      if (type == "acceleration"){ 
         for (i in 1:length(path.str)) str <- c(str, list.files(path.str[i], pattern = "*.acc", ignore.case = TRUE, full.names = TRUE))
      }else{
         for (i in 1:length(path.str)) str <- c(str, list.files(path.str[i], pattern = "*.dat", ignore.case = TRUE, full.names = TRUE))
      }
   }
   
   if (length(str) == 0) return(NULL)
   
   # Keep only latest file within each sub-directory:
   dirs <- unlist(lapply(strsplit(str, "/", fixed = TRUE), function(x) x[length(x)-1]))
   files <- unlist(lapply(strsplit(str, "/", fixed = TRUE), function(x) x[length(x)]))
   probes <- substr(dirs, 1, 5)
   number <- unlist(lapply(strsplit(files, probes), function(x) as.numeric(x[1])))
   temp <- data.frame(index = 1:length(dirs), dir = dirs, file = files, number = number)
   temp <- sort(temp, by = c("dir", "number"))
   index <- cumsum(aggregate(temp$index, by = temp["dir"], function(x) length(x))$x)
   str <- str[index]
   
   # Remove path:
   if (!path) str <- unlist(lapply(strsplit(str, "/", fixed = TRUE), function(x) x[length(x)]))

   return(str)
}
