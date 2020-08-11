esonar.file.str <- function(x, year, tow.id, path = TRUE, survey = "sc", ...){
   # ESONAR.FILE.STR - Return a list of available E-Sonar file names.

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
      path.str <- esonar.path.str(survey = survey, year = year, ...)
   }else{
      path.str <- esonar.path.str(survey = survey, ...)
   }

   # Get list of CSV files:
   str <- NULL
   for (i in 1:length(path.str)) str <- c(str, list.files(path = path.str[i], pattern = "*.csv", recursive = TRUE, full.names = path, ignore.case = TRUE))
   
   # Remove invalid results:
   str <- str[setdiff(1:length(str), grep("File Not Found", str))]
   
   # Extract subset using tow ID:
   if (!missing(tow.id)){
      temp <- unlist(lapply(strsplit(str, "/", fixed = TRUE), function(x) x[length(x)]))
      temp <- toupper(gsub(".csv", "", tolower(temp)))
      index <- which(temp %in% tow.id)
      if (length(index) == 0) str <- NULL else str <- str[index]
   }
   
   # Remove path:
   if (!path) str <- unlist(lapply(strsplit(str, "/", fixed = TRUE), function(x) x[length(x)]))
   
   # Remove test trials:
   if (length(str) > 0){
      index <- grep("test", tolower(str))
      if (length(index) > 0) str <- str[-index]
   }
   
   return(str)
}
