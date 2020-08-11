star.oddi.path.str <- function(x, year, survey = "sc", location = "headline", type = "depth", tow.id, ...){
   # STAR.ODDI.PATH.STR - Return the path of a Star Oddi directory.

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
   
   # Convert 'survey' to lowercase:
   survey <- tolower(survey)

   # Check that either 'sampling' or 'survey' are specified:
   if (length(survey) == 0) stop("'survey' argument must be specified.'")

   # Parse 'survey' argument:
   survey <- gsub(" ", "", survey, fixed = TRUE)
   survey <- gsub(".", "", survey, fixed = TRUE)
   survey <- match.arg(survey, c("sc", "snowcrab"))

   # Define root search path:
   if (survey %in% c("sc", "snowcrab")) str <- .gulf.path$sc.minilog

   # Parse 'location' parameter:
   location <- match.arg(gsub(" ", "", tolower(location)), c("headline", "footrope"))
   
   # Parse 'location' parameter:
   type <- match.arg(gsub(" ", "", tolower(type)), c("depth", "tilt", "acceleration"))
   if (type == "tilt") location <- "footrope"
   if (type == "acceleration") location <- "footrope"

   # Snow crab survey:
   if (survey %in% c("sc", "snowcrab")){
      # Append year to path:
      if (missing(year)){
         str <- paste0(str, list.files(path = str, pattern = "^Fishing Year [0-9]+$"), "/")
      }else{
         str <- paste(str, "Fishing Year ", year, "/", sep = "")   
      }
      str <- paste(str, "Trawl Data/South Western Gulf/Star Oddi", sep= "")
      str <- str[file.exists(str)]

      if ((location == "footrope") & (type == "tilt"))  str <- paste0(str, "/Tilt")
      if ((location == "headline") & (type == "depth")) str <- paste0(str, "/Headline")
      if ((location == "footrope") & (type == "acceleration")) str <- paste0(str, "/Tilt")
      
      # Look through sub-directories:
      str <- str[file.exists(str)]
      if (length(str) > 0){
         tmp <- NULL
         for (i in 1:length(str)) tmp <- c(tmp, list.files(path = str[i], pattern = "G[CP][0-3][0-9][0-9]", ignore.case = TRUE, full.names = TRUE))
         str <- tmp
      }
   }         

   # Remove irrelevant errors:
   index <- which(str == "File Not Found")
   index <- grep("File Not Found", str)
   if (length(index) > 0) str <- str[-index]
   
   # Extract subset using tow ID:
   if (!missing(tow.id)){
      temp <- unlist(lapply(strsplit(str, "/", fixed = TRUE), function(x) x[length(x)]))
      temp <- toupper(substr(temp, 6, nchar(temp)))
      index <- which(temp %in% tow.id)
      if (length(index) == 0) str <- NULL else str <- str[index]
   }
   
   return(str)
}
