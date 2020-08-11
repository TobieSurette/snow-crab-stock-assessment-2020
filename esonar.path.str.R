esonar.path.str <- function(year, survey = "sc", ...){
   # ESONAR.PATH.STR - Return the path of an E-Sonar directory.

   # Convert arguments to lowercase:
   survey <- tolower(survey)

   # Check that either 'sampling' or 'survey' are specified:
   if (length(survey) == 0) stop("'survey' argument must be specified.'")

   # Define netmind path:
   if (survey %in% c("sc", "snowcrab", "snow.crab")) str <- .gulf.path$sc.esonar
   
   # Append year to path:
   if (missing(year)){
      str <- paste0(str, list.files(path = str, pattern = "^Fishing Year [0-9]+$"))
   }else{
      str <- paste(str, "Fishing Year ", year, "/", sep = "")   
   }
   str <- paste(str, "/Trawl Data/South Western Gulf/ESonar/Summary", sep= "")

   str <- str[file.exists(str)]
   
   # Remove irrelevant errors:
   index <- which(str == "File Not Found")
   index <- grep("File Not Found", str)
   if (length(index) > 0) str <- str[-index]

   return(str)
}
