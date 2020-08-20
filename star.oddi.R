star.oddi <- function(x, ...) UseMethod("star.oddi")

locate.star.oddi <- function(x, remove = TRUE, ...){
   # Global search:
   files <- locate(pattern = "*.dat", ...)

   # Remove irrelvant data files:
   if (remove){
      index <- grep("[0-9]-", files)
      if (length(index) > 0) files <- files[-index]
      index <- grep("lost", tolower(files))
      if (length(index) > 0) files <- files[-index]
   }

   # Remove cross-copied files:
   if (length(files) > 0){
      f <- unlist(lapply(strsplit(files, "/"), function(x) x[length(x)-1]))
      tow.id <- substr(f, 6, 15)
      f <- unlist(lapply(strsplit(files, "/"), function(x) x[length(x)]))
      id <- as.numeric(unlist(lapply(strsplit(f, "[HC]"), function(x) x[1])))
      tmp <- aggregate(list(id = id), by = list(tow.id = tow.id), max)
      files <- files[which(!is.na(match(data.frame(tow.id = tow.id, id = id), tmp)))]
   }

   return(files)
}

