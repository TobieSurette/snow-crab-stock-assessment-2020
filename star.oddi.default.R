star.oddi.default <- function(x, header, ...){
   # STAR.ODDI.DEFAULT - Create a 'star.oddi' object.

   if (!("star.oddi" %in% class(x))) class(x) <- c("star.oddi", class(x))

   if (!missing(header)){
      # Add header information as attributes:
      attributes(x) <- c(attributes(x), header)
   }

   return(x)
}
