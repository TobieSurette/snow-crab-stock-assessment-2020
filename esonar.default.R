esonar.default <- function(x, header, ...){
   # ESONAR.DEFAULT - Create a 'esonar' object.

   if (!("esonar" %in% class(x))) class(x) <- c("esonar", class(x))

   if (!missing(header)){
      # Add header information as attributes:
      attributes(x) <- c(attributes(x), header)
   }

   return(x)
}
