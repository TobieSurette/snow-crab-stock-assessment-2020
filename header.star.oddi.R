header.star.oddi <- function(x){
   # HEADER.STAR.ODDI - Returns a Star Oddi object's header information.

   fields <- setdiff(names(attributes(x)), names(attributes(data.frame())))
   V <- attributes(x)[fields]

   if (length(V) == 0) return(NULL) else return(V)
}
