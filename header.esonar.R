header.esonar <- function(x){
   # HEADER.ESONAR - Returns an 'esonar' object's header information.

   fields <- setdiff(names(attributes(x)), names(attributes(data.frame())))
   V <- attributes(x)[fields]

   if (length(V) == 0) return(NULL) else return(V)
}
