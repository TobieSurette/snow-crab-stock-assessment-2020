split.star.oddi <- function(x, by = "file.name"){
   # SPLIT.STAR.ODDI- Split a 'star.oddi' object into its component tows.

   if (!is.null(header(x))){
      if (!("FileName" %in% names(x))){
         h <- header(x)
         names <- c("Date.time", "Recorder", "File.type", "Columns", "Channels",
                    "Field.separation", "Decimal.point", "Date.def.", "Time.def.",
                    "Channel.1", "Channel.2", "Reconvertion", "Line.color", "Trend.Type.Number",
                    "Limit.Temp.Corr.OTCR")
         names <- names[names %in% names(h)]
      }
      res <- list(x)
   }else{
      # Separate 'x' into component parts:
      res <- by(x, x[, by, drop = FALSE], function(x) return(x))

      # Remove 'by' attributes:
      attributes(res) <- NULL
   }

   return(res)
}
