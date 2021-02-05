library(gulf.data)
library(gulf.graphics)
library(gulf.spatial)


file <- "data/closures/snow.crab.narw.grid.closures.2020.csv"

x <- read.csv(file)

x$date.close <- gulf.utils::date(year = as.numeric(unlist(lapply(strsplit(x$date.close, "/", fixed = TRUE), function(x) x[3]))),
                                 month = as.numeric(unlist(lapply(strsplit(x$date.close, "/", fixed = TRUE), function(x) x[1]))),
                                 day = as.numeric(unlist(lapply(strsplit(x$date.close, "/", fixed = TRUE), function(x) x[2]))))

x$week.close <- week(x$date.close)
x$julian.close <- julian(x$date.close)

ix <- x$date.open != ""
tmp <- gulf.utils::date(year = as.numeric(unlist(lapply(strsplit(x$date.open[ix], "/", fixed = TRUE), function(x) x[3]))),
                                 month = as.numeric(unlist(lapply(strsplit(x$date.open[ix], "/", fixed = TRUE), function(x) x[1]))),
                                 day = as.numeric(unlist(lapply(strsplit(x$date.open[ix], "/", fixed = TRUE), function(x) x[2]))))
x$date.open <- as.Date(NA)
x$date.open[ix] <- tmp

x$week.open <- 52
x$julian.open <- 365
x$week.open[ix] <- week(tmp)
x$julian.open[ix] <- julian(tmp)

dates <- gulf.utils::date(year = 2020, month = 1, day = 1)
dates <- data.frame(date = dates + (0:364)*24*60*60)
dates$week <- week(dates$date)

weeks <- sort(unique(x$week.close))
for (i in 1:length(weeks)){
   ix <- x$week.close <= weeks[i]
   r <- range(dates$date[dates$week == weeks[i]])
   ix <- which(ix & x$julian.open > julian(r[1])  )

   clg()
   gdevice("pdf", file = paste0(getwd(), "/results/figures/english/maps/NARW closures week ", i), height = 6, width = 7)

   map.new()
   map("bathymetry")
   plot.grid(x$grid[ix], col = NA)
   plot.grid(x$grid[x$week.close == weeks[i]], col = "yellow")
   map("coast", col = "floralwhite", border = "saddlebrown", lwd = 0.4)
   str <- paste0(format(r[1], "%b-%d"), " to ", format(r[2], "%b-%d"))
   text(par("usr")[1] + 0.85 * diff(par("usr")[1:2]), par("usr")[3] + 0.85 * diff(par("usr")[3:4]), str)
   legend("bottomleft", legend = c("closed", "new closure"),
          pch = 22, cex = 1.25, pt.cex = 3, pt.bg = c(NA, "yellow"), col = "black", bg = NA)
   box()
   map.axis(1:2)

   dev.off()
}
