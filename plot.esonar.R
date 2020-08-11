plot.esonar <- function(x, set.card = NULL){
   # PLOT.ESONAR - Plot a 'esonar' object.

   # Define time series in minutes:
   time <- as.numeric((time(x) - min(time(x))) / 60)

   layout(matrix(1:4, ncol = 2, nrow = 2))

   # Plot primary sensor profile:
   index <- !is.na(time) & !is.na(x$headline) & (x$headline > 0)
   plot(time[index], x$headline[index],
        type = "l", xlab = "Time(min)", ylab = "Headline(m)",
        col = "blue", ylim = c(0, 50))
   points(time[index], x$headline[index], pch = 21, bg = "blue")
   
   # Plot secondary sensor profile:
   index <- !is.na(time) & !is.na(x$depth)
   plot(time[index], x$depth[index], type = "l", xlab = "Time(min)", ylab = "Depth(m)", col = "blue")
   points(time[index], x$depth[index], pch = 21, bg = "blue")

   # Plot doorspread profile:
   index <- !is.na(time) & !is.na(x$doormaster) & (x$doormaster > 0)
   plot(time[index], x$doormaster[index], type = "l",
        xlab = "Time(min)", ylab = "Door spread(m)",
        ylim = c(0, 30), col = "blue")
   points(time[index], x$doormaster[index], pch = 21, bg = "blue")

   map(x)
}
