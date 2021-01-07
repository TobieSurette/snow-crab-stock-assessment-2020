library(gulf.data)
library(gulf.graphics)


years <- 2017:2020
output = "pdf"
reference <- "end.time"
language <- language("fr")

clg()
gdevice(output, file = paste0("results/figures/", language, "/passive.duration.", min(years), "-", max(years)))
m <- kronecker(matrix(1:length(years)), matrix(1, nrow = 5, ncol = 5))
m <- rbind(0, cbind(0, m, 0), 0, 0)
layout(m)
par(mar = c(0,0,0,0))

r <- NULL
for (i in 1:length(years)){
   s <- read.scsset(years[i], survey = "regular", valid = 1)
   gbarplot(table(round(time2sec(time(s, "liftoff"), time(s, "stop")))), xlim = c(0, 200), ylim = c(0, 10),
            xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n", grid = TRUE)
   if (i == 2) mtext("Frequency", 2, 2.5, at = 0, cex = 1.25)
   if (i == 4) mtext("Elapsed time(seconds)", 1, 2.75, cex = 1.25)
   if (i == 1) axis(2) else axis(2, at = 2*(0:4))
   if (i == 4) axis(1)
   text(175, 8, years[i], cex = 1.5)

   box(lwd = 0.75)

   fun <- function(x){
      return(c(mu = mean(x), sigma = sd(x), median = median(x),
               q2.5 = quantile(x, p = 0.025),
               q25 = quantile(x, p = 0.25),
               q75 = quantile(x, p = 0.75),
               q97.5 = quantile(x, p = 0.975)))
   }

   print(s$haul.time)

   t <- time2sec(time(s, "haul"), time(s, "stop"))

   #t <- time2sec(time(s, "liftoff"), time(s, "stop"))
   t <- t[t >= 0 & !is.na(t)]

   # print(t)
   r <- rbind(r, fun(t))

}

dev.off()



