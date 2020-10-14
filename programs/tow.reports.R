library(gulf.data)
library(gulf.graphics)
library(gulf.spatial)

# Load tow data:
x <- read.scsset(2020, valid = 1)

# Remove tow with no eSonar data:
x <- x[x$tow.id != "GP276F", ]

clg()
for (i in 1:nrow(x)){
   print(i)
   plot(x[i, ], pdf = TRUE, path = paste0(getwd(), "/results/tow reports"))
}
