library(gulf.data)
library(gulf.graphics)
library(gulf.spatial)

# Load tow data:
x <- read.scsset(2020, valid = 1)

# Plot tow data:
plot(x, pdf = TRUE)

