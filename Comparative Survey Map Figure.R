library(gulf)

clg()
jpeg(filename = "U:/Snow Crab/Stock Assessment 2019/Comparative Survey Map.jpeg", width = 5*480, height = 7*480, res = 6*75)

s <- read.scset(year = 2019, valid = TRUE)
s <- s[substr(s$tow.id, 2, 2) == "C", ]

gulf.map(region = "wcb", sea = TRUE, xlim = c(-62.15, -60.5), ylim = c(45.75, 47.5), land = FALSE)
points(longitude(s), latitude(s), pch = 21, bg = "red", cex = 1.25)
map.fishing.zones(species = 2526)
coastline()
text(longitude(s), latitude(s), substr(s$tow.id, 3, 5), cex = 0.5, pos = 2)
box()

ss <- read.scset(year = 2019, valid = TRUE)
ss <- ss[substr(ss$tow.id, 2, 2) != "C" & !(substr(ss$tow.id, 3, 5) %in% substr(s$tow.id, 3, 5)), ]

points(longitude(ss), latitude(ss), pch = 22, bg = "grey", cex = 0.75)

legend("bottomright", legend = c("Comparative", "Regular"), pch = c(21, 22), pt.bg = c("red", "grey"), bg = "white", cex = 1.2, pt.cex = 1.5)

dev.off()
