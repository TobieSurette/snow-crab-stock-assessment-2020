library(gulf)

s <- read.scset(year = 2019, valid = 1)

s <- summary(s, category = c("COM", "MM", "MI", "FM", "FI"))

jm <- s[substr(s$tow.id, 2, 2) == "C", ]
av <- s[substr(s$tow.id, 3, 5) %in%  substr(jm$tow.id, 3, 5) & substr(s$tow.id, 2, 2) != "C", ]

dist <- distance(longitude(jm), latitude(jm), longitude(av), latitude(av), pairwise = FALSE)
names(dist) <- substr(jm$tow.id, 3, 5)
#Touchdown and Liftoff - Tilt 2019.csv
z <- read.csv("U:/Snow Crab/Stock Assessment 2019/Touchdown and Swept Area Results/GP Swept Area - Tilt 2019.csv")
jm$swept.area <- z$swept.area[match(jm$tow.id, z$tow.id)]
jm$swept.area.sd <- z$swept.area.sd[match(jm$tow.id, z$tow.id)]
av$swept.area <- z$swept.area[match(av$tow.id, z$tow.id)]
av$swept.area.sd <- z$swept.area.sd[match(av$tow.id, z$tow.id)]

z <- read.csv("U:/Snow Crab/Stock Assessment 2019/Touchdown and Swept Area Results/Passive Swept Area 2017-2019.csv")
z <- z[z$year == 2019, ]
jm$swept.area.passive    <- z$passive.swept.area[match(jm$tow.id, z$tow.id)]
jm$swept.area.passive.sd <- NA
av$swept.area.passive    <- z$passive.swept.area[match(av$tow.id, z$tow.id)]
av$swept.area.passive.sd <- NA

# Define set of species to be collated:
species <- c(10, 40, 42, 201, 1823, 2521, 2527, 4210, 6115, 6121, 6123, 6400, 6500)
name.str <- c("cod", "plaice", "yellowtail", "thorny", "sea.potato", "coarctatus", 
              "araneus", "whelk", "mudstar", "purple.sunstar", "spiny.sunstar", "sea.urchin", "sand.dollar")

# Load by-catch data:
cat <- read.csv("W:/Crab/Offshore Crab Common/Fishing Year 2019/Trawl Data/South Western Gulf/By-catch/SCS2019C.csv", header = TRUE, stringsAsFactors = FALSE)
cat <- cat[(cat$tow.id %in% av$tow.id) | (cat$tow.id %in% jm$tow.id), ]

# Add missing count data from length-frequency data:
len <- read.csv("W:/Crab/Offshore Crab Common/Fishing Year 2019/Trawl Data/South Western Gulf/By-catch/SCS2019L.csv", header = TRUE, stringsAsFactors = FALSE)
len <- len[(len$tow.id %in% av$tow.id) | (len$tow.id %in% jm$tow.id), ]

counts <- aggregate(list(number.caught = len$length), by = len[c("tow.id", "species")], length)
index <- match(counts[c("tow.id", "species")], cat[c("tow.id", "species")])
counts <- counts[is.na(index), ]

tmp <- cat[1:nrow(counts), ]
tmp[names(tmp)] <- NA
tmp[names(counts)] <- counts
cat <- rbind(cat, tmp)

# Import species count data:
for (i in 1:length(species)){
  tmp <- cat[cat$species == species[i], ]
  av[,name.str[i]] <- tmp$number.caught[match(av$tow.id, tmp$tow.id)]
  jm[,name.str[i]] <- tmp$number.caught[match(jm$tow.id, tmp$tow.id)]
}
tmp <- av[name.str]; tmp[is.na(tmp)] <- 0; av[name.str] <- tmp;  
tmp <- jm[name.str]; tmp[is.na(tmp)] <- 0; jm[name.str] <- tmp;

# Import passive duration length:
z <- read.csv("U:/Snow Crab/Stock Assessment 2019/Touchdown and Swept Area Results/Touchdown and Liftoff - Tilt 2019.csv")
jm$liftoff <- z$liftoff[match(jm$tow.id, z$tow.id)]
av$liftoff <- z$liftoff[match(av$tow.id, z$tow.id)]

jm$passive.duration <- as.numeric(difftime(time.default(paste(as.character(date(jm)), jm$liftoff, "AST")), time.default(paste(as.character(date(jm)), jm$end.time.logbook, "AST")), units = "secs"))
av$passive.duration <- as.numeric(difftime(time.default(paste(as.character(date(av)), av$liftoff, "AST")), time.default(paste(as.character(date(av)), av$end.time.logbook, "AST")), units = "secs"))

# Collate into a single table 
vars <- c("tow.id", "depth", "bottom.temperature", "swept.area", "swept.area.sd", "swept.area.passive", "swept.area.passive.sd", "passive.duration", 
          "COM", "MM", "MI", "FM", "FI", name.str)
for (i in 1:length(vars)){
   x <- av[, vars[i]]
   y <- jm[, vars[i]]
   tmp <- data.frame(x = x, y = y, stringsAsFactors = FALSE)
   names(tmp) <- paste0(vars[i], ".", c("av", "jm"))
   if (i == 1) res <- tmp else res <- cbind(res, tmp)
}

# Crab export:
vars <- c("tow.id", "depth", "bottom.temperature", "swept.area", "swept.area.sd", "passive.duration", 
          "COM", "MM", "MI", "FM", "FI", "coarctatus", "araneus")
vars <- paste0(rep(vars, each = 2), c(".av", ".jm"))
crab <- res[vars]

crab$tow.id <- substr(crab$tow.id.av, 3, 10)
crab$swept.area.av <- paste0(round(crab$swept.area.av), "(", round(crab$swept.area.sd.av), ")")
crab$swept.area.jm <- paste0(round(crab$swept.area.jm), "(", round(crab$swept.area.sd.jm), ")")

crab <- crab[c("tow.id", setdiff(vars, c("tow.id.av", "tow.id.jm", "swept.area.sd.av", "swept.area.sd.jm")))]

# Fish export:
vars <- c("tow.id", "depth", "bottom.temperature", "swept.area", "swept.area.sd", "passive.duration", 
          "cod", "plaice", "yellowtail", "thorny")
vars <- paste0(rep(vars, each = 2), c(".av", ".jm"))
fish <- res[vars]
fish$tow.id <- substr(fish$tow.id.av, 3, 10)
fish$swept.area.av <- paste0(round(fish$swept.area.av), "(", round(fish$swept.area.sd.av), ")")
fish$swept.area.jm <- paste0(round(fish$swept.area.jm), "(", round(fish$swept.area.sd.jm), ")")
fish <- fish[c("tow.id", setdiff(vars, c("tow.id.av", "tow.id.jm", "swept.area.sd.av", "swept.area.sd.jm")))]

# Fish export:
vars <- c("tow.id", "depth", "bottom.temperature", "swept.area", "swept.area.sd", "passive.duration", 
          "sea.potato", "whelk", "mudstar", "purple.sunstar", "spiny.sunstar", "sea.urchin", "sand.dollar")
vars <- paste0(rep(vars, each = 2), c(".av", ".jm"))
inv <- res[vars]
inv$tow.id <- substr(inv$tow.id.av, 3, 10)
inv$swept.area.av <- paste0(round(inv$swept.area.av), "(", round(inv$swept.area.sd.av), ")")
inv$swept.area.jm <- paste0(round(inv$swept.area.jm), "(", round(inv$swept.area.sd.jm), ")")
inv <- inv[c("tow.id", setdiff(vars, c("tow.id.av", "tow.id.jm", "swept.area.sd.av", "swept.area.sd.jm")))]

species <- "sea.urchin" #  coarctatus araneus
winbugs(z = as.matrix(res[, paste0(species, c(".av", ".jm"))]), digits = 1)
winbugs(area_mu = as.matrix(res[, c("swept.area.av", "swept.area.jm")]), digits = 1)
winbugs(area_sigma = as.matrix(res[, c("swept.area.sd.av", "swept.area.sd.jm")]), digits = 1)
winbugs(area_passive_mu = as.matrix(res[, c("swept.area.passive.av", "swept.area.passive.jm")]), digits = 1)


# Plot vessel effects:
r <- read.csv("U:/Snow Crab/Stock Assessment 2019/Comparative Vessel Effects.csv", header = TRUE, stringsAsFactors = FALSE)

plot(c(0, 16), c(-3, 3), type = "n", xlab = "", ylab = "", xaxs = "i", yaxs = "i", xaxt = "n")
grid()
w <- 0.4
for (i in 1:nrow(r)){
   xx <- c(i-w, i+w, i+w, i-w)
   yy <- c(rep(r$"X25.00."[i], 2), rep(r$"X75.00."[i], 2))
   if (r$group[i] == "crab") col <- "grey10"
   if (r$group[i] == "fish") col <- "grey80"
   if (r$group[i] == "invertebrate") col <- "grey50"
   polygon(xx, yy, col = col)
   lines(c(i,i), c(r$"X2.50."[i], r$"X25.00."[i]), lwd = 2)
   lines(c(i,i), c(r$"X75.00."[i], r$"X97.50."[i]), lwd = 2)
   lines(c(i-0.2,i+0.2), rep(r$"X97.50."[i], 2), lwd = 2)
   lines(c(i-0.2,i+0.2), rep(r$"X2.50."[i], 2), lwd = 2)
   text(i, r$"X97.50."[i] + 0.1, r$variable[i], pos = 4, cex = 0.75, srt = 90, offset = 0.0)
}
lines(par("usr")[1:2], c(0,0), col = "red", lwd = 2, lty = "dashed")
legend("bottomleft", legend = c("crab", "fish", "invertebrate"), pch = 22, cex = 1.5, pt.cex = 3, pt.bg = c("grey10", "grey80", "grey50"), bg = "white") 
box()
mtext("log(vessel effect)", 2, 2.5, cex = 1.25)
mtext("Category/species", 1, 1.25, cex = 1.25)



r <- read.csv("U:/Snow Crab/Stock Assessment 2019/Comparative Vessel Effects.csv", header = TRUE, stringsAsFactors = FALSE)
r <- r[10:15, ]
plot(c(0, nrow(r)+1), c(-2.5, 2.5), type = "n", xlab = "", ylab = "", xaxs = "i", yaxs = "i", xaxt = "n")
grid()
w <- 0.4
for (i in 1:nrow(r)){
   xx <- c(i-w, i+w, i+w, i-w)
   yy <- c(rep(r$"X25.00."[i], 2), rep(r$"X75.00."[i], 2))
   if (r$group[i] == "crab") col <- "grey50"
   if (r$group[i] == "fish") col <- "grey90"
   if (r$group[i] == "invertebrate") col <- "grey50"
   polygon(xx, yy, col = col)
   lines(c(i,i), c(r$"X2.50."[i], r$"X25.00."[i]), lwd = 2)
   lines(c(i,i), c(r$"X75.00."[i], r$"X97.50."[i]), lwd = 2)
   lines(c(i-0.2,i+0.2), rep(r$"X97.50."[i], 2), lwd = 2)
   lines(c(i-0.2,i+0.2), rep(r$"X2.50."[i], 2), lwd = 2)
   text(i, r$"X97.50."[i] + 0.1, r$variable[i], pos = 4, cex = 0.75, srt = 90, offset = 0.0)
}
lines(par("usr")[1:2], c(0,0), col = "red", lwd = 3, lty = "dashed")
mtext("log(vessel effect)", 2, 2.5, cex = 1.25)
mtext("Species / Espèce", 1, 1.25, cex = 1.25)
box()
