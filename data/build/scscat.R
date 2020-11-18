library(gulf.data)

year <- 2020

# Files to treat:
x <- read.csv(paste0("data/raw/scs.cat.", year, ".csv"), header = TRUE, stringsAsFactors = FALSE)

names(x) <- tolower(names(x))

# Parse date:
x$tow.id <- toupper(x$gpnum)
x$comment <- gsub("\n", ", ", x$comment)
x$comment[x$comment == ", "] <- ""

# Isolate catch variables:
vars <- names(x)[grep("[cwp][0-9]+", names(x))]
vars <- vars[!apply(x[vars], 2, function(x) all(is.na(x)))]  # Remove species with only NA values.

# Reformat table:
species <- sort(as.numeric(unique(substr(vars, 2, nchar(vars)))))
y <- NULL
for (i in 1:length(species)){
   str <- vars[which(as.numeric(substr(vars, 2, nchar(vars))) %in% species[i])]
   if (length(grep("^p", str)) > 0) p <-  x[,str[grep("p", str)]] else p <- NA
   if (length(grep("^w", str)) > 0) cw <- x[,str[grep("w", str)]] else cw <- NA
   if (length(grep("^c", str)) > 0) nc <- x[,str[grep("c", str)]] else nc <- NA
   tmp <- cbind(x[c("date", "tow.id")], data.frame(species = species[i], weight.caught = cw, number.caught = nc, presence = p))
   y <- rbind(y, tmp)
}
y$number.caught[which(y$number.caught == 0)] <- NA
y$weight.caught[which(y$weight.caught == 0)] <- NA
y$comment <- ""

# Remove empty catches:
y <- y[!apply(y[c("weight.caught", "number.caught", "presence")], 1, function(x) all(is.na(x))), ]

# Fix missing presences:
y$presence[is.na(y$presence) & (!is.na(y$weight.caught) | !is.na(y$number.caught))] <- 1

# Read set of valid tows:
s <- squeeze(read.scsset(year = year))

# Check that tow IDs exist:
index <- match(y$tow.id, s$tow.id)
y[is.na(index), ]

# Check that dates match:
all(y$month == s$month[index] & y$day == s$day[index])

# Import tow number:
y$tow.number <- s$tow.number[index]

# Re-order columns:
y <- y[c("date", "tow.number", "tow.id", "species", "number.caught", "weight.caught", "presence", "comment")]

# Spot corrections:
y$species[y$tow.id == "GP165A1" & y$species == 8200] <- 1900
y$species[y$tow.id == "GP218F" & y$species == 8200] <- 1900
y$number.caught[y$tow.id == "GP272F" & y$species == 4355] <- 388
y$weight.caught[y$tow.id == "GP272F" & y$species == 4355] <- 44.1
y$number.caught[y$tow.id == "GP351F" & y$species == 40] <- 129

# Check individual species entries:
i = 1
species(species[i])
tmp <- y[y$species == species[i], ]
tmp$ratio <- round(tmp$weight.caught / tmp$number.caught, 3)
tmp[order(tmp$weight.caught), ]
y[y$tow.id == "GP231F", ]

y <- sort(y, by = c("date", "tow.number", "tow.id", "species"))
rownames(y) <- NULL

# Write by-catch table:
write.table(y, file = paste0("data/by-catch/scs.cat.", year, ".csv"), sep = ",", row.names = FALSE)

# Write to gulf.data repository:
tmp <- unlist(lapply(strsplit(getwd(), "/"), function(x) x[length(x)]))
path <- paste0(gsub(tmp, "", getwd()), "gulf.data/inst/extdata")
if (file.exists(path)){
   file <- paste0(path, "/", "scs.cat.", year, ".csv")
   write.csv(y, file = file, row.names = FALSE)
}
