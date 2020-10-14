# Load data comments:
#comments <- read.csv("W:/Crab/Offshore Crab Common/Fishing Year 2018/Trawl Data/South Western Gulf/Blackbook/By-catch Comments 2018.csv", stringsAsFactors = FALSE)
             
# Regular survey measurements:
x <- read.csv("U:/Snow Crab/Stock Assessment 2019/Fish Lengths Survey.csv", stringsAsFactors = FALSE)
names(x) <- tolower(names(x))
x$tow.id <- toupper(x$tow.id)

x$species[x$species == "339.7mm,"] <- 41
x$length <- gsub(",Male", "", x$length)
x$length.unit <- NA
x$length.unit[grep("mm", x$length)] <- "mm"
x$length.unit[x$tow.id == "GP107A1" & x$species == 30 & x$number == 1] <- "mm"
x$length.unit[x$tow.id == "GP258F" & is.na(x$length.unit)] <- "mm"
x$length.unit[x$tow.id == "GP247F" & x$species == 40] <- "cm"
x$length.unit[x$tow.id == "GP216F" & x$species == 40] <- "cm"
x$length.unit[x$tow.id == "GP216F" & x$species == 64] <- "cm"
x$length.unit[x$tow.id == "GP129F" & x$species == 64 & is.na(x$length.unit)] <- "cm"
x$species[x$tow.id == "GP129F" & x$species == 64 & x$number > 36] <- 40
x <- x[-which(x$tow.id == "GP317F" & x$species == 41 & x$number == 1), ]
x <- x[-which(x$tow.id == "GP061F" & x$species == 512 & x$number == 15), ]
x$length.unit[x$tow.id == "GP247F" & x$species == 512 & is.na(x$length.unit)] <- "mm"
x$length.unit[x$tow.id == "GP247F" & x$species == 302 & is.na(x$length.unit)] <- "cm"
x$length.unit[x$tow.id == "GP216F" & x$species == 302 & is.na(x$length.unit)] <- "cm"
x$length.unit[x$tow.id == "GP216F" & x$species == 304 & is.na(x$length.unit)] <- "cm"
x$length.unit[x$tow.id == "GP216F" & x$species == 623 & is.na(x$length.unit)] <- "cm"
x$length.unit[x$tow.id == "GP216F" & x$species == 512 & is.na(x$length.unit)] <- "cm"

# Define length precision:
x$length.precision <- 1   
index <- rep(FALSE, nrow(x))
index[setdiff(1:nrow(x), grep("[.]", x$length))] <- TRUE
x$length <- as.numeric(gsub("mm[,]*", "", x$length))
index <- which(index & x$length > 50)
x$length.precision[index] <- 10
                  
x$year <- 2019
vars <- c("year", "tow.id", "species", "length", "length.unit", "length.precision")
x <- x[vars]

# Comparative survey measurements:
y <- read.csv("U:/Snow Crab/Stock Assessment 2019/Fish Lengths Comparative.csv", stringsAsFactors = FALSE)
names(y) <- tolower(names(y))
y$tow.id <- toupper(y$tow.id)

y <- y[-which(y$tow.id == "GC317F" & y$species == 40 & y$length == "9"), ]
y <- y[-which(y$tow.id == "GC291F" & y$species == 40 & y$length == "130"), ]
y$length[y$tow.id == "GC318F" & y$species == 620 & y$length == "330"] <- "330.0mm"
y$length[y$tow.id == "GC332F" & y$species == 512 & y$length == "126"] <- "126.0mm"
y <- y[-setdiff(1:nrow(y), grep("[.]", y$length)), ]                                    
y$length[y$length == "-143.3m155.0mm"] <- "155.0mm"
y$length[y$length == "2206.7mm"] <- ""
y$length[y$length == "SM313.8mm"] <- "313.8mm"
y <- y[-which(y$length == "0.02"), ]
y$length <- gsub("mm", "", y$length)
y$length <- as.numeric(y$length)
y$length.unit <- "mm"
y$length.precision <- 1
y$year <- 2019        
y$length[y$length == 5233.9] <- 233.9
y <- y[vars]

y$length[y$species == 64  & y$length == 482.3] <- NA

# Combine data:
x <- rbind(x, y)
x$species <- as.numeric(x$species)

# Read 2019 comparative data sheets:
y <- read.csv("U:/Snow Crab/Stock Assessment 2019/Comparative Length-Freq Extra 2019.csv", stringsAsFactors = FALSE)
rep(y$length, each = y$count)

x$comment <- ""

# Expand data sheet length-frequencies from comparative survey:
z <- NULL
for (i in 1:nrow(y)){
   tmp <- data.frame(year             = rep(y$year[i], each = y$count[i]),
                     tow.id           = rep(y$tow.id[i], each = y$count[i]),
                     species          = rep(y$species[i], each = y$count[i]),
                     length           = rep(y$length[i], each = y$count[i]),
                     length.unit      = rep(y$length.unit[i], each = y$count[i]),
                     length.precision = rep(y$length.precision[i], each = y$count[i]),
                     comment          = rep(y$comments[i], each = y$count[i]), 
                     stringsAsFactors = FALSE) 
                     
   z <- rbind(z, tmp)
}

# Combine data:
x <- rbind(x, z)

# Write to file:
write.csv(x, "W:/Crab/Offshore Crab Common/Fishing Year 2019/Trawl Data/South Western Gulf/By-catch/SCS2019L.csv", row.names = FALSE)
