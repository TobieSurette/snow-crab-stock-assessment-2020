library(gulf)

# Comparartive survey data (Jean Mathieu):
x <- rbind(read.table("U:/Snow Crab/Stock Assessment 2019/Crab Export 2019.tab", header = FALSE, sep = "\t", stringsAsFactors = FALSE), 
           read.table("U:/Snow Crab/Stock Assessment 2019/Crab Export Comparative 2019.tab", header = FALSE, sep = "\t", stringsAsFactors = FALSE))

names(x) <- c("date", "tow.number", "crab.number", "sex", "carapace.width", "chela.height", "maturity", "shell.condition", "gonad.colour", "egg.colour",  "eggs.remaining",
              paste0("L", 1:5), paste0("R", 1:5), "samplers", "tow.id", "comments")
              
x$missing.legs <- apply(x[, c(paste0("L", 1:5), paste0("R", 1:5))], 1, paste0, collapse = "")
x$year <- as.numeric(substr(x$date, 1, 4))
x$month <- as.numeric(substr(x$date, 6, 7))
x$day <- as.numeric(substr(x$date, 9, 10))

vars <- c("year", "month", "day", "tow.id", "tow.number", "crab.number", "sex", "carapace.width", "chela.height", "shell.condition", "missing.legs", 
          "gonad.colour", "egg.colour", "eggs.remaining", "samplers", "comments")  

x <- x[vars]

x$tow.id <- toupper(x$tow.id)
x$shell.condition.mossy <- "*"
x$shell.condition.mossy[substr(x$shell.condition, 2, 2) == "M"] <- "M"
x$shell.condition <- substr(x$shell.condition, 1, 1)
x$tag.number <- "00000000"
x$position.type <- "LL" 
x$weight <- "*****"
x$depth <- "***"
x$durometer <- "***"
x$trap.code <- "****"
x$abdomen.width <- "*****"    

# Remove irrelevant data:
x <- x[x$month >= 7, ]
x <- sort(x, by = c("year", "month", "day", "tow.number", "crab.number"))
index <- is.na(x$carapace.width) & (x$shell.condition == "*") & (x$gonad.colour == "*") & (x$egg.colour == "*") & (x$eggs.remaining == "*")
x <- x[!index, ]
x <- x[x$sex %in% c("1", "2"), ]

# Shell condition corrections:
x$shell.condition[x$tow.id == "GC296F" & x$sex == 2 & x$crab.number == 115] <- 3
x$shell.condition[x$tow.id == "GC298F" & x$sex == 2 & x$crab.number == 226] <- 2
x$shell.condition[x$tow.id == "GC299F" & x$sex == 2 & x$crab.number == 343] <- 4
x$shell.condition[x$tow.id == "GC304F" & x$sex == 2 & x$crab.number == 242] <- 4
x$shell.condition[x$tow.id == "GP032F" & x$sex == 2 & x$crab.number %in% 54:56] <- 4
x$shell.condition[x$tow.id == "GP123F" & x$sex == 2 & x$crab.number == 218] <- 2
x$shell.condition[x$tow.id == "GP288F" & x$sex == 2 & x$crab.number == 162] <- 3   
x$shell.condition[x$tow.id == "GP302F" & x$sex == 2 & x$crab.number == 235] <- 2
                                                                                          
# Fix comments:
x$comments <- gsub("Ž", "e", x$comments)
x$comments <- gsub("ã©", "e", x$comments)
x$comments <- paste0(toupper(substr(x$comments, 1, 1)), tolower(substr(x$comments, 2, nchar(x$comments))))

# Check missing legs:
table(unlist((strsplit(x$missing.legs, ""))))

# Check for inconsistencies is sex measurements:
which(x$sex == "2" & !is.na(x$chela.height))
index <- which((x$sex == "1") & ((x$gonad.colour != "*") | (x$egg.colour != "*") | (x$eggs.remaining != "*")))

# Immatures which have egg values:
index <- which((x$sex == "2") & ((x$gonad.colour != "*") & ((x$egg.colour != "*") | (x$eggs.remaining != "*"))))
x$egg.colour[index] <- "*"
x$eggs.remaining[index] <- "*"

# No maturity values:
index <- which((x$sex == "2") & ((x$gonad.colour == "*") & (x$egg.colour == "*") & (x$comments == "Abdomen manquante" )))
x$eggs.remaining[index] <- "*"

# Missing eggs remaining:
index <- which((x$egg.colour == "4") & (x$eggs.remaining == "*"))
x$eggs.remaining[index] <- "0"

# Fix tow exceeding 1000 crab:
x$crab.number[x$tow.id == "GP101A1"] <- 999 + x$crab.number[x$tow.id == "GP101A1"]  
x$tow.number[x$tow.id == "GP101A1"] <- 4 
x$tow.id[x$tow.id == "GP101A1"] <- "GP101F"
x$tow.number[x$month == 8 & x$day == 23 & x$tow.number > 4] <- x$tow.number[x$month == 8 & x$day == 23 & x$tow.number > 4] - 1

# Fix samplers:
x$samplers[x$samplers == "Yves garlo Larocque"] <- "Yves Larocque"

x$carapace.width[x$carapace.width == 0] <- NA
x$carapace.width[x$tow.id == "GC330F" & x$crab.number == 29] <- 94.28 # Expected value.           

# Chela height fixes (expected value):
x$chela.height[x$tow.id == "GP197F" & x$crab.number == 51] <- 29.47
x$chela.height[x$tow.id == "GP066F" & x$crab.number == 85] <- 29.47
x$chela.height[x$tow.id == "GP325F" & x$crab.number == 209] <- NA  
x$chela.height[x$tow.id == "GP163F" & x$crab.number == 113] <- NA  
x$chela.height[x$tow.id == "GP053F" & x$crab.number == 30] <- NA   
x$chela.height[x$tow.id == "GP148F" & x$crab.number == 19] <- NA    
x$chela.height[x$tow.id == "GC297F" & x$crab.number == 27] <- NA  

# IMPORTANT - Check number of crab with missing chela height measurements:
x[which(x$carapace.width >= 95 & is.na(x$chela.height)), ]

# Load tow data: 
y <- read.scset(year = 2019)

# Remove fake crab:
x <- x[-which(x$tow.id == "GC330F" & x$crab.number == 1), ]

# Check for data with non-valid tows:
index <- match(x$tow.id, y$tow.id)
table(y$valid[index])

# Check that dates match:
which((y$year[index] != x$year) | (y$month[index] != x$month) | (y$day[index] != x$day))

# Check that tow numbers match:
which(y$tow.number[match(x$tow.id, y$tow.id)] != x$tow.number)

# Check that crab numbers are sequential within tows:
x <- sort(x, by = c("year", "month", "day", "tow.number", "crab.number"))
aggregate(x$crab.number, by = x["tow.id"], function(x) if (length(x) == 1) return(1) else return( unique(diff(x))))

# Check indexing variables:
tmp <- aggregate(x$tow.id, by = cbind(substr(x$tow.id, 2, 2), x[c("month", "day", "tow.number")]), function(x) length(unique(x)))
dim(unique(cbind(substr(x$tow.id, 2, 2), x[c("month", "day", "tow.number")])))[1] == length(unique(x$tow.id))

# Convert to 'scbio' object:
xx <- scbio(x)

tows <- unique(x[c("month", "day", "tow.number", "tow.id")])
for (i in 1:nrow(tows)){
   index <- gsub(" ", "", xx$tow.id) == tows$tow.id[i]
   #file <- paste0("GCR", substr(x$year[i], 3, 4), convert.vector(i, to = "a3", fill = "0"), ".txt")      # Old file name format.
   file <- paste0(tows$tow.id[i], ".txt")
   print(paste0("U:/Snow Crab/Stock Assessment 2019/", file))
   write(xx[index, ], file = paste0("U:/Snow Crab/Stock Assessment 2019/", file)) 
}

# Update R and CSV file versions:
update.scbio(year = 2019)

