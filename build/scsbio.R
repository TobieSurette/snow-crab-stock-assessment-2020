library(gulf.data)

x <- read.table("data/raw/scs.bio.2020.csv", header = TRUE, sep =",", stringsAsFactors = FALSE, fileEncoding = "Windows-1252")
names(x)          <- gsub("_", ".", tolower(names(x)))
x$tow.id          <- deblank(toupper(x$gpnumber))
x$tow.number      <- x$trawl.number
x$sex             <- as.numeric(gsub("[*]", "", x$sex))
x$egg.colour      <- as.numeric(gsub("[*]", "", x$egg.color))
x$eggs.remaining  <- as.numeric(gsub("[*]", "", x$percent.eggs))
x$gonad.colour    <- as.numeric(gsub("[*]", "", x$gonade))
x$shell.condition <- deblank(gsub("[*]", "", x$shell.condition))

# Extract time stamp:
x$time <- unlist(lapply(strsplit(x$timestamp, " "), function(x) x[2]))

x$missing.legs <- apply(x[, c(paste0("l", 1:5), paste0("l", 1:5))], 1, paste0, collapse = "")
x$year  <- as.numeric(substr(x$date, 1, 4))
x$month <- as.numeric(substr(x$date, 6, 7))
x$day   <- as.numeric(substr(x$date, 9, 10))
x$date  <- as.character(date(x))

x$shell.condition.mossy <- ""
x$shell.condition.mossy[substr(toupper(x$shell.condition), 2, 2) == "M"] <- "M"
x$shell.condition <- as.numeric(substr(x$shell.condition, 1, 1))
if (!("tag.number" %in% names(x)))    x$tag.number <- NA
if (!("weight" %in% names(x)))        x$weight <- NA
if (!("durometer" %in% names(x)))     x$durometer <- NA
if (!("trap.code" %in% names(x)))     x$trap.code <- NA
if (!("abdomen.width" %in% names(x))) x$abdomen.width <- NA

# Remove irrelevant data:
x <- x[order(time(x)), ]
index <- is.na(x$carapace.width) & is.na(x$shell.condition) & is.na(x$gonad.colour) & is.na(x$egg.colour) & is.na(x$eggs.remaining)
x <- x[which(!index), ]
x <- x[x$sex %in% c(1:2), ]

# Fix comments:
x$comment <- paste0(toupper(substr(x$comment, 1, 1)), tolower(substr(x$comment, 2, nchar(x$comment))))
x$comment[x$comment == "Vien de mu‚"] <- "Vient de muer"
x$comment[x$comment == "Pattes bris‚es"] <- "Pattes brisees"

# Check missing legs:
table(unlist((strsplit(x$missing.legs, ""))))

# Chela height corrections:
x$chela.height[x$chela.height == "."] <- ""
x$chela.height <- as.numeric(x$chela.height)

# Check for inconsistencies is sex measurements:
which(x$sex == 2 & !is.na(x$chela.height))
index <- which((x$sex == 1) & ((!is.na(x$gonad.colour)) | (!is.na(x$egg.colour)) | (!is.na(x$eggs.remaining))))

# Missing eggs remaining:
x$eggs.remaining[which((x$egg.colour == 4) & is.na(x$eggs.remaining))] <- 0

# Carapace width corrections:
x$carapace.width <- abs(x$carapace.width)

# Female size corrections:
x$carapace.width[which(x$sex == 2 & is.mature.scsbio(x) & x$carapace.width < 30)] <- NA


# Delete irrelevant data and re-order variables:
vars <- c("date", "time", "tow.id", "tow.number", "crab.number", "sex",
          "carapace.width", "chela.height", "abdomen.width", "durometer", "weight",
          "shell.condition", "missing.legs", "shell.condition.mossy",
          "gonad.colour", "egg.colour", "eggs.remaining", "samplers", "tag.number", "trap.code", "comment")
x <- x[vars]

write.csv(x, file = "data/scs.bio.2020.csv", row.names = FALSE)

# Update gulf.data repo:
if (file.exists("C:/Users/SuretteTJ/Desktop/gulf.data")){
   write.csv(x, file = "C:/Users/SuretteTJ/Desktop/gulf.data/inst/extdata/scs.bio.2020.csv", row.names = FALSE)
}

# x <- write.csv("data/scs.bio.2020.csv", header = TRUE, row.names = FALSE)

# # Load tow data:
# y <- read.scset(year = 2020)
#
# # Check for data with non-valid tows:
# index <- match(x$tow.id, y$tow.id)
# table(y$valid[index])
#
# # Check that dates match:
# which((y$year[index] != x$year) | (y$month[index] != x$month) | (y$day[index] != x$day))
#
# # Check that tow numbers match:
# which(y$tow.number[match(x$tow.id, y$tow.id)] != x$tow.number)
#
# # Check that crab numbers are sequential within tows:
# x <- sort(x, by = c("year", "month", "day", "tow.number", "crab.number"))
# aggregate(x$crab.number, by = x["tow.id"], function(x) if (length(x) == 1) return(1) else return( unique(diff(x))))
#
# # Check indexing variables:
# tmp <- aggregate(x$tow.id, by = cbind(substr(x$tow.id, 2, 2), x[c("month", "day", "tow.number")]), function(x) length(unique(x)))
# dim(unique(cbind(substr(x$tow.id, 2, 2), x[c("month", "day", "tow.number")])))[1] == length(unique(x$tow.id))
#
# # Convert to 'scbio' object:
# xx <- scsbio(x)
#
# tows <- unique(x[c("month", "day", "tow.number", "tow.id")])
# for (i in 1:nrow(tows)){
#    index <- gsub(" ", "", xx$tow.id) == tows$tow.id[i]
#    #file <- paste0("GCR", substr(x$year[i], 3, 4), convert.vector(i, to = "a3", fill = "0"), ".txt")      # Old file name format.
#    file <- paste0(tows$tow.id[i], ".txt")
#    print(paste0("U:/Snow Crab/Stock Assessment 2019/", file))
#    write(xx[index, ], file = paste0("U:/Snow Crab/Stock Assessment 2019/", file))
# }
#
# # Update R and CSV file versions:
# update.scbio(year = 2019)

