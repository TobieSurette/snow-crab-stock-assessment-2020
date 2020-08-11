
files <- esonar.file.str(year = 2020)

file.copy(from = files, to = "U:/Snow Crab/Stock Assessment 2020/data/esonar", overwrite = TRUE, copy.date = TRUE)
