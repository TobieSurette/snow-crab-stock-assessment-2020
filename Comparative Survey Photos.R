library(gulf)
library(jpeg)

jpeg <- TRUE

# Determine comparative tows:
files <- scphoto.file.str(year = 2019)
files <- unlist(lapply(strsplit(files, "/"), function(x) x[length(x)]))
files <- unlist(lapply(strsplit(files, "[.]"), function(x) x[1]))
tows <- files[grep("GC", files)]

tows <- tows[tows != "GC336F"]

clg()
if (jpeg){
   jpeg(paste0("U:/Snow Crab/Stock Assessment 2019/Comparative.jpg"),  width = 8.5 * 480, height = 11 * 480, res = 300)
}else{
   windows(width = 13, height = 11)
}

m <- kronecker(matrix(1:length(tows), ncol = 3), matrix(1, nrow = 3, ncol = 4))
m <- rbind(0, cbind(0, m, 0), 0)

layout(m)
par(mar = c(0, 0, 0, 0))
for (i in 1:length(tows)){
   files <- scphoto.file.str(year = 2019)

   # Avalon:
   file <- files[grep(gsub("C", "P", tows[i]), files)]
   pa <- readJPEG(file, native = FALSE)  
   pa <- pa[seq(1, nrow(pa), by = 5), seq(1, ncol(pa), by = 5), ]
   tmp <- array(NA, dim = c(dim(pa)[2], dim(pa)[1], dim(pa)[3]))
      for (j in 1:3) tmp[,,j] <- t(pa[,,j])
      pa <- tmp
   
   
   # Jean Mathieu:
   file <- files[grep(tows[i], files)]
   pc <- readJPEG(file, native = FALSE)  
   pc <- pc[seq(1, nrow(pc), by = 5), seq(1, ncol(pc), by = 5), ]

   # Join two image:
   r <- dim(pa)[1] / dim(pc)[1]
   if (r < 1) pc <- pc[round(seq(1, dim(pc)[1], length = dim(pa)[1])), round(seq(1, dim(pc)[2], length = round(r * dim(pc)[2]))), ]
    
   p <- array(NA, dim = c(nrow(pa), ncol(pa) + ncol(pc), 3))
   for (j in 1:3) p[,,j] <- cbind(pa[,,j], pc[,,j])
   
   plot(c(0, 1), c(0, 1), type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i")
   rasterImage(p, 0, 0, 1, 1)
   
   text(dim(pa)[2] / (dim(p)[2]), 0.90, substr(tows[i], 3, 10), cex = 1.75, col = "grey10", font = 2)
   text(dim(pa)[2] / (dim(p)[2]), 0.90, substr(tows[i], 3, 10), cex = 1.7, col = "grey90")
   
   box(col = "grey", lwd = 2)
}

dev.off()


