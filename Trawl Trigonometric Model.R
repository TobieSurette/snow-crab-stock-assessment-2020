clg()

theta <- seq(19.45, 90, len = 1000)
f <- function(x) return(3-(1/sin(pi*x/180)))
 
# Tow duration versus liftoff angle plot:
plot(theta, f(theta), type = "l", lwd = 2, col = "blue")
 
d <- 60
v <- 1

plot(theta, (60/1)*f(theta), type = "n", lwd = 2, 
     col = "blue", xaxs = "i", yaxs = "i", xlab = "", ylab = "")
grid()
lines(theta, (60/1)*f(theta), lwd = 2, lty = "solid", col = "blue")
lines(theta, (60/1.5)*f(theta), lwd = 2, lty = "dashed", col = "blue")
lines(theta, (60/2.0)*f(theta), lwd = 2, lty = "dotted", col = "blue")
 
mtext("Liftoff angle (degrees)", 1, 2.5, cex = 1.4)
mtext("Duration of passive phase (seconds)", 2, 2.5, cex = 1.4)

legend("topleft", 
       legend = c("1.0 m/s", "1.5 m/s", "2.0 m/s"),
       lty = c("solid", "dashed", "dotted"),
       lwd = 2, col = "blue", bg = "white", cex = 1.2)

# Swept area scaling factor:
theta <- seq(19.45, 90, len = 1000)
g <- function(x, vp, vw) return(2*sqrt(2)-(1/tan(pi*x/180)) + (vp/vw) * (3 - (1/sin(pi*x/180))))

d <- 60

windows()
plot(theta, 7.1*60*g(theta, 0.81, 1.0), type = "n", lwd = 2, 
     col = "blue", xaxs = "i", yaxs = "i", xlab = "", ylab = "")
grid()
grid()
lines(theta, 6.8*60*g(theta, 0.81, 1.0), lwd = 2, lty = "solid", col = "blue")
lines(theta, 6.8*60*g(theta, 0.81, 1.5), lwd = 2, lty = "dashed", col = "blue")
lines(theta, 6.8*60*g(theta, 0.81, 2.0), lwd = 2, lty = "dotted", col = "blue")
mtext("Liftoff angle (degrees)", 1, 2.5, cex = 1.4)
mtext("Passive phase swept area (m2)", 2, 2.5, cex = 1.4)
legend("topleft", 
       legend = c("1.0 m/s", "1.5 m/s", "2.0 m/s"),
       lty = c("solid", "dashed", "dotted"),
       lwd = 2, col = "blue", bg = "white", cex = 1.2)

windows()
plot(theta, 7.1*60*g(theta, 0.81, 1.0), type = "n", lwd = 2, 
     col = "blue", xaxs = "i", yaxs = "i", xlab = "", ylab = "")
grid()
grid()
lines(theta, 6.8*60*g(theta, 0.81, 2), lwd = 2, lty = "solid", col = "blue")
lines(theta, 6.3*60*g(theta, 0.81, 1.0), lwd = 2, lty = "dashed", col = "blue")
mtext("Liftoff angle (degrees)", 1, 2.5, cex = 1.4)
mtext("Passive phase swept area (m2)", 2, 2.5, cex = 1.4)
legend("topleft", 
       legend = c("1.0 m/s", "1.5 m/s", "2.0 m/s"),
       lty = c("solid", "dashed", "dotted"),
       lwd = 2, col = "blue", bg = "white", cex = 1.2)
    
    
windows()
r <- (2800 + 6.3*60*g(theta+15, 0.81, 1.0)) / (2800 + 6.8*60*g(theta, 0.81, 2))
plot(theta, r, type = "l", lwd = 2, 
     col = "blue", xaxs = "i", yaxs = "i", xlab = "", ylab = "")

mtext("Liftoff angle (degrees)", 1, 2.5, cex = 1.4)
mtext("Ratio", 2, 2.5, cex = 1.4)
grid()
                     