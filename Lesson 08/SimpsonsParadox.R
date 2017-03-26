# SimpsonsParadox.R
# Copyright 2016 by Ernst Henle

# Simpsons Paradox
# A consistent trend appears in multiple datasets
# The trend reverses when these datasets are combined
# https://en.wikipedia.org/wiki/Simpson's_paradox

rm(list=ls()) # Clear Workspace
cat("\014") # Clear Console

set.seed(2)

plot(c(), xlab="x", ylab="y", xlim=c(0, 50), ylim=c(0, 120))
x1 <- rnorm(100, mean = 10, sd = 4)
y1 <- 80 + x1 + rnorm(100, mean = 10, sd = 4)
points(x1, y1, bg="pink", pch = 21)
abline(lm(y1~x1))
corr1 <- paste("r =",round(cor(x1,y1), 2))
legend("topright", legend=c(corr1, "", "", ""),         pch=c(21, 22, 24, 21), pt.bg=c("pink", "lightblue", "yellow", "grey"), pt.cex=c(1,0,0,0), lwd=c(1,1,1,3), lty=c(1,0,0,0), seg.len=3, bg="lightgrey")

plot(c(), xlab="x", ylab="y", xlim=c(0, 50), ylim=c(0, 120))
x2 <- rnorm(100, mean = 30, sd = 4)
y2 <- 0 + x2 + rnorm(100, mean = 10, sd = 4)
points(x2, y2, bg="lightblue", pch=22)
abline(lm(y2~x2))
corr2 <- paste("r =",round(cor(x2,y2), 2))
legend("topright", legend=c("", corr2, "", ""),         pch=c(21, 22, 24, 21), pt.bg=c("pink", "lightblue", "yellow", "grey"), pt.cex=c(0,1,0,0), lwd=c(1,1,1,3), lty=c(0,1,0,0), seg.len=3, bg="lightgrey")

plot(c(), xlab="x", ylab="y", xlim=c(0, 50), ylim=c(0, 120))
x3 <- rnorm(100, mean = 20, sd = 4)
y3 <- 40 + x3 + rnorm(100, mean = 10, sd = 4)
points(x3, y3, bg="yellow", pch=24)
abline(lm(y3~x3))
corr3 <- paste("r =",round(cor(x3,y3), 2))
legend("topright", legend=c("", "", corr3, ""),         pch=c(21, 22, 24, 21), pt.bg=c("pink", "lightblue", "yellow", "grey"), pt.cex=c(0,0,1,0), lwd=c(1,1,1,3), lty=c(0,0,1,0), seg.len=3, bg="lightgrey")

plot(c(), xlab="x", ylab="y", xlim=c(0, 50), ylim=c(0, 120))
points(x1, y1, bg="pink", pch = 21)
abline(lm(y1~x1))
points(x2, y2, bg="lightblue", pch=22)
abline(lm(y2~x2))
points(x3, y3, bg="yellow", pch=24)
abline(lm(y3~x3))
x <- c(x1, x2, x3)
y <- c(y1, y2, y3)
abline(lm(y~x), lty=2, lwd=3)
corr <- paste("r =",round(cor(x,y), 2))
legend("topright", legend=c(corr1, corr2, corr3, corr), pch=c(21, 22, 24, 21), pt.bg=c("pink", "lightblue", "yellow", "lightgrey"), pt.cex=c(1,1,1,0), lwd=c(1,1,1,3), lty=c(1,1,1,2), seg.len=3, bg="lightgrey")
