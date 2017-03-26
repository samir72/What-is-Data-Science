# SpuriousCorrelation.R
# Copyright 2016 by Ernst Henle

# Spurious self-correlation
# Virtual Correlation
# https://en.wikipedia.org/wiki/Spurious_correlation
# Pearson, Karl (1897). "Mathematical Contributions to the Theory of Evolution-On a Form of Spurious Correlation Which May Arise When Indices Are Used in the Measurement of Organs". Proceedings of the Royal Society of London. 60: 489-498
# Reed J. L. (1921) "On the correlation between any two functions and its application to the genaral case of spurious correlation," J. of the Washington Academy of Science, Vol 11 pp 449-455

# Clear Workspace
rm(list=ls())
# Clear Console
cat("\014")

set.seed(3)

par(mgp=c(1, 0.5, 0))
par(mar=c(2, 2, 3, 1))

x <- runif(1000, min=20, max=30)
y <- runif(1000, min=20, max=30)
Corr <- round(cor(x, y),3)
print(Corr)
PlotTitle <- paste("No Correlation between\nx and y (r=", Corr, ")", sep="")
plot(x, y, main=PlotTitle)

# Create a third random variable
z <- runif(1000, min=20, max=30)
print(round(cor(z, x),3))
print(round(cor(z, y),3))

# Normalize x and y by z
yz <- y/z
xz <- x/z
Corr <- round(cor(xz, yz),3)
print(Corr)
PlotTitle <- paste("Spurious Correlation between\nx/z and y/z (r=", Corr, ")", sep="")
plot(xz, yz, xlab= "x/z", ylab="y/z", xlim=c(min(xz), max(xz)), ylim=c(min(yz), max(yz)), main=PlotTitle)
abline(lm(yz~xz), lty=2, lwd=3)

if (F)
{
  plot(c(), c(), xlab= "x/z", ylab="y/z", xlim=c(min(xz), max(xz)), ylim=c(min(yz), max(yz)), main=PlotTitle)
  selection <- z > quantile(z,0.8) & z <= quantile(z,1)
  points(xz[selection], yz[selection], cex=2.5, col=rgb(1, 1, 0, 1))
  selection <- z > quantile(z,0.6) & z <= quantile(z,0.8)
  points(xz[selection], yz[selection], cex=2.0, col=rgb(0.8, 0.8, 0.2, 1))
  selection <- z > quantile(z,0.4) & z <= quantile(z,0.6)
  points(xz[selection], yz[selection], cex=1.5, col=rgb(0.5, 0.5, 0.5, 1))
  selection <- z > quantile(z,0.2) & z <= quantile(z,0.4)
  points(xz[selection], yz[selection], cex=1, col=rgb(0.3, 0.3, 0.7, 1))
  selection <- z <= quantile(z,0.2)
  points(xz[selection], yz[selection], cex=0.5, col=rgb(0, 0, 1, 1))
  abline(lm(yz~xz), lty=2, lwd=3)
} # if

par(mgp=c(3, 1, 0))
par(mar=c(5.1, 4.1, 4.1, 2.1))
