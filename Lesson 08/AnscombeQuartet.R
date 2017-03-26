# AnscombeQuartet.R
# Copyright 2016 by Ernst Henle

# Anscombe's Quartet
# Four different datasets have the following measures
# Mean of x:  9
# Sample variance of x: 11
# Mean of y:	7.50
# Sample variance of y:  4.125
# Correlation between x and y:  0.816
# Linear regression line:  y = 3.00 + 0.500x

rm(list=ls()) # Clear Workspace
cat("\014") # Clear Console

# Anscombe's Quartet Data
x1 <- c(10,    8,   13,    9,   11,   14,    6,    4,   12,    7,    5)
y1 <- c( 8.04, 6.95, 7.58, 8.81, 8.33, 9.96, 7.24, 4.26,10.84, 4.82, 5.68)
x2 <- c(10,    8,   13,    9,   11,   14,    6,    4,   12,    7,    5)
y2 <- c( 9.14, 8.14, 8.74, 8.77, 9.26, 8.10, 6.13, 3.10, 9.13, 7.26, 4.74)
x3 <- c(10,    8,   13,    9,   11,   14,    6,    4,   12,    7,    5)
y3 <- c( 7.46, 6.77,12.74, 7.11, 7.81, 8.84, 6.08, 5.39, 8.15, 6.42, 5.73)
x4 <- c( 8,    8,    8,    8,    8,    8,    8,   19,    8,    8,    8)
y4 <- c( 6.58, 5.76, 7.71, 8.84, 8.47, 7.04, 5.25,12.50, 5.56, 7.91, 6.89)

# Set layout for 4 plots
par(mfrow=c(2,2))
suppressWarnings(par(mgp=c(0.25, -1.25, 0)))
par(mar=c(1.5, 1.5, 0.5, 0.5))

# Create linear regression model for each dataset
# Plot each dataset along with its model
ml1 <- lm(y1 ~ x1)
plot(x1, y1, xlim=c(0, 20), ylim=c(0, 14), cex=3, bg=rgb(.6,.6,.1,.3), pch=21, tck=.03)
abline(ml1)
ml2 <- lm(y2 ~ x2)
plot(x2, y2, xlim=c(0, 20), ylim=c(0, 14), cex=3, bg=rgb(.6,.6,.1,.3), pch=21, tck=.03)
abline(ml2)
ml3 <- lm(y3 ~ x3)
plot(x3, y3, xlim=c(0, 20), ylim=c(0, 14), cex=3, bg=rgb(.6,.6,.1,.3), pch=21, tck=.03)
abline(ml3)
ml4 <- lm(y4 ~ x4)
plot(x4, y4, xlim=c(0, 20), ylim=c(0, 14), cex=3, bg=rgb(.6,.6,.1,.3), pch=21, tck=.03)
abline(ml4)

Measure=c("mean(x)", 'mean(y)', 'var(x)', "var(y)", 'cor(x, y)', 'intercept(y~x)', 'slope(y~x)')

x1y1 <- rep(NA, 7)
x1y1[1] <- round(mean(x1), 2)
x1y1[2] <- round(mean(y1), 2)
x1y1[3] <- round(var(x1), 2)
x1y1[4] <- round(var(y1), 2)
x1y1[5] <- round(cor(x1, y1), 2)
x1y1[6] <- round(lm(y1~x1)$coefficients[1], 2)
x1y1[7] <- round(lm(y1~x1)$coefficients[2], 2)

x2y2 <- rep(NA, 7)
x2y2[1] <- round(mean(x2), 2)
x2y2[2] <- round(mean(y2), 2)
x2y2[3] <- round(var(x2), 2)
x2y2[4] <- round(var(y2), 2)
x2y2[5] <- round(cor(x2, y2), 2)
x2y2[6] <- round(lm(y2~x2)$coefficients[1], 2)
x2y2[7] <- round(lm(y2~x2)$coefficients[2], 2)

x3y3 <- rep(NA, 7)
x3y3[1] <- round(mean(x3), 2)
x3y3[2] <- round(mean(y3), 2)
x3y3[3] <- round(var(x3), 2)
x3y3[4] <- round(var(y3), 2)
x3y3[5] <- round(cor(x3, y3), 2)
x3y3[6] <- round(lm(y3~x3)$coefficients[1], 2)
x3y3[7] <- round(lm(y3~x3)$coefficients[2], 2)

x4y4 <- rep(NA, 7)
x4y4[1] <- round(mean(x4), 2)
x4y4[2] <- round(mean(y4), 2)
x4y4[3] <- round(var(x4), 2)
x4y4[4] <- round(var(y4), 2)
x4y4[5] <- round(cor(x4, y4), 2)
x4y4[6] <- round(lm(y4~x4)$coefficients[1], 2)
x4y4[7] <- round(lm(y4~x4)$coefficients[2], 2)

print(data.frame(row.names = Measure, x1y1, x2y2, x3y3, x4y4))

# Set layout to default
par(mfrow=c(1,1))
par(mgp=c(3, 1, 0))
par(mar=c(5.1, 4.1, 4.1, 2.1))
