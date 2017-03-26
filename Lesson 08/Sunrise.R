# Sunrise.R
# Copyright 2016 by Ernst Henle

# Does the sun rise over time in Seattle? 
# Observe height of sun (altitude) in the sky between 7:30 AM and 1:00 PM.
# Repeat results at different times of the year from spring to winter.
# Plot data and get correlations.
# The answer to the question depends on how we interpret the data.
# The answer to the question depends on the specific formulation of the question.
# Altitudes are available here:  http://www.suncalc.org/#/47.6062,-122.3321,11/

rm(list=ls()) # Clear Workspace
cat("\014") # Clear Console

# Time of day when measurements are made
Hour <- c(7.5, 8, 8.5, 9, 9.5, 10, 10.5, 11, 11.5, 12, 12.5, 13)

# Get correlation and plot for rising sun on summer solstice
AltDegJun21 <- c(20.1, 25.0, 30.1, 35.1, 40.1, 45.1, 49.9, 54.4, 58.5, 61.9, 64.4, 65.7)
Jun21Cor <- paste("r =",round(cor(Hour, AltDegJun21), 2))
plot(Hour, AltDegJun21, bg="yellow", pch = 24)
abline(lm(AltDegJun21~Hour))
legend("bottom", legend=Jun21Cor, pch=24, pt.bg="yellow", pt.cex=1, lty=1, seg.len=3, bg="lightgrey")

# Get correlation and plot for rising sun on spring equinox
AltDegSep21 <- c(4.9, 9.9, 14.8, 19.6, 24.2, 28.5, 32.4, 35.8, 38.7, 40.9, 42.3, 42.8)
Sep21Cor <- paste("r =",round(cor(Hour, AltDegSep21), 2))
plot(Hour, AltDegSep21, bg="lightblue", pch = 22)
abline(lm(AltDegSep21~Hour))
legend("bottom", legend=Sep21Cor, pch=22, pt.bg="lightblue", pt.cex=1, lty=1, seg.len=3, bg="lightgrey")

# Get correlation and plot for rising sun on winter solstice
AltDegDec21 <- c(0, 0, 3.7, 7.3, 10.5, 13.3, 15.6, 17.3, 18.4, 18.9,18.8,18.0)
Dec21Cor <- paste("r =",round(cor(Hour, AltDegDec21),2))
plot(Hour, AltDegDec21, bg="pink", pch = 21)
abline(lm(AltDegDec21~Hour))
legend("bottom", legend=Dec21Cor, pch=21, pt.bg="pink", pt.cex=1, lty=1, seg.len=3, bg="lightgrey")

# We combine these results by time-of-day
TimeOfDay <- c(Hour, Hour, Hour)
AltDeg <- c(AltDegJun21, AltDegSep21, AltDegDec21)
Cor <- paste("r =",round(cor(TimeOfDay, AltDeg), 2))
plot(TimeOfDay, AltDeg, cex=0)
points(Hour, AltDegDec21, bg="pink", pch = 21)
abline(lm(AltDegDec21~Hour))
points(Hour, AltDegSep21, bg="lightblue", pch = 22)
abline(lm(AltDegSep21~Hour))
points(Hour, AltDegJun21, bg="yellow", pch = 24)
abline(lm(AltDegJun21~Hour))
abline(lm(AltDeg~TimeOfDay), lty=2, lwd=3)
legend("topleft", legend=Cor, pch=21, pt.bg="lightgrey", pt.cex=0, lty=2, lwd=3, seg.len=3, bg="lightgrey")

# We combine these results by time-of-day and scale the results
TimeOfDay <- c(Hour, Hour, Hour)
AltDegJun21Scaled <- scale(AltDegJun21)
AltDegSep21Scaled <- scale(AltDegSep21)
AltDegDec21Scaled <- scale(AltDegDec21)
AltDeg <- c(AltDegJun21Scaled, AltDegSep21Scaled, AltDegDec21Scaled)
Cor <- paste("r =",round(cor(TimeOfDay, AltDeg), 2))
plot(TimeOfDay, AltDeg, cex=0)
points(Hour, AltDegJun21Scaled, bg="pink", pch = 21)
abline(lm(AltDegJun21Scaled~Hour))
points(Hour, AltDegSep21Scaled, bg="lightblue", pch = 22)
abline(lm(AltDegSep21Scaled~Hour))
points(Hour, AltDegDec21Scaled, bg="yellow", pch = 24)
abline(lm(AltDegDec21Scaled~Hour))
abline(lm(AltDeg~TimeOfDay), lty=2, lwd=3)
legend("topleft", legend=Cor, pch=21, pt.bg="lightgrey", pt.cex=0, lty=2, lwd=3, seg.len=3, bg="lightgrey")

# But here are the "real" results.  Sun height vs Time
# We combine these results by time-of-year
Jun21InDays <- 31*3+30+28
Jun21InHours <- Hour + Jun21InDays*24
Sep21InDays <- 31*5+30*2+28
Sep21InHours <- Hour + Sep21InDays*24
Dec21InDays <- 31*6+30*4+28
Dec21InHours <- Hour + Dec21InDays*24
DateTimeInHours <- c(Jun21InHours, Sep21InHours, Dec21InHours)
AltDeg <- c(AltDegJun21, AltDegSep21, AltDegDec21)
Cor <- paste("r =",round(cor(DateTimeInHours, AltDeg), 2))
plot(DateTimeInHours, AltDeg, cex=0)
points(Dec21InHours, AltDegDec21, bg="pink", pch = 21)
points(Sep21InHours, AltDegSep21, bg="lightblue", pch = 22)
points(Jun21InHours, AltDegJun21, bg="yellow", pch = 24)
abline(lm(AltDeg~DateTimeInHours), lty=2, lwd=3)
legend("top", legend=Cor, pch=21, pt.bg="lightgrey", pt.cex=0, lty=2, lwd=3, seg.len=3, bg="lightgrey")
