#Clear workspace
rm(list=ls())
x <- c(0,1,2,3,4,5,6,7,8,9,10)
y <- c(10,0,40,50,40,50,40,50,90,100,80)
x <- c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
y <- c(0.1,0,0.4,0.5,0.4,0.5,0.4,0.5,0.9,1,0.8)
plot(x,y)
xmean <- mean(x)
ymean <-mean(y)
xsd <- sd(x)
ysd <- sd(y)
xzbase <- (x - xmean)/xsd#calculating zscore directly
yzbase <- (y - ymean)/ysd#calculating zscore directly
xzbaseyzbase <- xzbase*yzbase
sxzbaseyzbase <- sum(xzbaseyzbase)
rbase <- sxzbaseyzbase/(length(x)-1)
b <- rbase*(ysd/xsd)
Int_Coeff <- lm(y~x)
abline(lm(y~x))#regression line
#Alternate way of calculating z score
xz <- scale(x, center = TRUE, scale = TRUE)#Functiona to calculate z score
yz <- scale(y, center = TRUE, scale = TRUE)
xzxy <- xz*yz
sxzyz <-sum(xzxy)
r <- sxzyz/(length(x)-1)
cor(x,y)
rcorr(x,y, type = c("pearson"))
rcorr(x,y)

#yhead <- a + bx#b = regression coefficient
#b = r * (sdy/sdx)
#a = ymean - b*xmean
a <- ymean -b*xmean
yhead <- a + b*0.2#b

yhead <- 3 + 3.3*7
