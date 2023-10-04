library(mgcv)

setwd("/Users/icbissell/Documents/research/Eocene_teeth/code/GAM")
total_data <- read.csv("/Users/icbissell/Documents/research/Eocene_teeth/code/GAM/data/teeth_total.csv")

#add column that sets length as the max of width and length
teeth_total$length <- pmax(teeth_total$Height, teeth_total$Width)

length_means <- aggregate(teeth_total$length, list(teeth_total$Age), FUN = mean)
colnames(length_means)[1] = "age"
colnames(length_means)[2] = "length"

# This is the actual model: plotting SST against ageSST, smoothed. 
# k is the smoothing parameter. 
lm <- gam(length~s(age, k=5), data=length_means)
# Plotting the model
plot(lm)
# plots the actual data
points(length_means$age, length_means$length)

par(mfrow=c(2,2))

lm_5 <- gam(length~s(Age, k=5), data=teeth_total)
plot(lm_5)
points(length_means$age, length_means$length-mean(length_means$length), pch = 16)
summary(lm_5)

lm_10 <- gam(length~s(Age, k=10), data=teeth_total)
plot(lm_10)
points(length_means$age, length_means$length-mean(length_means$length), pch = 16)
summary(lm_10)

lm_15 <- gam(length~s(Age, k=15), data=teeth_total)
plot(lm_15)
points(length_means$age, length_means$length-mean(length_means$length), pch = 16)
summary(lm_15)

lm_20 <- gam(length~s(Age, k=20), data=teeth_total)
plot(lm_20)
points(length_means$age, length_means$length-mean(length_means$length), pch = 16)
summary(lm_20)

par(mfrow = c(3,4))
for(i in 1:12) {
  lm_i <- gam(length~s(Age, k=i), data = teeth_total)
  plot(lm_i, main = paste("k = ", i))
  mtext(paste("aic:", round(lm_i$aic, digits = 2)), side=3)
  points(length_means$age, length_means$length-mean(length_means$length), pch = 16)
  summary(lm_i)
}

par(mfrow = c(1,1))
gam_aic <- c()
for(i in 1:20) {
  lm_i <- gam(length~s(Age, k=i), data = teeth_total)
  gam_aic[i] <- lm_i$aic
}

plot(gam_aic, xlab = "k", ylab = "AIC", pch = 16)


lm_5 <- gam(length~s(Age, k=5), data = teeth_total)
lm_6 <- gam(length~s(Age, k=6), data = teeth_total)
lm_7 <- gam(length~s(Age, k=7), data = teeth_total)
lm_8 <- gam(length~s(Age, k=8), data = teeth_total)
lm_9 <- gam(length~s(Age, k=9), data = teeth_total)
lm_10 <- gam(length~s(Age, k=10), data = teeth_total)
lm_11 <- gam(length~s(Age, k=11), data = teeth_total)
lm_12 <- gam(length~s(Age, k=12), data = teeth_total)
lm_13 <- gam(length~s(Age, k=13), data = teeth_total)
lm_14 <- gam(length~s(Age, k=14), data = teeth_total)
lm_15 <- gam(length~s(Age, k=15), data = teeth_total)

anova(lm_5, lm_6, lm_7, lm_8, lm_9, lm_10, lm_11, lm_12, lm_13, lm_14, lm_15, test = "Chisq")


