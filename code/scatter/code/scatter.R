library(zoo)
library(mgcv)

#set code directory
setwd("/Users/icbissell/Documents/research/Eocene_teeth/code/scatter")

#set path to Westerhold oxygen data
o_data <- read.csv("data/Westerhold_2020_Oxygen_Carbon_smooth.csv")
#set path to IAR data
iar <- read.csv("data/iar.csv")
#set path to mean length data
mean_length <- read.csv("data/length_means.csv")
#import DSDP 596 IAR data
IAR.596 <- read.csv("data/DSDP_596_Fish_Accumulation_siteid_1_132.csv")
IAR.596 <- IAR.596[(IAR.596$age > min(mean_length$age)) & (IAR.596$age < max(mean_length$age)), ]


#set path to total length data
teeth_total <- read.csv("data/teeth_total.csv")

#prepare temperature and length tests
o_df1 <- data.frame(age = as.numeric(o_data$Age_Ma), d18O = as.numeric(o_data$d18O_loess_smooth))
o_df1 = o_df1[(o_df1$age >= min(mean_length$age)) & (o_df1$age <= max(mean_length$age)), ]

#compute rolling mean of temperature data
o_roll <- data.frame(rollmean(o_df1, 100))

match_d18O <- c()
for(i in 1:length(iar$age)) {
  match_d18O[i] <- o_roll$d18O[which(abs(o_roll$age-iar$age[i])==min(abs(o_roll$age-iar$age[i])))]
}

d18O_IAR <- data.frame(age = iar$age, d18O = match_d18O, IAR = iar$IAR)


match_d18O_596 <- c()
for(i in 1:length(IAR.596$age)) {
  match_d18O_596[i] <- o_roll$d18O[which(abs(o_roll$age-IAR.596$age[i])==min(abs(o_roll$age-IAR.596$age[i])))]
}

d18O_IAR_596 <- data.frame(age = IAR.596$age, d18O = match_d18O_596, IAR = IAR.596$ich_accum)


#cut to points <62 million years
d18O_IAR <- d18O_IAR[-which(d18O_IAR$age > 62), ]
summary(lm(log(d18O_IAR$IAR) ~ d18O_IAR$d18O))

par(mfrow = c(1, 2))
par(mar = c(5, 4, 4, 2)) 
model <-lm(log(IAR)~d18O, d18O_IAR)
newx <- seq(min(d18O_IAR$d18O), max(d18O_IAR$d18O), by = 0.01)
new_data <- data.frame(d18O = newx)
pred <- predict(model, newdata = new_data, se.fit = TRUE)

y_pred <- pred$fit
se <- pred$se.fit
ci <- 1.96 * se  # Assuming a normal distribution, 1.96 corresponds to a 95% confidence level
lower <- y_pred - ci
upper <- y_pred + ci
cf1 <- coef(model)
slope.1 <- cf1[2]
axis.scale <- 0.8

plot(log(d18O_IAR$IAR)~d18O_IAR$d18O, xlim = rev(range(d18O_IAR$d18O)),
     pch = 16, xlab = '', ylab = '')
lines(newx, lower, lty = 2)
lines(newx, upper, lty = 2)
abline(model)
mtext(text = expression(paste(delta, ''^'18', 'O')), side = 1, line = 2.5, cex = 1.0)
mtext(text = expression(paste('log(IAR) [ich cm'^'-2','Myr'^'-1', ']')), side = 2, line = 2.5, cex = axis.scale)
mtext(text = "IODP 1553", cex = 1)
r2 <- round(summary(model)$r.squared, 3)
pval <- summary(model)$coefficients[, "Pr(>|t|)"]
pval <- pval[2]
text(0.4, 9.75, bquote(paste('R'^'2',' = ', .(r2))), cex = 0.8)
text(0.4, 9.55, bquote(paste('P = ', .(pval))), cex = 0.8)
text(0.4, 9.35, bquote(paste('slope = ', .(slope.1))), cex = 0.8)


#create model for 596
model.2 <-lm(log(IAR)~d18O, data = d18O_IAR_596)
newx.2 <- seq(min(d18O_IAR_596$d18O), max(d18O_IAR_596$d18O), by = 0.01)
newer_data <- data.frame(d18O = newx.2)
pred.2 <- predict(model.2, newdata = newer_data, se.fit = TRUE)

y_pred <- pred.2$fit
se <- pred.2$se.fit
ci <- 1.96 * se  # Assuming a normal distribution, 1.96 corresponds to a 95% confidence level
lower <- y_pred - ci
upper <- y_pred + ci
cf2 <- coef(model.2)
slope.2 <- cf2[2]

plot(log(d18O_IAR_596$IAR) ~ d18O_IAR_596$d18O, xlim = rev(range(d18O_IAR_596$d18O)), 
     pch = 16, xlab = '', ylab = '')
lines(newx.2, lower, lty = 2)
lines(newx.2, upper, lty = 2)
abline(model.2)
mtext(text = expression(paste(delta, ''^'18', 'O')), side = 1, line = 2.5, cex = 1.0)
mtext(text = expression(paste('log(IAR) [ich cm'^'-2','Myr'^'-1', ']')), side = 2, line = 2.5, cex = axis.scale)
mtext(text = "DSDP 596", cex = 1)
r2 <- round(summary(model.2)$r.squared, 3)
pval <- summary(model.2)$coefficients[, "Pr(>|t|)"]
pval <- pval[2]
text(0.4, 5.5, bquote(paste('R'^'2',' = ', .(r2))), cex = 0.8)
text(0.4, 5.3, bquote(paste('P = ', .(pval))), cex = 0.8)
text(0.4, 5.1, bquote(paste('slope = ', .(slope.2))), cex = 0.8)


# teeth_total$length <- teeth_total$length * 0.52/0.73
# teeth_total$Width <- teeth_total$Width * 0.52/0.73
# teeth_total$Height <- teeth_total$Height * 0.52/0.73


#calculate factor to scale
# scale.vec <- data.frame(Age = unique(teeth_total$Age))
# scale.vec$fac <- 0
# for (i in 1:length(scale.vec$Age)) {
#   sub.t <- subset(teeth_total, Age == scale.vec$Age[i])
#   scale.vec$fac[i] <- sum(sub.t$Height > 106 & sub.t$length > 106) / length(sub.t$length)
# }
# scale.vec <- scale.vec[-which(scale.vec$Age > 62), ]
# scale.vec <- scale.vec[-which(scale.vec$Age < 47), ]
# 
# #rescale 1553 to match 596
# scale.vec <- scale.vec[order(scale.vec$Age), ]
# d18O_IAR <- d18O_IAR[order(d18O_IAR$age), ]
# scale.1553 <- data.frame(Age = scale.vec$Age, scale.iar = d18O_IAR$IAR * scale.vec$fac, d18O = d18O_IAR$d18O)
# 
# #replot with scale
# par(mfrow = c(1, 2))
# par(mar = c(5, 4, 4, 2)) 
# model <-lm(scale.iar~d18O, scale.1553)
# newx <- seq(min(scale.1553$d18O), max(scale.1553$d18O), by = 0.01)
# new_data <- data.frame(d18O = newx)
# pred <- predict(model, newdata = new_data, se.fit = TRUE)
# 
# y_pred <- pred$fit
# se <- pred$se.fit
# ci <- 1.96 * se  # Assuming a normal distribution, 1.96 corresponds to a 95% confidence level
# lower <- y_pred - ci
# upper <- y_pred + ci
# cf1 <- coef(model)
# slope.1 <- cf1[2]
# axis.scale <- 0.8
# 
# plot(scale.1553$scale.iar~scale.1553$d18O, xlim = rev(range(scale.1553$d18O)),
#      pch = 16, xlab = '', ylab = '')
# lines(newx, lower, lty = 2)
# lines(newx, upper, lty = 2)
# abline(model)
# mtext(text = expression(paste(delta, ''^'18', 'O')), side = 1, line = 2.5, cex = 1.0)
# mtext(text = expression(paste('log(IAR) [ich cm'^'-2','Myr'^'-1', ']')), side = 2, line = 2.5, cex = axis.scale)
# mtext(text = "IODP 1553", cex = 1)
# r2 <- round(summary(model)$r.squared, 3)
# pval <- summary(model)$coefficients[, "Pr(>|t|)"]
# pval <- pval[2]
# 
# 
# #create model for 596
# model.2 <-lm(IAR~d18O, data = d18O_IAR_596)
# newx.2 <- seq(min(d18O_IAR_596$d18O), max(d18O_IAR_596$d18O), by = 0.01)
# newer_data <- data.frame(d18O = newx.2)
# pred.2 <- predict(model.2, newdata = newer_data, se.fit = TRUE)
# 
# y_pred <- pred.2$fit
# se <- pred.2$se.fit
# ci <- 1.96 * se  # Assuming a normal distribution, 1.96 corresponds to a 95% confidence level
# lower <- y_pred - ci
# upper <- y_pred + ci
# cf2 <- coef(model.2)
# slope.2 <- cf2[2]
# 
# plot(d18O_IAR_596$IAR ~ d18O_IAR_596$d18O, xlim = rev(range(d18O_IAR_596$d18O)), 
#      pch = 16, xlab = '', ylab = '')
# lines(newx.2, lower, lty = 2)
# lines(newx.2, upper, lty = 2)
# abline(model.2)
# mtext(text = expression(paste(delta, ''^'18', 'O')), side = 1, line = 2.5, cex = 1.0)
# mtext(text = expression(paste('log(IAR) [ich cm'^'-2','Myr'^'-1', ']')), side = 2, line = 2.5, cex = axis.scale)
# mtext(text = "DSDP 596", cex = 1)
# r2 <- round(summary(model.2)$r.squared, 3)
# pval <- summary(model.2)$coefficients[, "Pr(>|t|)"]
# pval <- pval[2]
