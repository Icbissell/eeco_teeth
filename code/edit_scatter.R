library(zoo)
library(mgcv)
########################################################################

# writeFile <- 'pdf'
# writeFile <- 'jpg'
writeFile <- 'off'

fig.dims <- c(5, 11) #Set Figure-dimensions

if(writeFile == 'pdf') {
  pdf('plots/scatter.pdf', height = fig.dims[1], width = fig.dims[2], useDingbats = FALSE)
}

if(writeFile == 'jpg') {
  jpeg('plots/scatter.jpg', height = fig.dims[1], width = fig.dims[2], units = 'in', res = 300)
}


axis.scale <- 1

par(mfrow = c(1, 2))
par(mar = c(4, 4, 4, 4))

#try correction with linear model fit:
model.iar <- lm(d18O_IAR$IAR ~ d18O_IAR$age)
model.d18 <- lm(d18O_IAR$d18O ~ d18O_IAR$age)

# Detrended series
model.iar <- residuals(model.iar)
model.d18 <- residuals(model.d18)

model <-lm(model.iar ~ model.d18)
newx <- seq(min(model.d18), max(model.d18), length.out = 100)
new_data <- data.frame(model.d18 = newx)

pred <- predict(model, newdata = new_data, se.fit = TRUE)

y_pred <- pred$fit
se <- pred$se.fit

ci <- 1.96 * se
lower <- y_pred - ci
upper <- y_pred + ci

cf1 <- coef(model)
slope.1 <- round(cf1[2], 3)

plot(model.iar~model.d18, xlim = rev(range(model.d18)),
     pch = 16, xlab = '', ylab = '', axes = F)
axis(side = 1, cex.axis = 0.9)
axis(side = 2, cex.axis = 0.9)
box()

lines(newx, y_pred)

# Plot the 95% confidence interval lines
lines(newx, lower, lty = 2)
lines(newx, upper, lty = 2)

mtext(text = expression(paste(delta, ''^'18', 'O', "(per mille)")), side = 1, line = 2.5, cex = 1.0)
mtext(text = expression(paste('IAR residuals')), 
      side = 2, line = 2.1, cex = axis.scale)
mtext(text = "IODP 1553", cex = 1.5, line = 0.5)
r2 <- round(summary(model)$r.squared, 4)
pval <- summary(model)$coefficients[, "Pr(>|t|)"]
pval <- round(pval[2], 4)

text(0.4, 10000, bquote(paste('R'^'2',' = ', .(r2))), cex = 0.8)
text(0.4, 9300, bquote(paste('P = ', .(pval))), cex = 0.8)
text(0.4, 8700, bquote(paste('slope = ', .(slope.1))), cex = 0.8)

mtext("a", side = 3, line = 1, at = 0.9, font = 2, cex = 1.4)

#create model for 596
#try correction with linear model fit:
model.iar <- lm(d18O_IAR_596$IAR ~ d18O_IAR_596$age)
model.d18 <- lm(d18O_IAR_596$d18O ~ d18O_IAR_596$age)

# Detrended series
model.iar <- residuals(model.iar)
model.d18 <- residuals(model.d18)

model <-lm(model.iar ~ model.d18)
newx <- seq(min(model.d18), max(model.d18), length.out = 100)
new_data <- data.frame(model.d18 = newx)

pred <- predict(model, newdata = new_data, se.fit = TRUE)

y_pred <- pred$fit
se <- pred$se.fit

ci <- 1.96 * se
lower <- y_pred - ci
upper <- y_pred + ci

cf1 <- coef(model)
slope.2 <- round(cf1[2], 3)

plot(model.iar~model.d18, xlim = rev(range(model.d18)),
     pch = 16, xlab = '', ylab = '', axes = F)
axis(side = 1, cex.axis = 0.9)
axis(side = 2, cex.axis = 0.9)
box()

lines(newx, y_pred)

# Plot the 95% confidence interval lines
lines(newx, lower, lty = 2)
lines(newx, upper, lty = 2)
mtext(text = expression(paste(delta, ''^'18', 'O', "(per mille)")), side = 1, line = 2.5, cex = 1.0)
mtext(text = expression(paste('IAR residuals')), side = 2, line = 2.1, cex = axis.scale)
mtext(text = "DSDP 596", cex = 1.5, line = 0.5)
r2 <- round(summary(model)$r.squared, 3)
pval <- summary(model)$coefficients[, "Pr(>|t|)"]
pval <- round(pval[2], 22)

text(0.3, 160, bquote(paste('R'^'2',' = ', .(r2))), cex = 0.8)
text(0.3, 150, bquote(paste('P = ', .(pval))), cex = 0.8)
text(0.3, 140, bquote(paste('slope = ', .(slope.2))), cex = 0.8)
mtext("b", side = 3, line = 1, at = 1, font = 2, cex = 1.4)


# close file
if(writeFile != 'off') {
  dev.off()
}



########################################

#calculate factor to scale
# scale.vec <- data.frame(Age = unique(teeth_total$Age)) #replacing this column since it no longer exists
scale.vec <- data.frame(Age = unique(teeth_total$nieder_ages))

scale.vec$fac <- 0
for (i in 1:length(scale.vec$Age)) {
  sub.t <- subset(teeth_total, nieder_ages == scale.vec$Age[i])
  scale.vec$fac[i] <- sum(pmin(sub.t$Height, sub.t$Width) > 150) / length(sub.t$length)
}


#rescale 596 to match 1553
scale.vec <- scale.vec[order(scale.vec$Age), ]
d18O_IAR_596 <- d18O_IAR_596[order(d18O_IAR_596$age), ]
scale.596 <- data.frame(Age = d18O_IAR_596$age, scale.iar = d18O_IAR_596$IAR, d18O = d18O_IAR_596$d18O)
for(i in 1:length(scale.596$Age)) {
  scale.596$scale.iar[i] <- (
    scale.596$scale.iar[i] /
      scale.vec$fac[which(abs(scale.vec$Age-scale.596$Age[i])==min(abs(scale.vec$Age-scale.596$Age[i])))]
  )
}

# writeFile <- 'pdf'
# writeFile <- 'jpg'
writeFile <- 'off'

fig.dims <- c(5, 11) #Set Figure-dimensions

if(writeFile == 'pdf') {
  pdf('plots/scatter_scale.pdf', height = fig.dims[1], width = fig.dims[2], useDingbats = FALSE)
}

if(writeFile == 'jpg') {
  jpeg('plots/scatter_scale.jpg', height = fig.dims[1], width = fig.dims[2], units = 'in', res = 300)
}


axis.scale <- 1

par(mfrow = c(1, 2))
par(mar = c(4, 4, 4, 4))

#try correction with linear model fit:
model.iar <- lm(d18O_IAR$IAR ~ d18O_IAR$age)
model.d18 <- lm(d18O_IAR$d18O ~ d18O_IAR$age)

# Detrended series
model.iar <- residuals(model.iar)
model.d18 <- residuals(model.d18)

model <-lm(model.iar ~ model.d18)
newx <- seq(min(model.d18), max(model.d18), length.out = 100)
new_data <- data.frame(model.d18 = newx)

pred <- predict(model, newdata = new_data, se.fit = TRUE)

y_pred <- pred$fit
se <- pred$se.fit

ci <- 1.96 * se
lower <- y_pred - ci
upper <- y_pred + ci

cf1 <- coef(model)
slope.1 <- round(cf1[2], 3)

plot(model.iar~model.d18, xlim = rev(range(model.d18)),
     pch = 16, xlab = '', ylab = '', axes = F)
axis(side = 1, cex.axis = 0.9)
axis(side = 2, cex.axis = 0.9)
box()

lines(newx, y_pred)

# Plot the 95% confidence interval lines
lines(newx, lower, lty = 2)
lines(newx, upper, lty = 2)

mtext(text = expression(paste(delta, ''^'18', 'O', "(per mille)")), side = 1, line = 2.5, cex = 1.0)
mtext(text = expression(paste('IAR residuals')), 
      side = 2, line = 2.1, cex = axis.scale)
mtext(text = "IODP 1553", cex = 1.5, line = 0.5)
r2 <- round(summary(model)$r.squared, 4)
pval <- summary(model)$coefficients[, "Pr(>|t|)"]
pval <- round(pval[2], 4)

text(0.4, 10000, bquote(paste('R'^'2',' = ', .(r2))), cex = 0.8)
text(0.4, 9300, bquote(paste('P = ', .(pval))), cex = 0.8)
text(0.4, 8700, bquote(paste('slope = ', .(slope.1))), cex = 0.8)

mtext("a", side = 3, line = 1, at = 0.9, font = 2, cex = 1.4)

#create model for 596
#try correction with linear model fit:
model.iar <- lm(scale.596$scale.iar ~ scale.596$Age)
model.d18 <- lm(scale.596$d18O ~ scale.596$Age)

# Detrended series
model.iar <- residuals(model.iar)
model.d18 <- residuals(model.d18)

model <-lm(model.iar ~ model.d18)
newx <- seq(min(model.d18), max(model.d18), length.out = 100)
new_data <- data.frame(model.d18 = newx)

pred <- predict(model, newdata = new_data, se.fit = TRUE)

y_pred <- pred$fit
se <- pred$se.fit

ci <- 1.96 * se
lower <- y_pred - ci
upper <- y_pred + ci

cf1 <- coef(model)
slope.2 <- round(cf1[2], 3)

plot(model.iar~model.d18, xlim = rev(range(model.d18)),
     pch = 16, xlab = '', ylab = '', axes = F)
axis(side = 1, cex.axis = 0.9)
axis(side = 2, cex.axis = 0.9)
box()

lines(newx, y_pred)

# Plot the 95% confidence interval lines
lines(newx, lower, lty = 2)
lines(newx, upper, lty = 2)
mtext(text = expression(paste(delta, ''^'18', 'O', "(%)")), side = 1, line = 2.5, cex = 1.0)
mtext(text = expression(paste('IAR residuals')), side = 2, line = 2.1, cex = axis.scale)
mtext(text = "DSDP 596", cex = 1.5, line = 0.5)
r2 <- round(summary(model)$r.squared, 3)
pval <- summary(model)$coefficients[, "Pr(>|t|)"]
pval <- round(pval[2], 22)

text(0.2, 20000, bquote(paste('R'^'2',' = ', .(r2))), cex = 0.8)
text(0.2, 19000, bquote(paste('P = ', .(pval))), cex = 0.8)
text(0.2, 18000, bquote(paste('slope = ', .(slope.2))), cex = 0.8)
mtext("b", side = 3, line = 1, at = 1, font = 2, cex = 1.4)


# close file
if(writeFile != 'off') {
  dev.off()
}


