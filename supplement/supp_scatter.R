
# writeFile <- 'pdf'
# writeFile <- 'jpg'
# writeFile <- 'png'
writeFile <- 'off'

fig.dims <- c(7, 11) #Set Figure-dimensions

if(writeFile == 'pdf') {
  pdf('supplement_plot/sup_scatter.pdf', height = fig.dims[1], width = fig.dims[2], useDingbats = FALSE)
}

if(writeFile == 'jpg') {
  jpeg('supplement_plot/sup_scatter.jpg', height = fig.dims[1], width = fig.dims[2], units = 'in', res = 300)
}

if(writeFile == 'png') {
  jpeg('supplement_plot/sup_scatter.png', height = fig.dims[1], width = fig.dims[2], units = 'in', res = 300)
}

##### Calculate d18O around the IAR values #####
match_d18O <- c()

for(i in 1:length(chas_dataset$nieder_ages)) {
  ind <- which(abs(o_roll$age-chas_dataset$nieder_ages[i])==min(abs(o_roll$age-chas_dataset$nieder_ages[i])))
  print(ind)
  if (ind == 1) {
    match_d18O[i] <- o_roll$d18O[ind]
  }
  
  else{
    match_d18O[i] <- mean(o_roll$d18O[(ind-2):(ind+2)])
  }
  
}

d18O_IAR_nieder <- data.frame(age = chas_dataset$nieder_ages, d18O = match_d18O,
                       IAR = chas_dataset$nieder_IAR)


match_d18O <- c()

for(i in 1:length(chas_dataset$shipboard_ages)) {
  ind <- which(abs(o_roll$age-chas_dataset$shipboard_ages[i])==min(abs(o_roll$age-chas_dataset$shipboard_ages[i])))
  print(ind)
  if (ind == 1) {
    match_d18O[i] <- o_roll$d18O[ind]
  }
  
  else{
    match_d18O[i] <- mean(o_roll$d18O[(ind-2):(ind+2)])
  }
  
}

d18O_IAR_ship <- data.frame(age = chas_dataset$nieder_ages, d18O = match_d18O,
                       IAR = chas_dataset$shipboard_IAR)



par(mfrow = c(1, 2))
par(mar = c(5, 4, 4, 2))

#create model for shipboard
model <-lm(log(IAR)~d18O, d18O_IAR_ship)
newx <- seq(min(d18O_IAR_ship$d18O), max(d18O_IAR_ship$d18O), by = 0.01)
new_data <- data.frame(d18O = newx)
pred <- predict(model, newdata = new_data, se.fit = TRUE)

y_pred <- pred$fit
se <- pred$se.fit
ci <- 1.96 * se  # Assuming a normal distribution, 1.96 corresponds to a 95% confidence level
lower <- y_pred - ci
upper <- y_pred + ci
cf1 <- coef(model)
slope.1 <- round(cf1[2], 3)
axis.scale <- 0.8

plot(log(d18O_IAR_ship$IAR)~d18O_IAR_ship$d18O, xlim = rev(range(d18O_IAR_ship$d18O)),
     pch = 16, xlab = '', ylab = '', axes = F)
axis(side = 1, cex.axis = 0.9)
axis(side = 2, cex.axis = 0.9)
box()

lines(newx, lower, lty = 2)
lines(newx, upper, lty = 2)
abline(model)
mtext(text = expression(paste(delta, ''^'18', 'O', "(\u2030)")), side = 1, line = 2.5, cex = 1.0)
mtext(text = expression(paste('log(IAR)')), side = 2, line = 2.1, cex = axis.scale)
mtext(text = "Shipboard model", cex = 1.5, line = 0.5)
r2 <- round(summary(model)$r.squared, 4)
pval <- summary(model)$coefficients[, "Pr(>|t|)"]
pval <- round(pval[2], 4)

text(0.4, 10.6, bquote(paste('R'^'2',' = ', .(r2))), cex = 0.8)
text(0.4, 10.47, bquote(paste('P = ', .(pval))), cex = 0.8)
text(0.4, 10.37, bquote(paste('slope = ', .(slope.1))), cex = 0.8)



model <-lm(log(IAR)~d18O, d18O_IAR_nieder)
newx <- seq(min(d18O_IAR_nieder$d18O), max(d18O_IAR_nieder$d18O), by = 0.01)
new_data <- data.frame(d18O = newx)
pred <- predict(model, newdata = new_data, se.fit = TRUE)

y_pred <- pred$fit
se <- pred$se.fit
ci <- 1.96 * se  # Assuming a normal distribution, 1.96 corresponds to a 95% confidence level
lower <- y_pred - ci
upper <- y_pred + ci
cf1 <- coef(model)
slope.1 <- round(cf1[2], 3)
axis.scale <- 0.8

plot(log(d18O_IAR_nieder$IAR)~d18O_IAR_nieder$d18O, xlim = rev(range(d18O_IAR_nieder$d18O)),
     pch = 16, xlab = '', ylab = '', axes = F)
axis(side = 1, cex.axis = 0.9)
axis(side = 2, cex.axis = 0.9)
box()

lines(newx, lower, lty = 2)
lines(newx, upper, lty = 2)
abline(model)
mtext(text = expression(paste(delta, ''^'18', 'O', "(\u2030)")), side = 1, line = 2.5, cex = 1.0)
mtext(text = expression(paste('log(IAR)')), side = 2, line = 2.1, cex = axis.scale)
mtext(text = "Niederbockstruck et al. model", cex = 1.5, line = 0.5)
r2 <- round(summary(model)$r.squared, 4)
pval <- summary(model)$coefficients[, "Pr(>|t|)"]
pval <- round(pval[2], 4)

text(0.4, 10, bquote(paste('R'^'2',' = ', .(r2))), cex = 0.8)
text(0.4, 9.9, bquote(paste('P = ', .(pval))), cex = 0.8)
text(0.4, 9.8, bquote(paste('slope = ', .(slope.1))), cex = 0.8)


############ first differences ################


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

mtext(text = expression(paste(delta, ''^'18', 'O', "(‰)")), side = 1, line = 2.5, cex = 1.0)
mtext(text = expression(paste('IAR residuals')), 
      side = 2, line = 2.1, cex = axis.scale)
mtext(text = "IODP U1553", cex = 1.5, line = 0.5)
r2 <- round(summary(model)$r.squared, 4)
pval <- summary(model)$coefficients[, "Pr(>|t|)"]
pval <- round(pval[2], 4)

text(0.6, 11600, bquote(paste('R'^'2',' = ', .(r2))), cex = 0.8)
text(0.6, 10600, bquote(paste('P = ', .(pval))), cex = 0.8)
text(0.6, 9600, bquote(paste('slope = ', .(slope.1))), cex = 0.8)

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
mtext(text = expression(paste(delta, ''^'18', 'O', "(‰)")), side = 1, line = 2.5, cex = 1.0)
mtext(text = expression(paste('IAR residuals')), side = 2, line = 2.1, cex = axis.scale)
mtext(text = "DSDP 596", cex = 1.5, line = 0.5)
r2 <- round(summary(model)$r.squared, 3)
pval <- summary(model)$coefficients[, "Pr(>|t|)"]
pval <- round(pval[2], 22)

text(0.5, 185, bquote(paste('R'^'2',' = ', .(r2))), cex = 0.8)
text(0.5, 172, bquote(paste('P = ', .(pval))), cex = 0.8)
text(0.5, 159, bquote(paste('slope = ', .(slope.2))), cex = 0.8)
mtext("b", side = 3, line = 1, at = 1, font = 2, cex = 1.4)



# close file
if(writeFile != 'off') {
  dev.off()
}