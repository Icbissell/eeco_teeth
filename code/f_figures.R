library(ichthyoliths)
library(viridis)
library(zoo)
library(egg)
library(mgcv)
library(ggplot2)

<<<<<<< HEAD
=======
setwd("../f_figures")

>>>>>>> d4d4d9f6dd43ad3d861c552bcc618e7fbdc4e031
#dev.off()


######## Morphotypes over time rangechart ############
morphs <- read.csv("data/ToothMorph_V0.4_Morphotypes - Chas_U1553.csv")


save_pdf <- FALSE

if (save_pdf == TRUE) {
  pdf(file = "plots/morph_time.pdf", width = 10, height = 8)
}

morphs <- morphs[!morphs$Alias=="",]
morph.age <- data.frame(age = morphs$Age, morphotype = morphs$Alias)

morph.counts <- table(morph.age)

col.rangechart <- viridis(5)
col.rangechart[1] <- 'gray70'

par(mar = c(12, 5, 3, 3))

# xax<-rangechart(counts, reorder = 'lad.by.fad', normalize.counts = FALSE,
#                 col.points = 'by.count', cols.vec = col.rangechart, count.breaks = c(0, 1, 3, 5), # 1, 2, 3, 4-5, 6+
#                 cex.points = 'by.count', largesize = 1,
#                 xaxis.labels = 'alphanum', yaxis.ticks = TRUE,
#                 print.xaxis = T, main = '', ylab = 'Age (Ma)')

hide<-rangechart(morph.counts, reorder = 'fad.by.lad', normalize.counts = TRUE,
                 col.points = 'by.count', cols.vec = col.rangechart, count.breaks = c(0, 1, 3, 5, 7, 10), # 1, 2, 3, 4-5, 6+
                 cex.points = 'by.count', largesize = 1,
                 xaxis.labels = 'names', yaxis.ticks = TRUE,
                 print.xaxis = T, main = '', ylab = 'Age (Ma)', cex.yaxis = 0.8, cex.xaxis = 0.75)

#get rect coordinates
usr <- par("usr")
xleft <- usr[1]
xright <- usr[2]
ybottom <- 53
ytop <- 49
rec.col <- 'gray90'

#plot rectangle
rect(xleft, ybottom, xright, ytop, col = rec.col, border = rec.col)

#plot again on top of rectangle
par(new = T)
rangechart(morph.counts, reorder = 'fad.by.lad', normalize.counts = TRUE,
           col.points = 'by.count', cols.vec = col.rangechart, count.breaks = c(0, 1, 3, 5, 7, 10), # 1, 2, 3, 4-5, 6+
           cex.points = 'by.count', largesize = 1,
           xaxis.labels = 'names', yaxis.ticks = TRUE,
           print.xaxis = T, main = '', ylab = 'Age (Ma)', cex.yaxis = 0.8, cex.xaxis = 0.75)

axis(side = 2, at = round(as.numeric(rownames(morph.counts)), 2), las = 1, cex.axis = 0.8, tck = -0.01)

legend('bottomright', legend = c('1', '2-3', '4-5', '6-10', '11+'),
       pch = c(16),
       col = c(col.rangechart),
       pt.cex = c(1, 1.25, 1.5, 1.75, 2),
       ncol = 4, title = 'Tooth Count', title.adj = 0.5, cex = 0.5)


################################################


######## Vars over time ############


#add datasets

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

#set plotting vars
yaxis.age <- c(63, 41)
axis.scale <- 0.77
text.scale <- 1.2
pt.scale <- 1.4

#plot temperature (but hide it)
par(mfrow = c(1, 3),
    oma = c(8, 4, 4.5, 0.5),  #set outer margins for axis labels
    mar = c(0, 0, 0, 0))  # set plot martins to be very squished together

hide <- plot(x = o_roll$d18O, y = o_roll$age, type = 'n', pch = 16,
             col = 'darkcyan', ylim = yaxis.age, cex = pt.scale,
             xlim = c(max(o_roll$d18O), min(o_roll$d18O)),
             bty = 'n', axes = FALSE, xlab = '', ylab = '')

#get rect coordinates
usr <- par("usr")
xleft <- usr[1]
xright <- usr[2]
ybottom <- 53
ytop <- 49
rec.col <- 'gray90'

#plot rectangle
par(mfg = c(1, 1))
rect(xleft, ybottom, xright, ytop, col = rec.col, border = rec.col)

par(mfg = c(1, 2))
rect(xleft, ybottom, xright, ytop, col = rec.col, border = rec.col)

par(mfg = c(1, 3))
rect(xleft, ybottom, xright, ytop, col = rec.col, border = rec.col)

par(new = T, mfrow = c(1, 3),
    oma = c(8, 4, 4.5, 0.5),  #set outer margins for axis labels
    mar = c(0, 0, 0, 0))  # set plot martins to be very squished together

iar.axis.text <- expression(paste('IODP 1553 IAR (ich 38-150 ', mu, 'm ', 'cm'^'-2','Myr'^'-1',')'))
eo.col <- adjustcolor('lightskyblue', alpha.f = 0.3)

#plot temperature
par(mfg = c(1, 1))
plot(x = o_roll$d18O, y = o_roll$age, type = 'o', pch = 16, col = 'darkcyan', ylim = yaxis.age, cex = pt.scale,
     xlim = c(max(o_roll$d18O), min(o_roll$d18O)),
     bty = 'n', axes = FALSE, xlab = '', ylab = '')

axis(1) # D18O values
mtext (text = expression(paste(delta, ''^'18', 'O')), side = 1, line = 2.5, cex = axis.scale)

axis(2, at=seq(42,63, by=2)) #age axis
mtext(text = 'Age (Ma)', side = 2, line = 2.5, cex = axis.scale)

#plot IAR
plot(x = iar$IAR, y = iar$age, type = 'o', pch = 16, col = 'chocolate', ylim = yaxis.age, cex = pt.scale,
     xlim = c(0, max(iar$IAR)),
     bty = 'n', axes = FALSE, xlab = '', ylab = '')
axis(1) # IAR values
mtext (text = iar.axis.text, side = 1, line = 2.5, cex = axis.scale)
par(new = TRUE)
plot(x = IAR.596$ich_accum, y = IAR.596$age, type = 'o', pch = 18, col=adjustcolor('darkorchid', alpha=0.45), ylim = yaxis.age, cex = pt.scale,
     xlim = c(0, max(IAR.596$ich_accum)),
     bty = 'n', axes = FALSE, xlab = '', ylab = '')
axis(3)
mtext (text = expression(paste('DSDP 596 IAR (ich >106 ', mu, 'm ','cm'^'-2','Myr'^'-1', ')')), side = 3, line = 2.5, cex = axis.scale)
legend("topright", legend = c("IODP 1553", "DSDP 596"), col = c("chocolate", 'darkorchid'), pch = 16, bty = "n", inset = c(0.05, 0.02))


#plot mean length
gam_5 <- gam(length~s(Age, k=5), data=teeth_total)

x_vals <- seq(min(teeth_total$Age), max(teeth_total$Age), length = 100)
new_data <- data.frame(Age = x_vals)
pred <- predict(gam_5, newdata = new_data, se.fit = TRUE)
y_pred <- pred$fit
se <- pred$se.fit

ci <- 1.96 * se  # Assuming a normal distribution, 1.96 corresponds to a 95% confidence level
lower <- y_pred - ci
upper <- y_pred + ci

# plot(x_vals, y_pred, xlab = "x", ylab = "Response", type = "l")
# lines(x_vals, lower, col = "blue", lty = "dashed")
# lines(x_vals, upper, col = "blue", lty = "dashed")

plot(y_pred, x_vals, type = "l", bty = 'n', axes = FALSE, xlab = '',
     ylab = '', ylim = yaxis.age, xlim = c(min(mean_length$length), max(mean_length$length)))
points(mean_length$length, mean_length$age, pch = 16)
polygon(c(lower, rev(upper)), c(x_vals, rev(x_vals)), col=adjustcolor('dodgerblue', alpha=0.25), border = NA)
axis(1) # length values
mtext (text = "Mean length (unit?)", side = 1, line = 2.5, cex = axis.scale)


######## Cross plot ############
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
#o_df1 = o_df1[(o_df1$age >= min(mean_length$age)) & (o_df1$age <= max(mean_length$age)), ]

#compute rolling mean of temperature data
o_roll <- data.frame(rollmean(o_df1, 100))

match_d18O <- c()

for(i in 1:length(iar$age)) {
  ind <- which(abs(o_roll$age-iar$age[i])==min(abs(o_roll$age-iar$age[i])))
  print(ind)
  if (ind == 1) {
    match_d18O[i] <- o_roll$d18O[ind]
  }

  else{
    match_d18O[i] <- mean(o_roll$d18O[(ind-2):(ind+2)])
  }

}

d18O_IAR <- data.frame(age = iar$age, d18O = match_d18O, IAR = iar$IAR)

match_d18O_596 <- c()
for(i in 1:length(IAR.596$age)) {
  match_d18O_596[i] <- o_roll$d18O[which(abs(o_roll$age-IAR.596$age[i])==min(abs(o_roll$age-IAR.596$age[i])))]
}

d18O_IAR_596 <- data.frame(age = IAR.596$age, d18O = match_d18O_596, IAR = IAR.596$ich_accum)

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
slope.1 <- round(cf1[2], 3)
axis.scale <- 0.8

plot(log(d18O_IAR$IAR)~d18O_IAR$d18O, xlim = rev(range(d18O_IAR$d18O)),
     pch = 16, xlab = '', ylab = '')
lines(newx, lower, lty = 2)
lines(newx, upper, lty = 2)
abline(model)
mtext(text = expression(paste(delta, ''^'18', 'O')), side = 1, line = 2.5, cex = 1.0)
mtext(text = expression(paste('log(IAR) [ich cm'^'-2','Myr'^'-1', ']')), side = 2, line = 2.5, cex = axis.scale)
mtext(text = "IODP 1553", cex = 1)
r2 <- round(summary(model)$r.squared, 4)
pval <- summary(model)$coefficients[, "Pr(>|t|)"]
pval <- round(pval[2], 4)
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
slope.2 <- round(cf2[2], 4)

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
pval <- round(pval[2], 22)
text(0.4, 5.5, bquote(paste('R'^'2',' = ', .(r2))), cex = 0.8)
text(0.4, 5.3, bquote(paste('P = ', .(pval))), cex = 0.8)
text(0.4, 5.1, bquote(paste('slope = ', .(slope.2))), cex = 0.8)

###################################

