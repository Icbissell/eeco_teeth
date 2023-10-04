library(stringr)
library(car)
library(ggplot2)
library(plyr)
library(zoo)

#set code directory
setwd("/Users/icbissell/Documents/research/Eocene_teeth/code/length_plot")
#set path to lengths dataset
teeth_total <- read.csv("data/teeth_total.csv")
#set path to Westerhold oxygen data
o_data <- read.csv("data/Westerhold_2020_Oxygen_Carbon_smooth.csv")

#add column that sets length as the max of width and length
teeth_total$length <- pmax(teeth_total$Height, teeth_total$Width)

#compute quantiles
length_means <- aggregate(teeth_total$length, list(teeth_total$Age), FUN = mean)
names(length_means)[names(length_means) == "Group.1"] <- "age"
names(length_means)[names(length_means) == "x"] <- "mean_length"
length_quant_95 <- aggregate(teeth_total$length, list(teeth_total$Age), FUN = quantile, probs = 0.95)
length_quant_75 <- aggregate(teeth_total$length, list(teeth_total$Age), FUN = quantile, probs = 0.75)
length_quant_25 <- aggregate(teeth_total$length, list(teeth_total$Age), FUN = quantile, probs = 0.25)
length_medians <- aggregate(teeth_total$length, list(teeth_total$Age), FUN = median)

#plot detrended anomaly from mean line
model <- lm(length_means$mean_length ~ length_means$age)
curve_df <- data.frame(time = length_means$age, resi = model$residuals)

plot(curve_df$time, curve_df$resi, pch = 19)
polygon(x = c(min(curve_df$time), curve_df$time, max(curve_df$time)),
        y = c(0, curve_df$resi, 0), col = "skyblue")
clip(x1 = min(curve_df$time), x2 = max(curve_df$time), y1 = min(curve_df$resi),y2 = 0)
polygon(x = c(min(curve_df$time), curve_df$time, max(curve_df$time)),
        y = c(0, curve_df$resi, 0),
        col = "tomato")

#plot various quantiles
plot(length_quant_95$Group.1, length_quant_95$x, col = 'green', pch = 16, ylim = c(50, 550))
points(length_quant_75$Group.1, length_quant_75$x, col = 'red', pch = 16)
points(length_quant_25$Group.1, length_quant_25$x, col = 'orange', pch = 16)
points(length_medians$Group.1, length_medians$x, col = 'black', pch = 16)

#plot full dataset with trend line
scatterplot(teeth_total$length ~ as.numeric(teeth_total$Age), log = "y")

#plot violin plot of total dataset
teeth_total$age_round = round(teeth_total$Age, digits = 1)

pViol <- ggplot(teeth_total, aes(x = as.factor(age_round), y = length, fill = factor(age_round))) +
  geom_violin() +
  geom_boxplot(width = .2) +
  scale_fill_hue(rainbow(length(unique(teeth_total$SampleID))))
pViol

#plot mean lengths vs temperature 
o_data <- read.csv("/Users/icbissell/Documents/research/Eocene_teeth/code/IAR_time/data/Westerhold_2020_Oxygen_Carbon_smooth.csv")
o_df1 <- data.frame(age = as.numeric(o_data$Age_Ma), d18O = as.numeric(o_data$d18O_loess_smooth))
o_df1 = o_df1[(o_df1$age >= 47) & (o_df1$age <= 65), ]
d180.range <- c(1, -1) #ylim = rev(range(o_df1$d18O))
plot(rollmean(o_df1, k = 50), type = 'l', ylim = d180.range, 
     xlim = c(65, 47), ylab = "d18O")
axis(side=1, at = pretty(range(o_df1$age, na.rm = TRUE), n =10))
axis(side=2, at = pretty(range(o_df1$d18O, na.rm = TRUE)))
par(new = TRUE)
plot(length_means$age, length_means$mean_length, xlim = rev(c(min(length_means$age), max(length_means$age))), type = 'p', col = 'red', pch = 16, axes = FALSE, xlab = "", ylab = "")
axis(4)
mtext("mean length", side=4, line=-1.5)

#prepare temperature and length tests
o_df1 <- data.frame(age = as.numeric(o_data$Age_Ma), d18O = as.numeric(o_data$d18O_loess_smooth))
o_df1 = o_df1[(o_df1$age >= min(length_means$age)) & (o_df1$age <= max(length_means$age)), ]

#compute rolling mean of temperature data
roll <- data.frame(rollmean(o_df1, 100))

i <- 1
match_d18O <- c()
while(i <= length(length_means$age)) {
  match_d18O[i] <- roll$d18O[which(abs(roll$age-length_means$age[i])==min(abs(roll$age-length_means$age[i])))]
  i = i+1
}

match_d18O <- data.frame(age = length_means$age, d18O = match_d18O)

#test association with mean lengths
lm_1 = lm(length_means$mean_length ~ match_d18O$d18O)
summary(lm_1)
cor.test(match_d18O$d18O, length_means$mean_length, method = "spearman")

#cut to points <62 million years
d18O_62 <- match_d18O[-which(match_d18O$age > 62), ]
length_means_62 <- length_means[-which(length_means$age > 62), ]

#test association again
lm_2 = lm(length_means_62$mean_length ~ d18O_62$d18O)
summary(lm_2)
cor.test(d18O_62$d18O, length_means_62$mean_length, method = "spearman")



