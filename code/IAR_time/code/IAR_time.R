library(ggplot2)
library(zoo)

#set code directory
setwd("/Users/icbissell/Documents/research/Eocene_teeth/code/IAR_time")
#set path to processing log
ich_data <- read.csv("data/U1553_Processing_Log - Sample_Processing.csv")
#set path to Westerhold oxygen data
o_data <- read.csv("data/Westerhold_2020_Oxygen_Carbon_smooth.csv")

colnames(ich_data) <- ich_data[1,]
ich_data = ich_data[-1, ]

ich_df1 <- data.frame(age = as.numeric(ich_data$`Age (Linear Interpolation)`), IAR = as.numeric(ich_data$`ich/cm2/myr`))
ich_df1 = ich_df1[ich_df1$age > 46.98220159, ]
ich_df1 = ich_df1[ich_df1$IAR > 0, ]
ich_df1 = ich_df1[complete.cases(ich_df1), ]

o_df1 <- data.frame(age = as.numeric(o_data$Age_Ma), d18O = as.numeric(o_data$d18O_loess_smooth))
o_df1 = o_df1[(o_df1$age >= min(ich_df1$age)) & (o_df1$age <= max(ich_df1$age)), ]

age.range <- c(65, 47)
d180.range <- c(1, -1)

par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
plot(rollmean(o_df1, k = 20), type = 'l', ylim = d180.range, 
     xlim = age.range, ylab = "d18O")
axis(side=1, at = pretty(range(o_df1$age, na.rm = TRUE), n =10))
axis(side=2, at = pretty(range(o_df1$d18O, na.rm = TRUE)))
par(new = TRUE)
plot(ich_df1$age, log(ich_df1$IAR), type = "o", pch = 16, col = "red", 
     axes = FALSE, ylab = "", xlab = "", xlim = age.range)
axis(side=4, at = pretty(range(ich_df1$IAR, na.rm = TRUE)))
mtext("IAR", side=4, line=3)






