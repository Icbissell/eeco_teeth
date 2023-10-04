library(zoo)
library(ggplot2)
library(egg)

#set code directory
setwd("/Users/icbissell/Documents/research/Eocene_teeth/code/layer_time")
#set path to Westerhold oxygen data
o_data <- read.csv("data/Westerhold_2020_Oxygen_Carbon_smooth.csv")
#set path to IAR data
iar <- read.csv("data/iar.csv")
#set path to mean length data
mean_length <- read.csv("data/length_means.csv")

#prepare temperature and length tests
o_df1 <- data.frame(age = as.numeric(o_data$Age_Ma), d18O = as.numeric(o_data$d18O_loess_smooth))
o_df1 = o_df1[(o_df1$age >= min(mean_length$age)) & (o_df1$age <= max(mean_length$age)), ]

#compute rolling mean of temperature data
o_roll <- data.frame(rollmean(o_df1, 100))

d180.range <- c(max(o_roll$d18O), min(o_roll$d18O))

par(mfrow = c(3,1))
plot(o_roll$age, o_roll$d18O, ylim = d180.range)
plot(iar$age, iar$IAR, 'o')
plot(mean_length$age, mean_length$mean_length, 'o')

plot1 <- ggplot(data=o_roll) + 
  geom_point(data=o_roll, aes(x = age, y = d18O), color = "#E69F00") +
  ylim(d180.range) + 
  theme(axis.text.x=element_blank(), 
        axis.title.x=element_blank()) #remove x axis labels

plot2 <- ggplot(data=iar) + 
  geom_point(data=iar, aes(x = age, y = IAR)) + 
  geom_line(data=iar, aes(x = age, y = IAR), color = "#56B4E9") + 
  theme(axis.text.x=element_blank(), 
        axis.title.x=element_blank()) #remove x axis labels

plot3 <- ggplot(data=mean_length) + 
  geom_point(data=mean_length, aes(x = age, y = mean_length)) +
  geom_line(data=mean_length, aes(x = age, y = mean_length), color = "#009E73")

ggarrange(plot1, plot2, plot3, nrow = 3, ncol = 1)



