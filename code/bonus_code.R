ggplot(data = length_means, aes(x=as.numeric(Group.1), y=x)) + 
  geom_point() + geom_smooth() + labs(x = "time", y = "length_mean")

ggplot(data = length_quant_75, aes(x=as.numeric(Group.1), y=x)) + 
  geom_point() + geom_smooth() + labs(x = "time", y = "length_75")

ggplot(data = length_quant_95, aes(x=as.numeric(Group.1), y=x)) + 
  geom_point() + geom_smooth() + labs(x = "time", y = "length_95")

fit1 <- lm(x~Group.1, data=length_means)
fit2 <- lm(x~poly(Group.1,2,raw=TRUE), data=length_means)
fit3 <- lm(x~poly(Group.1,3,raw=TRUE), data=length_means)
fit4 <- lm(x~poly(Group.1,4,raw=TRUE), data=length_means)
fit5 <- lm(x~poly(Group.1,5,raw=TRUE), data=length_means)

plot(length_means$Group.1, length_means$x, pch=19)
x_axis <- data.frame(Group.1 = length_means$Group.1)

lines(x_axis$Group.1, predict(fit1, x_axis), col='green')
lines(x_axis$Group.1, predict(fit2, x_axis), col='red')
lines(x_axis$Group.1, predict(fit3, x_axis), col='purple')
lines(x_axis$Group.1, predict(fit4, x_axis), col='blue')
lines(x_axis$Group.1, predict(fit5, x_axis), col='orange')

summary(fit1)$adj.r.squared
summary(fit2)$adj.r.squared
summary(fit3)$adj.r.squared
summary(fit4)$adj.r.squared
summary(fit5)$adj.r.squared

AIC(fit1)
AIC(fit2)
AIC(fit3)
AIC(fit4)
AIC(fit5)

fit1 <- lm(log(length)~Age, data=teeth_total)
fit2 <- lm(log(length)~poly(Age,2,raw=TRUE), data=teeth_total)
fit3 <- lm(log(length)~poly(Age,3,raw=TRUE), data=teeth_total)
fit4 <- lm(log(length)~poly(Age,4,raw=TRUE), data=teeth_total)
fit5 <- lm(log(length)~poly(Age,5,raw=TRUE), data=teeth_total)
fit6 <- lm(log(length)~poly(Age,6,raw=TRUE), data=teeth_total)
fit7 <- lm(log(length)~poly(Age,7,raw=TRUE), data=teeth_total)
fit8 <- lm(log(length)~poly(Age,8,raw=TRUE), data=teeth_total)
fit9 <- lm(log(length)~poly(Age,9,raw=TRUE), data=teeth_total)
fit10 <- lm(log(length)~poly(Age,10,raw=TRUE), data=teeth_total)
fit11 <- lm(log(length)~poly(Age,11,raw=TRUE), data=teeth_total)

x_axis <- data.frame(Age = teeth_total$Age)
plot(teeth_total$Age, log(teeth_total$length))

lines(x_axis$Age, predict(fit1, x_axis), col='green')
lines(x_axis$Age, predict(fit2, x_axis), col='red')
lines(x_axis$Age, predict(fit3, x_axis), col='purple')
lines(x_axis$Age, predict(fit4, x_axis), col='blue')
lines(x_axis$Age, predict(fit5, x_axis), col='orange')
lines(x_axis$Age, predict(fit6, x_axis))
lines(x_axis$Age, predict(fit7, x_axis))
lines(x_axis$Age, predict(fit8, x_axis))
lines(x_axis$Age, predict(fit9, x_axis))
lines(x_axis$Age, predict(fit10, x_axis))
lines(x_axis$Age, predict(fit11, x_axis))

summary(fit1)$adj.r.squared
summary(fit2)$adj.r.squared
summary(fit3)$adj.r.squared
summary(fit4)$adj.r.squared
summary(fit5)$adj.r.squared
summary(fit6)$adj.r.squared
summary(fit7)$adj.r.squared
summary(fit8)$adj.r.squared
summary(fit9)$adj.r.squared
summary(fit10)$adj.r.squared
summary(fit11)$adj.r.squared

AIC(fit1)
AIC(fit2)
AIC(fit3)
AIC(fit4)
AIC(fit5)
AIC(fit6)
AIC(fit7)
AIC(fit8)
AIC(fit9)
AIC(fit10)
AIC(fit11)

plot(x_axis$Age, predict(fit6, x_axis), title("full dataset"))

########################################

fit1 <- lm(x~Group.1, data=length_medians)
fit2 <- lm(x~poly(Group.1,2,raw=TRUE), data=length_medians)
fit3 <- lm(x~poly(Group.1,3,raw=TRUE), data=length_medians)
fit4 <- lm(x~poly(Group.1,4,raw=TRUE), data=length_medians)
fit5 <- lm(x~poly(Group.1,5,raw=TRUE), data=length_medians)

plot(length_medians$Group.1, length_medians$x, pch=19)
x_axis <- data.frame(Group.1 = length_medians$Group.1)

lines(x_axis$Group.1, predict(fit1, x_axis), col='green')
lines(x_axis$Group.1, predict(fit2, x_axis), col='red')
lines(x_axis$Group.1, predict(fit3, x_axis), col='purple')
lines(x_axis$Group.1, predict(fit4, x_axis), col='blue')
lines(x_axis$Group.1, predict(fit5, x_axis), col='orange')

summary(fit1)$adj.r.squared
summary(fit2)$adj.r.squared
summary(fit3)$adj.r.squared
summary(fit4)$adj.r.squared
summary(fit5)$adj.r.squared

AIC(fit1)
AIC(fit2)
AIC(fit3)
AIC(fit4)
AIC(fit5)