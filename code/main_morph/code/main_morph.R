library(mgcv)

setwd("/Users/icbissell/Documents/research/Eocene_teeth/code/main_morph")

morphs <- read.csv("data/ToothMorph_V0.4_Morphotypes - Chas_U1553.csv")
teeth_total <- read.csv("data/teeth_total.csv")

morphs$com <- with(morphs, paste(Sample.ID,Object, sep = "_"))
teeth_total$com <- with(teeth_total, paste(
  sub("^(([^_]*_){5}[^_]*).*", "\\1", teeth_total$ObjectID), 
  sub("^(?:[^_]*_){7}([^_]*)_[^_]*$", "\\1", teeth_total$ObjectID),
  sep = "_"))

teeth_total$morph <- vector("character", nrow(teeth_total))

#loop through teeth_total and add morphs
for (i in 1:nrow(teeth_total)) {
  teeth_total$morph[i] <- morphs$Alias[which(morphs$com == teeth_total$com[i])]
}

rm(morphs)

morph.by.year<-table(teeth_total$Age, teeth_total$morph)
morph.by.year <- as.data.frame.matrix(morph.by.year)

################################################################
#calculate top morphotypes by summing across years
morph.count = colSums(morph.by.year)
morph.count = data.frame(morph=names(morph.count), value=morph.count, row.names=NULL)
morph.count<- morph.count[rev(order(morph.count$value)),]

#remove generic cone and triangle
rows_to_remove <- c("Generic Triangle", "Generic cone")
morph.count <- subset(morph.count, !(morph %in% rows_to_remove))

################################################################


################################################################
#calculate top morphotypes by normalizing and then summing
year.sums <- rowSums(morph.by.year)
year.sums = data.frame(year=names(year.sums), value=year.sums, row.names=NULL)

morph.percent = sweep(morph.by.year, 1, year.sums$value, FUN = '/')
morph.percent = colSums(morph.percent)
morph.percent = data.frame(morph=names(morph.percent), value=morph.percent, row.names=NULL)
morph.percent<- morph.percent[rev(order(morph.percent$value)),]

#remove generic cone and triangle
rows_to_remove <- c("Generic Triangle", "Generic cone")
morph.percent <- subset(morph.percent, !(morph %in% rows_to_remove))

################################################################

top_mc <- morph.count[1:10, ]
top_mp <- morph.percent[3:length(morph.percent$morph), ]
  
#subset total dataset to only include top morphotypes
top.lengths <- teeth_total[teeth_total$morph %in% top_mp$morph, ]

#calculate means per year
length_means <- aggregate(top.lengths$length, list(top.lengths$Age), FUN = mean)

#rename variables
names(length_means)[names(length_means) == "Group.1"] <- "age"
names(length_means)[names(length_means) == "x"] <- "mean_length"
  
#calculate linear model
model <- lm(length_means$mean_length ~ length_means$age)
summary(model)

best_gam <- function(top.lengths, length_means) {
  #come up with best gam for morph
  par(mfrow = c(3,4))
  for(i in 1:10) {
    lm_i <- gam(length~s(Age, k=i), data = top.lengths)
    plot(lm_i, main = paste("k = ", i))
    mtext(paste("aic:", round(lm_i$aic, digits = 2)), side=3)
    points(length_means$age, length_means$mean_length-mean(length_means$mean_length), pch = 16)
    summary(lm_i)
  }
  
  par(mfrow = c(1,1))
  gam_aic <- c()
  for(i in 1:11) {
    lm_i <- gam(length~s(Age, k=i), data = top.lengths)
    gam_aic[i] <- lm_i$aic
  }
  plot(gam_aic, xlab = "k", ylab = "AIC", pch = 16)
}

#calculate gam with dataset
gam <- gam(length~s(Age, k=5), data=top.lengths)
plot(gam)
points(length_means$age, length_means$mean_length-mean(length_means$mean_length), pch = 16)
mtext("Average of all morphtypes exluding the top two")


#plot top morphotypes

plot_gam <- function(teeth_total, gam_k, morph.percent, morph.cent) {
  
  t.mp <- morph.percent[morph.cent, ]
  #subset total dataset to only include top morphotypes
  t.l <- teeth_total[teeth_total$morph %in% t.mp$morph, ]
  
  #calculate means per year
  l.m <- aggregate(t.l$length, list(t.l$Age), FUN = mean)
  
  #rename variables
  names(l.m)[names(l.m) == "Group.1"] <- "age"
  names(l.m)[names(l.m) == "x"] <- "mean_length"
  
  gam.i <- gam(length~s(Age, k=gam_k), data=t.l)
  plot(gam.i)
  points(length_means$age, length_means$mean_length-mean(length_means$mean_length), pch = 16)
  mtext(paste("n = ", morph.cent), side = 3, line = 1 , cex = 0.75)
  mtext(t.mp$morph, side = 3, line = 2 , cex = 0.75)
}


par(mfrow = c(3, 4))
plot_gam(teeth_total, 6, morph.percent, 1)
plot_gam(teeth_total, 6, morph.percent, 2)
plot_gam(teeth_total, 1, morph.percent, 3)
plot_gam(teeth_total, 8, morph.percent, 4)
plot_gam(teeth_total, 4, morph.percent, 5)
plot_gam(teeth_total, 1, morph.percent, 6)
plot_gam(teeth_total, 7, morph.percent, 7)
plot_gam(teeth_total, 1, morph.percent, 8)
plot_gam(teeth_total, 9, morph.percent, 9)
plot_gam(teeth_total, 10, morph.percent, 10)
plot_gam(teeth_total, 5, morph.percent, 11)
plot_gam(teeth_total, 9, morph.percent, 12)



