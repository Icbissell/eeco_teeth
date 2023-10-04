library(vegan)

morphs <- read.csv("data/ToothMorph_V0.4_Morphotypes - Chas_U1553.csv")


save_pdf <- FALSE

if (save_pdf == TRUE) {
  pdf(file = "plots/morph_time.pdf", width = 10, height = 8)
}

morphs <- morphs[!morphs$Alias=="",]
morph.age <- data.frame(age = morphs$Age, morphotype = morphs$Alias)

# morph.age$group <- 1
# morph.age$group[which(morph.age$age <= 60 )] <- "G1"
# morph.age$group[which(morph.age$age > 60 )] <- "G2"

morph.counts <- table(morph.age)

# m.c <- data.frame(morph.counts)
# m.c$age <- as.numeric(as.character(m.c$age))
# m.c$group <- 1
# m.c$group[which(m.c$age <= 60 )] <- "G1"
# m.c$group[which(m.c$age > 60 )] <- "G2"

test <- vegdist(morph.counts, method="bray")
#test2 <- metaMDS(test, k=2)
test3 <- metaMDS(test, k=5)

age.vec<-as.numeric(rownames(morph.counts))
age.vec <- round(age.vec, 2)

plot(test3)
pts <- test3$points
text(x=pts[,1], y=pts[,2], labels = age.vec, pos=3, offset=0.35, cex=0.8)

for(i in 1:(length(age.vec)-1)) {
  segments(pts[i,1], pts[i,2], pts[i+1,1], pts[i+1,2], lty=1, col='purple', lwd=2)
}


#figure out how to get ellipses on here (add age breaks into 'groups')
ordiellipse(test3, groups = morph.age$group)
  
  
  
  
  