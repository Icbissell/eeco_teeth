library(ichthyoliths)
library(viridis)

setwd("/Users/icbissell/Documents/research/Eocene_teeth/code/morph_time")
data <- read.csv("data/ToothMorph_V0.4_Morphotypes - Chas_U1553_2.csv")


save_pdf <- FALSE

if (save_pdf == TRUE) {
  pdf(file = "figures/morph_time.pdf", width = 10, height = 8)
}

df1 <- data[!data$Alias=="",]
#df2 <- cbind(df1$Age, df1$Alias)
df3 <- data.frame(age = df1$Age, morphotype = df1$Alias)

counts <- table(df3)

col.rangechart <- viridis(5)
col.rangechart[1] <- 'gray70'

par(mar = c(12, 5, 3, 3))

# xax<-rangechart(counts, reorder = 'lad.by.fad', normalize.counts = FALSE, 
#                 col.points = 'by.count', cols.vec = col.rangechart, count.breaks = c(0, 1, 3, 5), # 1, 2, 3, 4-5, 6+
#                 cex.points = 'by.count', largesize = 1,
#                 xaxis.labels = 'alphanum', yaxis.ticks = TRUE,
#                 print.xaxis = T, main = '', ylab = 'Age (Ma)')

xax<-rangechart(counts, reorder = 'fad.by.lad', normalize.counts = TRUE, 
                col.points = 'by.count', cols.vec = col.rangechart, count.breaks = c(0, 1, 3, 5, 7, 10), # 1, 2, 3, 4-5, 6+
                cex.points = 'by.count', largesize = 1,
                xaxis.labels = 'names', yaxis.ticks = TRUE,
                print.xaxis = T, main = '', ylab = 'Age (Ma)', cex.yaxis = 0.8, cex.xaxis = 0.75)

axis(side = 2, at = round(as.numeric(rownames(counts)), 2), las = 1, cex.axis = 0.8, tck = -0.01)

legend('bottomright', legend = c('1', '2-3', '4-5', '6-10', '11+'),
       pch = c(16),
       col = c(col.rangechart),
       pt.cex = c(1, 1.25, 1.5, 1.75, 2),
       ncol = 4, title = 'Tooth Count', title.adj = 0.5, cex = 0.5)

if (save_pdf == TRUE) {
  dev.off()
}

