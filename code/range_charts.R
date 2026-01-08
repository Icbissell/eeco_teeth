#####################################
#                                   #
#           Range Charts            #
#                                   #
#####################################

library(viridis)
library(ichthyoliths)


# Requires age_models.R to be run to get morphometric datasets on the right age models

##### Shipboard Age Model Range Chart #####


writeFile <- 'pdf'
# writeFile <- 'jpg'
# writeFile <- 'off'
writeFile <- 'png'


fig.dims <- c(7, 11) #Set Figure-dimensions

if(writeFile == 'pdf') {
  pdf('plots/RangeChart.pdf', height = fig.dims[1], width = fig.dims[2], useDingbats = FALSE)
}

if(writeFile == 'jpg') {
  jpeg('plots/RangeChart.jpg', height = fig.dims[1], width = fig.dims[2], units = 'in', res = 300)
}

if(writeFile == 'png') {
  jpeg('plots/RangeChart.png', height = fig.dims[1], width = fig.dims[2], units = 'in', res = 300)
}

## Parameters
col.rangechart <- viridis(5)
col.rangechart[1] <- 'gray70'

par(mar = c(6, 5, 1, 2))

# xax<-rangechart(counts, reorder = 'lad.by.fad', normalize.counts = FALSE,
#                 col.points = 'by.count', cols.vec = col.rangechart, count.breaks = c(0, 1, 3, 5), # 1, 2, 3, 4-5, 6+
#                 cex.points = 'by.count', largesize = 1,
#                 xaxis.labels = 'alphanum', yaxis.ticks = TRUE,
#                 print.xaxis = T, main = '', ylab = 'Age (Ma)')

xax.size <- 0.55
hide<-rangechart(morph.counts.nieder, reorder = 'fad.by.lad', normalize.counts = TRUE,
                 col.points = 'by.count', cols.vec = col.rangechart, count.breaks = c(0, 1, 3, 5, 7, 10), # 1, 2, 3, 4-5, 6+
                 cex.points = 'by.count', largesize = 1.2,
                 xaxis.labels = 'alphanum', yaxis.ticks = T,
                 print.xaxis = T, main = '',ylab = '',  
                 cex.yaxis = 0.8, cex.xaxis = xax.size)

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
# rangechart(morph.counts.nieder, reorder = 'fad.by.lad', normalize.counts = TRUE,
#            col.points = 'by.count', cols.vec = col.rangechart, count.breaks = c(0, 1, 3, 5, 7, 10), # 1, 2, 3, 4-5, 6+
#            cex.points = 'by.count', largesize = 1.2,
#            xaxis.labels = 'alphanum', yaxis.ticks = TRUE,
#            print.xaxis = T, main = '', ylab = '', 
#            cex.yaxis = 0.8, cex.xaxis = xax.size)

morphnames <- rangechart(morph.counts.nieder, reorder = 'fad.by.lad', normalize.counts = TRUE,
           col.points = 'by.count', cols.vec = col.rangechart, count.breaks = c(0, 1, 3, 5, 7, 10), # 1, 2, 3, 4-5, 6+
           cex.points = 'by.count', largesize = 1.2,
           xaxis.labels = 'alphanum', yaxis.ticks = TRUE,
           print.xaxis = T, main = '', ylab = '', 
           cex.yaxis = 0.8, cex.xaxis = xax.size)
write.csv(morphnames, 'data/morphnames.csv')



mtext("Age (Ma)", side = 2, line = 2)
mtext("Morpotype index", side = 1, line = 2)


legend('bottomright', legend = c('1', '2-3', '4-5', '6-10', '11+'),
       pch = c(16),
       col = c(col.rangechart),
       pt.cex = c(1, 1.25, 1.5, 1.75, 2),
       ncol = 4, title = 'Tooth Count', title.adj = 0.5, cex = 0.65)

# mtext("Shipboard age model", side = 3, line = 1, cex = 1.2, font = 2)

# close file
if(writeFile != 'off') {
  dev.off()
}

###### Niederbockstruck et al age model Range Chart #####

col.rangechart <- viridis(5)
col.rangechart[1] <- 'gray70'

par(mar = c(12, 5, 3, 3))

hide<-rangechart(morph.counts.nieder, reorder = 'fad.by.lad', normalize.counts = TRUE,
                 col.points = 'by.count', cols.vec = col.rangechart, count.breaks = c(0, 1, 3, 5, 7, 10), # 1, 2, 3, 4-5, 6+
                 cex.points = 'by.count', largesize = 1,
                 xaxis.labels = 'names', yaxis.ticks = TRUE,
                 print.xaxis = T, main = '', ylab = 'Age (Ma)',
                 cex.yaxis = 0.8, #a clunky but effective way to remove the y-axis ages
                 cex.xaxis = 0.75)

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
rangechart(morph.counts.nieder, reorder = 'fad.by.lad', normalize.counts = TRUE,
           col.points = 'by.count', cols.vec = col.rangechart, count.breaks = c(0, 1, 3, 5, 7, 10), # 1, 2, 3, 4-5, 6+
           cex.points = 'by.count', largesize = 1,
           xaxis.labels = 'names', yaxis.ticks = TRUE,
           print.xaxis = F, main = '', ylab = 'Age (Ma)', cex.yaxis = 0.8, cex.xaxis = 0.75)

#axis(side = 2, at = round(as.numeric(rownames(morph.counts.nieder)), 2), las = 1, cex.axis = 0.8, tck = -0.01)

legend('bottomright', legend = c('1', '2-3', '4-5', '6-10', '11+'),
       pch = c(16),
       col = c(col.rangechart),
       pt.cex = c(1, 1.25, 1.5, 1.75, 2),
       ncol = 4, title = 'Tooth Count', title.adj = 0.5, cex = 0.5)

mtext("Niederbockstruck et al age model", side = 3, line = 1, cex = 1.2, font = 2)



##### Reset graphical parameters & Clean up #####
par(mar = c(5.1,4.1,4.1,2.1)) #reset default margins
par(mfrow = c(1,1)) #reset to single plotting frame

rm(hide, rec.col, usr, xleft, xright, ybottom, ytop)
