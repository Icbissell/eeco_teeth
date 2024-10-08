#####################################
#                                   #
#           Range Charts            #
#                                   #
#####################################

library(viridis)
library(ichthyoliths)


# Requires age_models.R to be run to get morphometric datasets on the right age models
###### Niederbockstruck et al age model Range Chart #####

# writeFile <- 'pdf'
# writeFile <- 'jpg'
writeFile <- 'off'

fig.dims <- c(7, 12) #Set Figure-dimensions

if(writeFile == 'pdf') {
  pdf('supplement_plot/RangeChart.pdf', height = fig.dims[1], width = fig.dims[2], useDingbats = FALSE)
}

if(writeFile == 'jpg') {
  jpeg('supplement_plot/RangeChart.jpg', height = fig.dims[1], width = fig.dims[2], units = 'in', res = 300)
}

col.rangechart <- viridis(5)
col.rangechart[1] <- 'gray70'

par(mar = c(7, 5, 3, 3))

hide<-rangechart(morph.counts.nieder, reorder = 'fad.by.lad', normalize.counts = TRUE,
                 col.points = 'by.count', cols.vec = col.rangechart, count.breaks = c(0, 1, 3, 5, 7, 10), # 1, 2, 3, 4-5, 6+
                 cex.points = 'by.count', largesize = 1,
                 xaxis.labels = 'names', yaxis.ticks = TRUE,
                 print.xaxis = T, main = '', ylab = 'Age (Ma)',
                 cex.yaxis = 0.8, #a clunky but effective way to remove the y-axis ages
                 cex.xaxis = 0.35)

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
           print.xaxis = F, main = '', ylab = 'Age (Ma)', 
           cex.yaxis = 0.8, cex.xaxis = 0.35)

#axis(side = 2, at = round(as.numeric(rownames(morph.counts.nieder)), 2), las = 1, cex.axis = 0.8, tck = -0.01)

legend('bottomright', legend = c('1', '2-3', '4-5', '6-10', '11+'),
       pch = c(16),
       col = c(col.rangechart),
       pt.cex = c(1, 1.25, 1.5, 1.75, 2),
       ncol = 4, title = 'Tooth Count', title.adj = 0.5, cex = 0.8)

mtext("Niederbockstruck et al. age model", side = 3, line = 1, cex = 1.2, font = 2)

# close file
if(writeFile != 'off') {
  dev.off()
}


###### Shipboard age model Range Chart #####

writeFile <- 'pdf'
# writeFile <- 'jpg'
# writeFile <- 'off'

fig.dims <- c(7, 12) #Set Figure-dimensions

if(writeFile == 'pdf') {
  pdf('supplement_plot/RangeChart_ship.pdf', height = fig.dims[1], width = fig.dims[2], useDingbats = FALSE)
}

if(writeFile == 'jpg') {
  jpeg('supplement_plot/RangeChart_ship.jpg', height = fig.dims[1], width = fig.dims[2], units = 'in', res = 300)
}

col.rangechart <- viridis(5)
col.rangechart[1] <- 'gray70'

par(mar = c(7, 5, 3, 3))
hide<-rangechart(morph.counts.shipboard, reorder = 'fad.by.lad', normalize.counts = TRUE,
                 col.points = 'by.count', cols.vec = col.rangechart, count.breaks = c(0, 1, 3, 5, 7, 10), # 1, 2, 3, 4-5, 6+
                 cex.points = 'by.count', largesize = 1.2,
                 xaxis.labels = 'names', yaxis.ticks = T,
                 print.xaxis = T, main = '',ylab = '',  
                 cex.yaxis = 0.8, cex.xaxis = 0.35)

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
rangechart(morph.counts.shipboard, reorder = 'fad.by.lad', normalize.counts = TRUE,
           col.points = 'by.count', cols.vec = col.rangechart, count.breaks = c(0, 1, 3, 5, 7, 10), # 1, 2, 3, 4-5, 6+
           cex.points = 'by.count', largesize = 1.2,
           xaxis.labels = 'names', yaxis.ticks = TRUE,
           print.xaxis = T, main = '', ylab = 'Age (Ma)', 
           cex.yaxis = 0.8, cex.xaxis = 0.35)
#axis(side = 2, at = round(as.numeric(rownames(morph.counts.nieder)), 2), las = 1, cex.axis = 0.8, tck = -0.01)

legend('bottomright', legend = c('1', '2-3', '4-5', '6-10', '11+'),
       pch = c(16),
       col = c(col.rangechart),
       pt.cex = c(1, 1.25, 1.5, 1.75, 2),
       ncol = 4, title = 'Tooth Count', title.adj = 0.5, cex = 0.8)

mtext("Shipboard age model", side = 3, line = 1, cex = 1.2, font = 2)

# close file
if(writeFile != 'off') {
  dev.off()
}


##### Reset graphical parameters & Clean up #####
par(mar = c(5.1,4.1,4.1,2.1)) #reset default margins
par(mfrow = c(1,1)) #reset to single plotting frame

rm(hide, rec.col, usr, xleft, xright, ybottom, ytop)
