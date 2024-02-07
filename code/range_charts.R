#####################################
#                                   #
#           Range Charts            #
#                                   #
#####################################

# Requires age_models.R to be run to get morphometric datasets on the right age models

##### Shipboard Age Model Range Chart #####

## Parameters

col.rangechart <- viridis(5)
col.rangechart[1] <- 'gray70'

par(mar = c(12, 5, 3, 3))

# xax<-rangechart(counts, reorder = 'lad.by.fad', normalize.counts = FALSE,
#                 col.points = 'by.count', cols.vec = col.rangechart, count.breaks = c(0, 1, 3, 5), # 1, 2, 3, 4-5, 6+
#                 cex.points = 'by.count', largesize = 1,
#                 xaxis.labels = 'alphanum', yaxis.ticks = TRUE,
#                 print.xaxis = T, main = '', ylab = 'Age (Ma)')

hide<-rangechart(morph.counts.shipboard, reorder = 'fad.by.lad', normalize.counts = TRUE,
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
rangechart(morph.counts.shipboard, reorder = 'fad.by.lad', normalize.counts = TRUE,
           col.points = 'by.count', cols.vec = col.rangechart, count.breaks = c(0, 1, 3, 5, 7, 10), # 1, 2, 3, 4-5, 6+
           cex.points = 'by.count', largesize = 1,
           xaxis.labels = 'names', yaxis.ticks = TRUE,
           print.xaxis = T, main = '', ylab = 'Age (Ma)', cex.yaxis = 0.8, cex.xaxis = 0.75)

axis(side = 2, at = round(as.numeric(rownames(morph.counts.shipboard)), 2), las = 1, cex.axis = 0.8, tck = -0.01)

legend('bottomright', legend = c('1', '2-3', '4-5', '6-10', '11+'),
       pch = c(16),
       col = c(col.rangechart),
       pt.cex = c(1, 1.25, 1.5, 1.75, 2),
       ncol = 4, title = 'Tooth Count', title.adj = 0.5, cex = 0.5)

mtext("Shipboard age model", side = 3, line = 1, cex = 1.2, font = 2)




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
