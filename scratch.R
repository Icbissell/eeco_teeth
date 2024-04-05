##### Step 1: Call in Ich Accumulation and Morphotype occurrence datasets and
#             put them on both age models #####

source('code/setup_age_models.R')

##### Step 2: Process sample-specific tooth length datasets #####
source('code/compute_length_updated.R')

##### Step 3: Calculate correlation Statistics on IAR vs. d18O, etc.
# Can run f_scatter.R from source, it will create the additional cross-plot figures
source('code/f_scatter.R')

##### Step 4: Capture-mark-recapture Evolutionary Rate Calculations #####
# If you have MARK installed, you can run this file; Otherwise, load the .RData file to add the output objects.
# source('code/cmr_calculations.R')
load('data/Pradrec.teeth.nieder.RData')
load('data/Pradrec.teeth.shipboard.RData')


library(viridis)
library(ichthyoliths)


# Requires age_models.R to be run to get morphometric datasets on the right age models

##### Shipboard Age Model Range Chart #####

## Parameters

col.rangechart <- viridis(5)
col.rangechart[1] <- 'gray70'

par(mar = c(10, 5, 1, 2))

# xax<-rangechart(counts, reorder = 'lad.by.fad', normalize.counts = FALSE,
#                 col.points = 'by.count', cols.vec = col.rangechart, count.breaks = c(0, 1, 3, 5), # 1, 2, 3, 4-5, 6+
#                 cex.points = 'by.count', largesize = 1,
#                 xaxis.labels = 'alphanum', yaxis.ticks = TRUE,
#                 print.xaxis = T, main = '', ylab = 'Age (Ma)')

hide<-rangechart(morph.counts.shipboard, reorder = 'fad.by.lad', normalize.counts = TRUE,
                 col.points = 'by.count', cols.vec = col.rangechart, count.breaks = c(0, 1, 3, 5, 7, 10), # 1, 2, 3, 4-5, 6+
                 cex.points = 'by.count', largesize = 1,
                 xaxis.labels = 'names', yaxis.ticks = T,
                 print.xaxis = T, main = '',ylab = '',  
                 cex.yaxis = 0.8, cex.xaxis = 0.6)

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
           print.xaxis = T, main = '', ylab = '', 
           cex.yaxis = 0.8, cex.xaxis = 0.6)

mtext("Age (Ma)", side = 2, line = 2)

legend('bottomright', legend = c('1', '2-3', '4-5', '6-10', '11+'),
       pch = c(16),
       col = c(col.rangechart),
       pt.cex = c(1, 1.25, 1.5, 1.75, 2),
       ncol = 4, title = 'Tooth Count', title.adj = 0.5, cex = 0.65)

# mtext("Shipboard age model", side = 3, line = 1, cex = 1.2, font = 2)






