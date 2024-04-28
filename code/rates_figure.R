##############################################
#                                            #
#        Capture-Mark-Recapture FIgures      #
#                                            #
##############################################

######## Libraries and Functions ######
library(RMark)

#### par.estimates() - calls and extracts parameter estimates from a model-list ####
par.estimates<-function(model.list, rank) {
  #model.list is the marklist to pull the model from
  #rank is model rank within model table
  #pars is vector of parameters, eg c('Phi', 'p', 'f')
  
  model<-model.list[[as.numeric(row.names(model.list$model.table)[rank])]]
  pars<-names(model$parameters)
  results<-model$results$real
  par.names<-rownames(results)
  par.names<-word(par.names,1,1) #extract first word of rownames
  
  for ( i in 1:length(pars)) {
    p <- pars[i]                                 #parameter [i] name
    par.index<-which(p == par.names)             #indices of parameter [i]
    par.output<-results[par.index,]              #call the output for parameter [i]
    assign(paste(p, '.df', sep=''), par.output)  #create object of par[i].df
  }
  
  pars.list<-c(paste(pars, '.df', sep=''))     # names for parameter dataframes
  pars.sep<-mget(pars.list)                    # make list from the objects in pars.list
  names(pars.sep)<-pars
  return(pars.sep)
}


##### Step 0: Define Plotting Parameters - Pull metadata (ages, names, sample size) from counts table - note that these are identical to cmr_calculations section 1.1 #####

### CMR runs on the "counts" objects (occurrence tables for range charts)
# morph.counts.nieder
# morph.counts.shipboard
## The default order is youngest to oldest; This needs to be reversed (oldest --> youngest) for CMR

morph.counts.nieder.rev <- morph.counts.nieder[length(morph.counts.nieder[,1]):1,] #Reversed ages
morph.counts.shipboard.rev <- morph.counts.shipboard[length(morph.counts.shipboard[,1]):1,] #Reversed ages

# Niederbockstruck et al age model
ages.nieder <- as.numeric(rownames(morph.counts.nieder.rev))
ages.nieder.dt <- abs(diff(ages.nieder)) #diff finds difference between values. This is negative by default
ages.nieder.midpt <- rollmean(ages.nieder,2) #midpoint between ages - for plotting estimates

# Shipboard age model
ages.shipboard <- as.numeric(rownames(morph.counts.shipboard.rev))
ages.shipboard.dt <- abs(diff(ages.shipboard)) #diff finds difference between values. This is negative by default
ages.shipboard.midpt <- rollmean(ages.shipboard,2) #midpoint between ages - for plotting estimates

# Variables that are the same for both age models (names, sample size)
names <- colnames(morph.counts.nieder.rev)
samplesize<-as.vector(apply(morph.counts.nieder.rev, 1, sum)) #number of teeth considered in each time bin


########## NEEDS TO BE UPDATED ONCE MODELS ARE DONE/RUN ################

#### 2.4 Plot the Output ####
#### 2.4.1 - Best fit model plots ####

## Pick the right model (and ages for plotting!)
Pradrec.teeth <- Pradrec.teeth.nieder
ages.midpt <- ages.nieder.midpt

# Pradrec.teeth <- Pradrec.teeth.shipboard
# ages.midpt <- ages.shipboard.midpt


# Extract parameters of best-fit model
Pradrec.pars <- par.estimates(Pradrec.teeth, 1) #pull the best rank model parameters
teeth.ext <- Pradrec.pars$Phi
teeth.orig <- Pradrec.pars$f


#### 2.4.2 Model Averages Plot ####

## Pick the relevant model and age points for plotting
Pradrec.teeth <- Pradrec.teeth.nieder
ages.midpt <- ages.nieder.midpt

# Pradrec.teeth <- Pradrec.teeth.shipboard
# ages.midpt <- ages.shipboard.midpt

# Figure out which parameters match which indices
Par.indices <- extract.indices(Pradrec.teeth[[1]])
Pradrec.teeth.avg <- model.average(Pradrec.teeth, vcv=TRUE) #gives confidence intervals as well
Pradrec.teeth.avg[[1]]$Par.name <- Par.indices$par #attach parameter info to this value

ext.est <- subset(Pradrec.teeth.avg[[1]], Pradrec.teeth.avg[[1]]$Par.name == "Phi")
orig.est <- subset(Pradrec.teeth.avg[[1]], Pradrec.teeth.avg[[1]]$Par.name == "f")
prob.est <- subset(Pradrec.teeth.avg[[1]], Pradrec.teeth.avg[[1]]$Par.name == "p")

# Actually make the plot
# plot(ages.midpt, orig.est$estimate, col = 'blue', pch = 16, ylim = c(0, 1),
#      xlab = 'age (Ma)', ylab = 'per capita evolutionary rate')
# segments(x0 = ages.midpt, y0 = orig.est$lcl, y1 = orig.est$ucl)
# 
# points(ages.midpt, 1-ext.est$estimate, col = 'red', pch = 16)
# segments(x0=ages.midpt,  y0=1-ext.est$ucl,
#          x1=ages.midpt, y1=1-ext.est$lcl, col = 'red')
# 
# mtext(side = 3, line = 1, "Niederbockstruck et al", font = 2)

writeFile <- 'pdf'
# writeFile <- 'jpg'
# writeFile <- 'off'

fig.dims <- c(7, 11) #Set Figure-dimensions

if(writeFile == 'pdf') {
  pdf('plots/RatesFigure.pdf', height = fig.dims[1], width = fig.dims[2], useDingbats = FALSE)
}

if(writeFile == 'jpg') {
  jpeg('plots/RatesFigure.jpg', height = fig.dims[1], width = fig.dims[2], units = 'in', res = 300)
}

xlims = c(max(chas_dataset$nieder_ages), min(chas_dataset$nieder_ages))

par(mfrow = c(2,1))
par(mar = c(3,6, 2, 3))

hide <- plot(chas_dataset$nieder_ages, chas_dataset$nieder_IAR, 
     xlim = xlims, 
     axes = F, type = 'n', lty = 2, 
     lwd = 1.2, ylab = "", xlab = "")

rec.col <- 'gray90'

#plot rectangle
rect(53, 0, 49, 50000, col = rec.col, border = rec.col)

par(new = T)
plot(chas_dataset$nieder_ages, chas_dataset$nieder_IAR, 
     xlim = xlims, 
     axes = F, type = 'l', lty = 2, 
     lwd = 1.2, ylab = "", xlab = "")


axis(4, cex.axis = 0.8, at = pretty(chas_dataset$nieder_IAR, n = 10))


par(new = T)
plot(ages.midpt, orig.est$estimate, col = palette.colors()[3], type = 'o', 
     ylim = c(0, 1), ylab = '', xlab = '', 
     xlim = xlims, 
     pch = 16, axes = F, cex = 1.3)

axis(side = 1, at = pretty(c(61.03830, ages.midpt), n = 10))
axis(side = 2, at = pretty(c(0,1), n = 10), cex.axis = 0.8)

lines(ages.midpt, 1- ext.est$estimate, col = palette.colors()[2], type = 'o', 
      ylim = c(0.8, 1.2), ylab = '', xlab = '', pch = 17, cex = 1.3, xlim = xlims)

# par(new = T)
# plot(foote.nieder$ages, foote.nieder$Ndiv.est, col=palette.colors()[1], pch=15, 
#      type='o', axes = F, ylab = "", xlab = "", xlim = xlims)
# 
# axis(side = 2, at = pretty(c(min(foote.nieder$Ndiv.est),140), 
#                            n = 10), line = 2.7, cex.axis = 0.8)

legend ('topleft', 
        legend=c('Origination', 'Extinction', 'Ichthyolith Accumulation Rate (IAR)'), 
        col=c(palette.colors()[3], palette.colors()[2], 'black'), lty=c(1,1,3),
        pch=c(16, 17, NA), cex = 0.8, bty = 'n')

mtext("Per Morphotype Evolutionary Rate", side = 2, line = 1.8, cex = 0.7)
mtext("Year (Ma)", side = 1, line = 1.8, cex = 0.7)
mtext("Capture Mark Recapture method", side = 3, line = 0.2, cex = 1.2, font = 2)

mtext("a", side = 3, line = 0.5, 
      cex = 1.4, font = 2, at = 63.5)

mtext(expression(paste("IAR (ich/", "cm"^"2", "/Myr)")), 
      side = 4, line = 1.8, cex = 0.7)
mtext("Diversity (total estimated)", side = 2, line = 4.4, cex = 0.7)

########## plot 2 with foot
foote.fn <- function(counts.table, ...) {
  
  # Numeric ages
  ages <- as.numeric(rownames(counts.table))
  
  dt<-c()                           # change in time (interval)
  for(i in 1:(length(ages))-1) {
    dt[i]<-(ages[i+1]-ages[i]) }
  
  # Taxa
  taxa <- colnames(counts.table)
  
  # Age datum table
  ad <- lad.fad.fn(counts.table, ...)
  
  ### Do he Foote calculations
  # a) count up crossers
  
  Nfl<-c()  # singletons (no boundary crossers)
  Nbl<-c()  # bottom crossers only
  Nft<-c()  # top crossers only
  Nbt<-c()  # both bottom and top crossers
  for (i in 1:length(ages)) {
    #subset of all species present or assumed present at the time point [i]
    ad.sub<-subset(ad, fads >= ages[i] & lads <= ages[i])
    #how many of each type of occurrance are present?
    Nfl[i] <- length(subset(ad.sub, fads==lads)[,1])  # "singletons" (no boundary cross)
    Nbl[i] <- length(subset(ad.sub, fads>=ages[i] & lads==ages[i] & fads!=lads)[,1]) # bottom crossers only
    Nft[i] <- length(subset(ad.sub, fads==ages[i] & lads<=ages[i] & fads!=lads)[,1]) # top crossers only
    Nbt[i] <- length(subset(ad.sub, fads> ages[i] & lads< ages[i])[,1]) # both top and bottom  crossers
  }
  
  
  # b) calculate relevant metrics
  Ntot <- apply(cbind(Nfl, Nbl, Nft, Nbt), 1, sum) # total Diversity observed
  Nb   <- apply(cbind(Nbl, Nbt), 1, sum)           # All bottom boundary crossers
  Nt   <- apply(cbind(Nft, Nbt), 1, sum)           # all top boundary crossers
  No   <- apply(cbind(Nfl, Nft), 1, sum)           # number of originations
  Ne   <- apply(cbind(Nfl, Nbl), 1, sum)           # number of extinctions
  Ndiv <- apply(cbind(Nb, Nt), 1, function(x) sum(x)/2)     # estimated mean standing diversity
  pp   <- apply(cbind(Nbt, Nt, c(dt,0)), 1, function(x) {-log(x[1]/x[2]) / x[3]}) # per capita origination
  qq   <- apply(cbind(Nbt, Nb, c(0,dt)), 1, function(x) {-log(x[1]/x[2]) / x[3]}) # per capita extinction
  
  
  df.foote <- data.frame(ages = ages, orig = pp, ext = qq, Ndiv.est = Ndiv, Ntot.obs = Ntot, Norig = No, Next = Ne) #, Nfl = Nfl, Nbl = Nbl, Nft = Nft, Nbt = Nbt, Nb = Nb, Nt = Nt) for troubleshooting
  
  return(df.foote)
  
}

##### 2. lad.fad.fn #####
# takes counts.table as input. Necessary to run foote.fn

lad.fad.fn <- function(counts.table, ages.vector) {
  
  if(missing(ages.vector)) {ages.vector <- as.numeric(rownames(counts.table))}
  
  df <- data.frame(taxon = colnames(counts.table), fads = NA, lads = NA)
  
  # Iterate across each species [could probably also do this with apply, but this is easier]
  for(i in 1:dim(counts.table)[2]) {
    occur <- ages.vector[which(counts.table[,i] != 0)] # What age values are non-zero?
    df$fads[i] <- max(occur) #first occurrence = oldest
    df$lads[i] <- min(occur) #last occurrence = youngest
  }
  
  return(df)
  
}


hide <- plot(chas_dataset$nieder_ages, chas_dataset$nieder_IAR, 
             xlim = xlims, 
             axes = F, type = 'l', lty = 2, lwd = 1.2, xlab = "", ylab = "")

rec.col <- 'gray90'

#plot rectangle
rect(53, 0, 49, 50000, col = rec.col, border = rec.col)

par(new = T)
plot(chas_dataset$nieder_ages, chas_dataset$nieder_IAR, 
     xlim = xlims, 
     axes = F, type = 'l', lty = 2, lwd = 1.2, xlab = "", ylab = "")

axis(4, cex.axis = 0.8, at = pretty(chas_dataset$nieder_IAR, n = 10))

par(new = T)
foote.nieder <- foote.fn(morph.counts.nieder)
plot(foote.nieder$ages, foote.nieder$orig, col=palette.colors()[3], pch=16, type='o',
     xlab = 'age (Ma)', ylab = '', ylim = c(0,0.6), 
     xlim = xlims, 
     axes = F, cex = 1.3)

axis(side = 1, at = pretty(chas_dataset$nieder_ages, n = 10))
axis(side = 2, at = pretty(c(0,0.6), n = 10), cex.axis = 0.8)

points(foote.nieder$ages, foote.nieder$ext, col=palette.colors()[2], 
       pch=17, type='o', cex = 1.3, xlim = xlims)

mtext("b", side = 3, line = 0.5, 
      cex = 1.4, font = 2, at = 63.5)

par(new = T)
plot(foote.nieder$ages, foote.nieder$Ndiv.est, col=palette.colors()[1], pch=15, 
     type='o', axes = F, ylab = "", xlab = "", xlim = xlims)

axis(side = 2, at = pretty(c(min(foote.nieder$Ndiv.est),140), 
                           n = 10), line = 2.7, cex.axis = 0.8)

legend ('topleft', 
        legend=c('Origination', 'Extinction', 'Ichthyolith Accumulation Rate (IAR)', 'Estimated diversity'), 
        col=c(palette.colors()[3], palette.colors()[2], 'black', 'black'), lty=c(1,1,3,1),
        pch=c(16, 17, NA, 15), cex = 0.8, bty = 'n')

mtext("Foote (2000) method", side = 3, line = 0.3, cex = 1.2, font = 2)
mtext("Per Morphotype Evolutionary Rate", side = 2, line = 1.8, cex = 0.7)
mtext("Year (Ma)", side = 1, line = 1.8, cex = 0.7)
mtext(expression(paste("IAR (ich/", "cm"^"2", "/Myr)")), 
      side = 4, line = 1.8, cex = 0.7)
mtext("Diversity (total estimated)", side = 2, line = 4.4, cex = 0.7)

# close file
if(writeFile != 'off') {
  dev.off()
}
