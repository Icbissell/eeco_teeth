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


## Plot code for Origination varies through time, extinction does not

plot(ages.midpt, teeth.orig$estimate, col = 'blue', pch = 16, ylim = c(0, 0.5),
     xlab = 'age (Ma)', ylab = 'per capita evolutionary rate')
segments(x0 = ages.midpt, y0 = teeth.orig$lcl, y1 = teeth.orig$ucl)

points(ages.midpt, rep(1-teeth.ext$estimate, length(ages.midpt)), col = 'red', pch = 16)
segments(x0=ages.midpt,  y0=rep(1-teeth.ext$ucl, length(ages.midpt)),
         x1=ages.midpt, y1=rep(1-teeth.ext$lcl, length(ages.midpt)))

## Plot code for Origination rate is constant and Extinction varies
plot(ages.midpt, rep(teeth.orig$estimate, 22), col = 'blue', pch = 16, ylim = c(0, 0.5),
     xlab = 'age (Ma)', ylab = 'per capita evolutionary rate')
segments(x0 = ages.midpt, y0 = rep(teeth.orig$lcl, 22), y1 = rep(teeth.orig$ucl, 22))

points(ages.midpt, 1-teeth.ext$estimate, col = 'red', pch = 16)
segments(x0=ages.midpt,  y0=1-teeth.ext$ucl,
         x1=ages.midpt, y1=1-teeth.ext$lcl, col = 'red')


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
plot(ages.midpt, orig.est$estimate, col = 'blue', pch = 16, ylim = c(0, 1),
     xlab = 'age (Ma)', ylab = 'per capita evolutionary rate')
segments(x0 = ages.midpt, y0 = orig.est$lcl, y1 = orig.est$ucl)

points(ages.midpt, 1-ext.est$estimate, col = 'red', pch = 16)
segments(x0=ages.midpt,  y0=1-ext.est$ucl,
         x1=ages.midpt, y1=1-ext.est$lcl, col = 'red')

mtext(side = 3, line = 1, "Niederbockstruck et al", font = 2)

# mtext(side = 3, line = 1, "Shipboard", font = 2)

# #second best fit model
# Pradrec.pars <- par.estimates(Pradrec.teeth, 2) #pull the best rank model parameters
# teeth.ext <- Pradrec.pars$Phi
# teeth.orig <- Pradrec.pars$f
#
#
# plot(ages.midpt, teeth.orig$estimate, col = 'blue', pch = 16, ylim = c(0, 0.5),
#      xlab = 'age (Ma)', ylab = 'per capita evolutionary rate')
# segments(x0 = ages.midpt, y0 = teeth.orig$lcl, y1 = teeth.orig$ucl)
# points(ages.midpt, rep(1-teeth.ext$estimate, length(ages.midpt)), col = 'red', pch = 16)
# segments(x0=ages.midpt,  y0=rep(1-teeth.ext$ucl, length(ages.midpt)),
#          x1=ages.midpt, y1=rep(1-teeth.ext$lcl, length(ages.midpt)))



## Calculate plus-minus standard error

plot(ages.midpt, orig.est$estimate, col = 'blue', pch = 16, ylim = c(0, 1),
     xlab = 'age (Ma)', ylab = 'per capita evolutionary rate')
segments(x0 = ages.midpt,
         y0 = (orig.est$estimate - orig.est$se), y1 = (orig.est$estimate + orig.est$se))

points(ages.midpt, 1-ext.est$estimate, col = 'red', pch = 16)
segments(x0 = ages.midpt,
         y0 = 1-(ext.est$estimate - ext.est$se),
         y1 = 1-(ext.est$estimate + ext.est$se), col = 'red')


plot(ages.midpt, orig.est$estimate, col = 'blue', pch = 16, ylim = c(0, 1),
     xlab = 'age (Ma)', ylab = 'per capita evolutionary rate')
segments(x0 = ages.midpt,
         y0 = (orig.est$estimate - orig.est$se), y1 = (orig.est$estimate + orig.est$se))

points(ages.midpt, 1-ext.est$estimate, col = 'red', pch = 16)
segments(x0 = ages.midpt,
         y0 = 1-(ext.est$estimate - ext.est$se),
         y1 = 1-(ext.est$estimate + ext.est$se), col = 'red')


