#################################################################
#                                                               #
#       Code for calculating Extinction and Origination Rates   #
#           Following Foote 2000, Paleobiology                  #
#                                                               #
#################################################################

# Full citation: Foote, M. (2000). "Origination and extinction components of taxonomic diversity: general problems." Paleobiology 26(sp4): 74-102.

# This function takes in a counts occurrence table (rows = ages, columns = taxa) and calculates
#   per-capita origination and exionction rates.
#   It also estimates total standing diversity
#   Output is in the form of a data frame.

### Source both functions in this script (fads.lads.fn and foote.fn) to use foote.fn

##### 0. Turn the dataset into a counts.table #####
# Use the table function to convert a data frame with age and morphotype columns:
# counts.table <- table(data$ages, data$morphotype)

##################################
#                                #
#     Internal functions for     #
#        Foote Calculation       #
#                                #
##################################

##### 1. foote.fn #####
# output:   pp (orig) - per capita origination
#           qq (ext) - per capita extinction
#           Ndiv.est - estimated standing diversity
#           Ntot.obs - Observed Taxa
#           Norig - Number of new taxa
#           Next - Number of taxa going extinct


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

##### 3. foote.plot #####

foote.plot <- function(foote, ...) {

   # if(class(foote) == 'matrix') foote <- foote.metrics(sample.matrix)

   plot(foote$ages, foote$orig, col='blue', pch=16, type='o',
        # main='Foote 2000 boundary crosser extinction and origination',
        xlab = 'age (Ma)', ylab = 'origination/extinction rate', ...)  #AgeID.unique
   points(foote$ages, foote$ext, col='red', pch=16, type='o')
   legend ('topright', legend=c('origination', 'extinction', 'Diversity (in sample)', 'Diversity (total est)'), col=c('blue', 'red', 'black', 'black'), lty=1, pch=c(16, 16, 1, 16))

   # add diversity estimated
   par(new=T)
   plot(foote$ages, foote$Ndiv.est, axes=F, type = 'o', xlab = '', ylab = '')
   axis(4)

   # add diversity total
   points(foote$ages, foote$Ntot, pch=16)
}

###############################
#                             #
#     Comparing age models    #
#        Foote figures        #
#                             #
###############################

par(mfrow = c(2,1))

##### Shipboard Age Model Calculations & Plot #####
foote.shipboard <- foote.fn(morph.counts.shipboard)
foote.plot(foote.shipboard)
mtext("Shipboard age model", side = 3, line = 1, cex = 1.2, font = 2)

##### Niederbockstruck et al Age Model Calculations & Plot #####
foote.nieder <- foote.fn(morph.counts.nieder)
foote.plot(foote.nieder)
mtext("Niederbockstruck et al age model", side = 3, line = 1, cex = 1.2, font = 2)



##### Reset and clean up #####
par(mfrow = c(1,1))
