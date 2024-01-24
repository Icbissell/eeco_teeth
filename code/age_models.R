############################
#                          #
#     Age Model Setups     #
#                          #
############################

# This script calculates age-depth, sedimentation rate, and ichthyolith accumulation rates for
# the processed samples in the dataset.


##### Functions #####

# Interpolation function for calculating age from age-depth (pointer) and depths.out (sample depths)
ageDepth.fn <- function(ages.pointer, depths.pointer, depths.out, plot.out = FALSE) {
   agemodel <- approx(x = depths.pointer, y = ages.pointer, xout = depths.out)

   if(plot.out == TRUE) {
      plot(agemodel, xlab = 'depth', ylab = 'age')
      points(depths.pointer, ages.pointer, pch = 16, col = 'red')
   }
   names(agemodel) <- c('depth', 'age')
   return(agemodel)
}

# Sedimentation rate calculation

sedRate.cm.myr.fn <- function(ages, depths) {

   sed.rate <- c(0) #give 0 value to first sample - this will be overwritten at the end

   for(i in 2:length(ages)) {
      sed.rate[i] <- ((depths[i]-depths[i-1])*100)/(ages[i]-ages[i-1])
   }
   sed.rate[1] <- sed.rate[2]
   return(sed.rate)
}


##### Call in and select relevant data for EECO Dataset from whole processing log #####

all_data <- read.csv('data/U1553_Processing_Log - Sample_Processing.csv', skip = 2, header = TRUE)

# pull full IODP sample information, ccsf, dry bulk density (DBD), total ichthyoliths for IAR calculation
relevant_columns <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,23,52)
# Be sure to check this!! Really just need sample number, ccsf, DBD, and total ichthyoliths

all_data <- all_data[,relevant_columns]


##### Age-Depth Tables for age models used in this study #####

### Shipboard age depth model ###
shipboard_age_table <- read.csv('data/U1553_Processing_Log - Age_Model_Shipboard_T19.csv',
                                skip = 4, header = TRUE)

shipboard_pointers <- data.frame(age = shipboard_age_table$Age..Ma.,
                                 ccsf.mid = shipboard_age_table$Midpoint_ccsf.a)

#Calculate age-depth and view plot
shipboard_ages <- ageDepth.fn(ages.pointer = shipboard_pointers$age,
                              depths.pointer = shipboard_pointers$ccsf.mid,
                              depths.out = all_data$Mid.depth.CCSF..calc.,
                              plot.out = F)

all_data$shipboard_ages <- shipboard_ages$age

all_data$shipboard_sedrate <- sedRate.cm.myr.fn(ages = all_data$shipboard_ages,
                                                depths = all_data$Mid.depth.CCSF..calc.)

# Calculate IAR: IAR (ich/cm2/myr) = ich/g * g/cm3 * cm/myr
all_data$shipboard_IAR <- (all_data$Total.Ichthyoliths..manual./all_data$Dry.Weight) * all_data$shipboard_sedrate * all_data$DBD..variable..g.cm.3.


#### Niederbockstruck et al age model ###

nieder_age_table <- read.csv('data/U1553_Processing_Log - Age_Model_Niederbockstruck_splice.csv',
                             skip = 3, header = TRUE)
nieder_pointers <- data.frame(age = nieder_age_table$Age.Ma,
                              ccsf.mid = nieder_age_table$mid_ccsf.a)

#Calculate age-depth and view plot
nieder_ages <- ageDepth.fn(ages.pointer = nieder_pointers$age,
                           depths.pointer = nieder_pointers$ccsf.mid,
                           depths.out = all_data$Mid.depth.CCSF..calc.,
                           plot.out = F)


all_data$nieder_ages <- nieder_ages$age

all_data$nieder_sedrate <- sedRate.cm.myr.fn(ages = all_data$nieder_ages,
                                             depths = all_data$Mid.depth.CCSF..calc.)

all_data$nieder_IAR <- (all_data$Total.Ichthyoliths..manual./all_data$Dry.Weight) * all_data$nieder_sedrate * all_data$DBD..variable..g.cm.3.


##### Pull only the E. Eocene samples that were processed as part of this study #####
chas_sample_IDs <- c(93, 95, 97, 99, 101, 103, 105, 107, 109, 111, 113, 115,
                     117, 119, 121, 123, 125, 130, 131, 133, 135, 139, 143)

chas_dataset <- all_data[chas_sample_IDs,]

##### Figures comparing age models #####

### 2-panel age-depth plot ###
par(mfrow = c(1,2))
# shipboard age model
shipboard_ages <- ageDepth.fn(ages.pointer = shipboard_pointers$age,
                              depths.pointer = shipboard_pointers$ccsf.mid,
                              depths.out = all_data$Mid.depth.CCSF..calc.,
                              plot.out = T)
mtext("Shipboard Age Model", side = 3, line = 1, cex = 1.5, font = 2)
abline(v=min(chas_dataset$Mid.depth.CCSF..calc.), lty = 3)
abline(v=max(chas_dataset$Mid.depth.CCSF..calc.), lty = 3)

# Niederbockstruck age model
nieder_ages <- ageDepth.fn(ages.pointer = nieder_pointers$age,
                           depths.pointer = nieder_pointers$ccsf.mid,
                           depths.out = all_data$Mid.depth.CCSF..calc.,
                           plot.out = T)
mtext("Niederbockstruck Age Model", side = 3, line = 1, cex = 1.5, font = 2)
abline(v=min(chas_dataset$Mid.depth.CCSF..calc.), lty = 3)
abline(v=max(chas_dataset$Mid.depth.CCSF..calc.), lty = 3)


### Line plots comparing the two different IARs ###
par(mfrow = c(1,1))
plot(chas_dataset$shipboard_ages, chas_dataset$shipboard_IAR,
     type = 'l', col = 'darkorange', lwd = 1.5,
     xlab = '',
     ylab = '')
points(chas_dataset$nieder_ages, chas_dataset$nieder_IAR, type = 'l', col = 'dodgerblue', lwd = 1.5)

# Add data points to compare exact samples to each other:
points(chas_dataset$shipboard_ages, chas_dataset$shipboard_IAR, pch = c(1:23))
points(chas_dataset$nieder_ages, chas_dataset$nieder_IAR, pch = c(1:23))

# legend and Axis labels
legend('topright', legend = c("Shipboard", "Niederbockstruck"),
       lty = 1, lwd = 1.5, col = c('darkorange', 'dodgerblue'))
mtext('Age (Ma)', side = 1, line = 3)
mtext(expression("Ichthyoliths " * cm^-2 * myr^-1), side = 2, line = 2.5)
mtext("Ichthyolith Accumulation Rate", side = 3, line = 1, cex = 1.2, font = 2)




################### Range Chart Comparisons #####################
morphs <- read.csv("data/ToothMorph_V0.4_Morphotypes - Chas_U1553.csv")

age.lookup.fn <- function(x, dataset, key.column, return.column) {
   lookup_table <- data.frame(key = dataset[,key.column], return = dataset[,return.column])
   xx <- which(key.column %in% x) #figure out the right array line to look up
   return.value <- dataset[xx, return.column]
   return(return.value)
}

morphs$shipboard_ages <- apply(morphs$Sample.number, 1, age.lookup.fn)

# which(chas_dataset$Serial.. %in% x) #returns correct values
