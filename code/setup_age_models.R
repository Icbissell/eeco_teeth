##################################
#                                #
#        Setup Datasets:         #
#        -IAR/Age models         #
#        -Morphotypes            #
#        -Published datasets     #
#                                #
##################################

## This script can be run as source ##
## It creates the objects chas_dataset, which includes shipboard and
#     Neiderbockstruck et al ages for all samples in the dataset. Note that
#     as this dataset is
# The main outputs from this script that are used for figure-making and further analyses are:
#     1. chas_data: a data frame with all 23 data points, as well as both age models
#        and calculated IAR values for each data point
#     2. morph.counts.nieder and morph.counts.shipboard: Morphotype occurrence tables
#        with rownames that have the respective ages for each age model, for range charts
#        and evolutionary rate calculations
#     3. morphs object is used in compute_length.R
#     4. age.range is the min/max ages from the datasets

############################
#                          #
#        Libraries         #
#                          #
############################
library(zoo) #for rollmean
library(mgcv) #for gam
library(ichthyoliths) #for range chart
library(viridis) #for range chart

############################
#                          #
#     Age Model Setup      #
#                          #
############################

# This script calculates age-depth, sedimentation rate, and ichthyolith accumulation rates for
# the processed samples in the dataset.

#########################
#                       #
#        Functions      #
#                       #
#########################

###### ageDepth.fn for calculating age-depth #####
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

##### sedRate.cm.myr.fn Sedimentation rate calculation #####

sedRate.cm.myr.fn <- function(ages, depths) {

   sed.rate <- c(0) #give 0 value to first sample - this will be overwritten at the end

   for(i in 2:length(ages)) {
      sed.rate[i] <- ((depths[i]-depths[i-1])*100)/(ages[i]-ages[i-1])
   }
   sed.rate[1] <- sed.rate[2]
   return(sed.rate)
}


###############################
#                             #
#        IAR Datasets         #
#                             #
###############################

##### Call in and select relevant data for EECO Dataset from whole processing log #####

all_data <- read.csv('data/U1553_Processing_Log - Sample_Processing.csv', skip = 2, header = TRUE)

# pull full IODP sample information, ccsf, dry bulk density (DBD), total ichthyoliths for IAR calculation
relevant_columns <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,23,52)
# Be sure to check this!! Really just need sample number, ccsf, DBD, and total ichthyoliths

all_data <- all_data[,relevant_columns]


##### Age-Depth Tables for age models used in this study #####

##### Shipboard age depth model #####
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


##### Niederbockstruck et al age model #####

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


##### chas_dataset <- Pull only the E. Eocene samples that were processed as part of this study #####
chas_sample_IDs <- c(93, 95, 97, 99, 101, 103, 105, 107, 109, 111, 113, 115,
                     117, 119, 121, 123, 125, 130, 131, 133, 135, 139, 143)

chas_dataset <- all_data[chas_sample_IDs,]

# Age range is rounded to million year bounds above and below the youngest points.
age.range <- c(floor(min(chas_dataset$shipboard_ages, chas_dataset$nieder_ages)), ceiling(max(chas_dataset$shipboard_ages, chas_dataset$nieder_ages)))


#####################################
#                                   #
#        Morphotype Datasets        #
#                                   #
#####################################
##### Call in and Clean Morphotypes Dataset #####

# Call in dataset
morphs <- read.csv("data/ToothMorph_V0.4_Morphotypes - Chas_U1553.csv")

# Clean the morphs dataset to only have named morphotypes
morphs <- morphs[!morphs$Alias=="",]


##### Shipboard Age Model #####

## Assign shipboard-based ages
morphs$shipboard_ages <- all_data[match(morphs$Sample.number, all_data$Serial..),18] # Pull the shipboard ages (column 18) value for the morphs

## Testing to see if the lookup pulled the right ages:
sort(unique(morphs$shipboard_ages))==chas_dataset$shipboard_ages #should be all true
# Yay this worked!

# Make counts matrix for plotting
morph.age.shipboard <- data.frame(age = morphs$shipboard_ages, morphotype = morphs$Alias)
morph.counts.shipboard <- table(morph.age.shipboard)


###### Niederbockstruck et al age model #####

## Assign Niederbockstruck ages
morphs$nieder_ages <- all_data[match(morphs$Sample.number, all_data$Serial..),21] # Pull the Niederbockstruck et al ages (column 21) value for the morphs

## Testing to see if the lookup pulled the right ages:
sort(unique(morphs$nieder_ages))==chas_dataset$nieder_ages #should be all true
# Yay this worked!


# Make counts matrix for plotting
morph.age.nieder <- data.frame(age = morphs$nieder_ages, morphotype = morphs$Alias)
morph.counts.nieder <- table(morph.age.nieder)




##################################
#                                #
#     Published Datasets Import  #
#                                #
##################################

##### Westerhold 2020 oxygen data #####

# import
o_data <- read.csv("data/Westerhold_2020_Oxygen_Carbon_smooth.csv")

# process oxygen dataset:
#prepare temperature and length tests
o_df1 <- data.frame(age = as.numeric(o_data$Age_Ma), d18O = as.numeric(o_data$d18O_loess_smooth))
o_df1 = o_df1[(o_df1$age >= min(age.range)) & (o_df1$age <= max(age.range)), ]

#compute rolling mean of temperature data
o_roll <- data.frame(rollmean(o_df1, 100))


##### DSDP Site 596 IAR - Sibert et al 2016/Britten and Sibert 2020 #####
IAR.596 <- read.csv("data/DSDP_596_Fish_Accumulation_siteid_1_132.csv")
# Only plotting data within our time range
IAR.596 <- IAR.596[(IAR.596$age > min(age.range)) & (IAR.596$age < max(age.range)), ]

############################
#                          #
#        Clean up          #
#                          #
############################
rm(relevant_columns, nieder_ages, shipboard_ages)
