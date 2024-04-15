#####################################
#                                   #
#     Sample-specific tooth length  #
#        daataset processing        #
#                                   #
#####################################

## Can be run as source ##


##### Libraries #####

library(stringr) # for text matching
library(plyr) # for rbind.fill

##### Functions #####

# define function to cut tooth length matrix to right size
cut_to <- function(lengthID, total_data) {
   #create shorter object ID to match between datasets
   objID <- lengthID$ObjectID[1]
   objId_cut <- sub("^(([^_]*_){5}[^_]*).*", "\\1", objID)

   #Shorten large dataset to only those with object id
   match_id <- total_data[grepl(objId_cut, total_data$Sample.ID, fixed = TRUE), ]
   match_id <- match_id[match_id$Tooth.Dent == "1", ]

   teeth <- data.frame(lengthID)
   teeth <- teeth[0, ]

   #match lengths with tooth IDs
   for(i in 1:length(match_id$Object)){
      if("TRUE" %in% str_detect(lengthID$ObjectID, match_id$Object[i])) {
         index <- lengthID$ObjectID[str_detect(lengthID$ObjectID, match_id$Object[i])]
         teeth[nrow(teeth) + 1, ] = lengthID[which(lengthID$ObjectID == index), ]
      }
   }
   return(teeth)
}

##### Tooth Length Dataset Processing #####


### Tooth length datasets ##
## Call in datasets
length_93 <- read.csv("data/93_morph2d_properties.csv")
length_95 <- read.csv("data/95_morph2d_properties.csv")
length_97 <- read.csv("data/97_morph2d_properties.csv")
length_99 <- read.csv("data/99_morph2d_properties.csv")
length_101_1 <- read.csv("data/101_1_morph2d_properties.csv")
length_101_2 <- read.csv("data/101_2_morph2d_properties.csv")
length_103 <- read.csv("data/103_morph2d_properties.csv")
length_105 <- read.csv("data/105_morph2d_properties.csv")
length_107 <- read.csv("data/107_morph2d_properties.csv")
length_109 <- read.csv("data/109_morph2d_properties.csv")
length_111 <- read.csv("data/111_morph2d_properties.csv")
length_113 <- read.csv("data/113_morph2d_properties.csv")
length_115_1 <- read.csv("data/115_1_morph2d_properties.csv")
length_115_2 <- read.csv("data/115_2_morph2d_properties.csv")
length_117 <- read.csv("data/117_morph2d_properties.csv")
length_119 <- read.csv("data/119_morph2d_properties.csv")
length_121_1 <- read.csv("data/121_1_morph2d_properties.csv")
length_121_2 <- read.csv("data/121_2_morph2d_properties.csv")
length_123 <- read.csv("data/123_morph2d_properties.csv")
length_125 <- read.csv("data/125_morph2d_properties.csv")
length_130_1 <- read.csv("data/130_1_morph2d_properties.csv")
length_130_2 <- read.csv("data/130_2_morph2d_properties.csv")
length_131 <- read.csv("data/131_morph2d_properties.csv")
length_133_1 <- read.csv("data/133_1_morph2d_properties.csv")
length_133_2 <- read.csv("data/133_2_morph2d_properties.csv")
length_135 <- read.csv("data/135_morph2d_properties.csv")
length_139 <- read.csv("data/139_morph2d_properties.csv")
length_143 <- read.csv("data/143_morph2d_properties.csv")

## Process tooth length datasets to only contain teeth that are ID'd in the morphotypes dataset
teeth_93 <- cut_to(length_93, morphs)
teeth_95 <- cut_to(length_95, morphs)
teeth_97 <- cut_to(length_97, morphs)
teeth_99 <- cut_to(length_99, morphs)
teeth_101_1 <- cut_to(length_101_1, morphs)
teeth_101_2 <- cut_to(length_101_2, morphs)
teeth_103 <- cut_to(length_103, morphs)
teeth_105 <- cut_to(length_105, morphs)
teeth_107 <- cut_to(length_107, morphs)
teeth_109 <- cut_to(length_109, morphs)
teeth_111 <- cut_to(length_111, morphs)
teeth_113 <- cut_to(length_113, morphs)
teeth_115_1 <- cut_to(length_115_1, morphs)
teeth_115_2 <- cut_to(length_115_2, morphs)
teeth_117 <- cut_to(length_117, morphs)
teeth_119 <- cut_to(length_119, morphs)

# remove one problematic tooth from sample #119 #
teeth_119 <- teeth_119[-which(teeth_119$ObjectID == "119_U1553D_4R_4W_20-23cm_N1of1_Z200x_obj00615_plane000"), ]

teeth_121_1 <- cut_to(length_121_1, morphs)
teeth_121_2 <- cut_to(length_121_2, morphs)
teeth_123 <- cut_to(length_123, morphs)
teeth_125 <- cut_to(length_125, morphs)
teeth_130_1 <- cut_to(length_130_1, morphs)
teeth_130_2 <- cut_to(length_130_2, morphs)
teeth_131 <- cut_to(length_131, morphs)
teeth_133_1 <- cut_to(length_133_1, morphs)
teeth_133_2 <- cut_to(length_133_2, morphs)
teeth_135 <- cut_to(length_135, morphs)
teeth_139 <- cut_to(length_139, morphs)
teeth_143 <- cut_to(length_143, morphs)


## combine multi-hole sections
teeth_101 <- rbind(teeth_101_1, teeth_101_2)
teeth_101["SampleID"] <- "101"
rm(teeth_101_1, teeth_101_2)

teeth_115 <- rbind(teeth_115_1, teeth_115_2)
teeth_115["SampleID"] <- "115"
rm(teeth_115_1, teeth_115_2)

teeth_121 <- rbind(teeth_121_1, teeth_121_2)
teeth_121["SampleID"] <- "121"
rm(teeth_121_1, teeth_121_2)

teeth_130 <- rbind(teeth_130_1, teeth_130_2)
teeth_130["SampleID"] <- "130"
rm(teeth_130_1, teeth_130_2)

teeth_133 <- rbind(teeth_133_1, teeth_133_2)
teeth_133["SampleID"] <- "133"
rm(teeth_133_1, teeth_133_2)

## combine all dfs into big frame
teeth_list <- mget(ls(pattern = "^teeth_*"))
teeth_total <- rbind.fill(teeth_list)

# add column that sets length as the max of width and length
teeth_total$length <- pmax(teeth_total$Height, teeth_total$Width)

### Add age columns to teeth_total
teeth_total$shipboard_ages <- all_data[match(teeth_total$SampleID, all_data$Serial..),18] # Pull the shipboard ages (column 18 of all_data) value for the morphs

teeth_total$nieder_ages <- all_data[match(teeth_total$SampleID, all_data$Serial..),21] # Pull the Niederbockstruck ages (column 21 of all_data) value for the morphs

# Check that I got the right - should be all true
sort(unique(teeth_total$shipboard_ages))==chas_dataset$shipboard_ages #should be all true
sort(unique(teeth_total$nieder_ages))==chas_dataset$nieder_ages #should be all true

# write teeth total to csv
write.csv(teeth_total, "data/teeth_total.csv", row.names = FALSE) ##Elizabeth comment: Make row.names = false in this writeCSV



##### Calculate mean length through time ######

length_means <- aggregate(teeth_total$length, list(teeth_total$SampleID), FUN = mean)
# Sort to have ascending sample number
length_means <- length_means[sort(as.numeric(length_means$Group.1), index.return = TRUE)$ix,]

# rename the headers to be more informative
colnames(length_means)[1] = "SampleID"
colnames(length_means)[2] = "length_mean"

# Stdev
length_sd <- aggregate(teeth_total$length, list(teeth_total$SampleID), FUN = sd)
length_sd <- length_sd[sort(as.numeric(length_sd$Group.1), index.return = TRUE)$ix,]
colnames(length_sd)[1] = "SampleID"
colnames(length_sd)[2] = "length_sd"

length_stats <- cbind(length_means, length_sd = length_sd$length_sd)

length_stats$shipboard_ages <- all_data[match(length_stats$SampleID, all_data$Serial..),18] # Pull the shipboard ages (column 18 of all_data) value for the morphs
length_stats$nieder_ages <- all_data[match(length_stats$SampleID, all_data$Serial..),21] # Pull the shipboard ages (column 18 of all_data) value for the morphs


#write teeth total to csv
write.csv(length_stats, "data/length_stats.csv", row.names = FALSE) #also make row.names = FALSE here

##### Clean up #####

rm(length_101_1, length_101_2, length_103, length_105, length_107, length_109, length_111, length_113, length_115_1, length_115_2, length_117, length_119, length_121_1, length_121_2, length_123, length_125, length_130_1, length_130_2, length_131, length_133_1, length_133_2, length_135, length_139, length_143, length_93, length_95, length_97, length_99, length_means, length_sd, teeth_101, teeth_103, teeth_105, teeth_107, teeth_109, teeth_111, teeth_113, teeth_115, teeth_117, teeth_119, teeth_121, teeth_123, teeth_125, teeth_130, teeth_131, teeth_133, teeth_135, teeth_139, teeth_143, teeth_93, teeth_95, teeth_97, teeth_99, teeth_list)


###############################
#        Old Code             #
###############################
# ##### Testing using 'morphs' instead of 'total_data' in cut_to function #####
# Morphotypes occurrence dataset is 'morphs' object from setup_age_models.R
# total_data <- read.csv("data/ToothMorph_V0.4_Morphotypes - Chas_U1553.csv")
#
# ## Processing tooth datsets
# teeth_93_old <- cut_to(length_93, total_data)
# teeth_95_old <- cut_to(length_95, total_data)
# teeth_97_old <- cut_to(length_97, total_data)
# teeth_99_old <- cut_to(length_99, total_data)
# teeth_101_1_old <- cut_to(length_101_1, total_data)
# teeth_101_2_old <- cut_to(length_101_2, total_data)
# teeth_103_old <- cut_to(length_103, total_data)
# teeth_105_old <- cut_to(length_105, total_data)
# teeth_107_old <- cut_to(length_107, total_data)
# teeth_109_old <- cut_to(length_109, total_data)
# teeth_111_old <- cut_to(length_111, total_data)
# teeth_113_old <- cut_to(length_113, total_data)
# teeth_115_1_old <- cut_to(length_115_1, total_data)
# teeth_115_2_old <- cut_to(length_115_2, total_data)
# teeth_117_old <- cut_to(length_117, total_data)
# teeth_119_old <- cut_to(length_119, total_data)
#
# # remove one problematic tooth from sample #119 #
# teeth_119_old <- teeth_119_old[-which(teeth_119_old$ObjectID == "119_U1553D_4R_4W_20-23cm_N1of1_Z200x_obj00615_plane000"), ]
#
# teeth_121_1_old <- cut_to(length_121_1, total_data)
# teeth_121_2_old <- cut_to(length_121_2, total_data)
# teeth_123_old <- cut_to(length_123, total_data)
# teeth_125_old <- cut_to(length_125, total_data)
# teeth_130_1_old <- cut_to(length_130_1, total_data)
# teeth_130_2_old <- cut_to(length_130_2, total_data)
# teeth_131_old <- cut_to(length_131, total_data)
# teeth_133_1_old <- cut_to(length_133_1, total_data)
# teeth_133_2_old <- cut_to(length_133_2, total_data)
# teeth_135_old <- cut_to(length_135, total_data)
# teeth_139_old <- cut_to(length_139, total_data)
# teeth_143_old <- cut_to(length_143, total_data)
#
# #Testing to see if these match
# sum(ifelse(teeth_93==teeth_93_old, 0, 1))
# sum(ifelse(teeth_95==teeth_95_old, 0, 1))
# sum(ifelse(teeth_97==teeth_97_old, 0, 1))
# sum(ifelse(teeth_99==teeth_99_old, 0, 1))
# sum(ifelse(teeth_101_1==teeth_101_1_old, 0, 1))
# sum(ifelse(teeth_101_2==teeth_101_2_old, 0, 1))
# sum(ifelse(teeth_103==teeth_103_old, 0, 1))
# sum(ifelse(teeth_105==teeth_105_old, 0, 1))
# sum(ifelse(teeth_107==teeth_107_old, 0, 1))
# sum(ifelse(teeth_109==teeth_109_old, 0, 1))
# sum(ifelse(teeth_111==teeth_111_old, 0, 1))
# sum(ifelse(teeth_113==teeth_113_old, 0, 1))
# sum(ifelse(teeth_115_1==teeth_115_1_old, 0, 1))
# sum(ifelse(teeth_115_2==teeth_115_2_old, 0, 1))
# sum(ifelse(teeth_117==teeth_117_old, 0, 1))
# sum(ifelse(teeth_119==teeth_119_old, 0, 1))
# sum(ifelse(teeth_121_1==teeth_121_1_old, 0, 1))
# sum(ifelse(teeth_121_2==teeth_121_2_old, 0, 1))
# sum(ifelse(teeth_123==teeth_123_old, 0, 1))
# sum(ifelse(teeth_125==teeth_125_old, 0, 1))
# sum(ifelse(teeth_130_1==teeth_130_1_old, 0, 1))
# sum(ifelse(teeth_130_2==teeth_130_2_old, 0, 1))
# sum(ifelse(teeth_131==teeth_131_old, 0, 1))
# sum(ifelse(teeth_133_1==teeth_133_1_old, 0, 1))
# sum(ifelse(teeth_133_2==teeth_133_2_old, 0, 1))
# sum(ifelse(teeth_135==teeth_135_old, 0, 1))
# sum(ifelse(teeth_139==teeth_139_old, 0, 1))
# sum(ifelse(teeth_143==teeth_143_old, 0, 1))

# ## combine multi-hole sections
# teeth_101 <- rbind(teeth_101_1_old, teeth_101_2_old)
# teeth_101["SampleID"] <- "101"
# rm(teeth_101_1_old, teeth_101_2_old)
#
# teeth_115 <- rbind(teeth_115_1_old, teeth_115_2_old)
# teeth_115["SampleID"] <- "115"
# rm(teeth_115_1_old, teeth_115_2_old)
#
# teeth_121 <- rbind(teeth_121_1_old, teeth_121_2_old)
# teeth_121["SampleID"] <- "121"
# rm(teeth_121_1_old, teeth_121_2_old)
#
# teeth_130 <- rbind(teeth_130_1_old, teeth_130_2_old)
# teeth_130["SampleID"] <- "130"
# rm(teeth_130_1_old, teeth_130_2_old)
#
# teeth_133 <- rbind(teeth_133_1_old, teeth_133_2_old)
# teeth_133["SampleID"] <- "133"
# rm(teeth_133_1_old, teeth_133_2_old)
#
# ## combine all dfs into big frame
# teeth_list <- mget(ls(pattern = "^teeth_*"))
# teeth_total <- rbind.fill(teeth_list)
# teeth_total_other <- teeth_total
#
#
# # Clean up for testing
# rm(list=ls(pattern = '*_old'))
#
# # Testing code for if data sets are equal:
# sum(ifelse(teeth_total == teeth_total_other, 0, 1))
#
# ##### Chas code to convert sample ID to age (prior to age model updates, do not use!) #####
# age <- aggregate(total_data$Age, list(total_data$Sample.ID), FUN = mean)
#
# for(i in 1:length(age$Group.1)){
#    age$Group.1[i] <- sub("^(([^_]*_){0}[^_]*).*", "\\1", age$Group.1[i])
# }
#
# colnames(age)[1] = "SampleID"
# colnames(age)[2] = "Age"
#
# teeth_total <- merge(age, teeth_total, by.y = "SampleID", by.x = "SampleID")
#
# #add column that sets length as the max of width and length
# teeth_total$length <- pmax(teeth_total$Height, teeth_total$Width)
#
# #write teeth total to csv
# write.csv(teeth_total, "data/teeth_total.csv") ##Elizabeth comment: Make row.names = false in this writeCSV
#
# length_means <- aggregate(teeth_total$length, list(teeth_total$Age), FUN = mean)
# colnames(length_means)[1] = "age"
# colnames(length_means)[2] = "length"
#
# #write teeth total to csv
# write.csv(length_means, "data/length_means.csv") #also make row.names = FALSE here
#
# ##### Old libraries #####
# library(car)
# library(ggplot2)
# library(zoo)
# library(mgcv)
