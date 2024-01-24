##### Libraries #####

library(stringr)
library(car)
library(ggplot2)
library(plyr)
library(zoo)

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


##### Dataset Processing #####

# Morphotypes occurrence dataset
total_data <- read.csv("data/ToothMorph_V0.4_Morphotypes - Chas_U1553.csv")

# Westerhold oxygen data
o_data <- read.csv("data/Westerhold_2020_Oxygen_Carbon_smooth.csv")

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
teeth_93 <- cut_to(length_93, total_data)
teeth_95 <- cut_to(length_95, total_data)
teeth_97 <- cut_to(length_97, total_data)
teeth_99 <- cut_to(length_99, total_data)
teeth_101_1 <- cut_to(length_101_1, total_data)
teeth_101_2 <- cut_to(length_101_2, total_data)
teeth_103 <- cut_to(length_103, total_data)
teeth_105 <- cut_to(length_105, total_data)
teeth_107 <- cut_to(length_107, total_data)
teeth_109 <- cut_to(length_109, total_data)
teeth_111 <- cut_to(length_111, total_data)
teeth_113 <- cut_to(length_113, total_data)
teeth_115_1 <- cut_to(length_115_1, total_data)
teeth_115_2 <- cut_to(length_115_2, total_data)
teeth_117 <- cut_to(length_117, total_data)
teeth_119 <- cut_to(length_119, total_data)

# remove one problematic tooth from sample #119 #
teeth_119 <- teeth_119[-which(teeth_119$ObjectID == "119_U1553D_4R_4W_20-23cm_N1of1_Z200x_obj00615_plane000"), ]

teeth_121_1 <- cut_to(length_121_1, total_data)
teeth_121_2 <- cut_to(length_121_2, total_data)
teeth_123 <- cut_to(length_123, total_data)
teeth_125 <- cut_to(length_125, total_data)
teeth_130_1 <- cut_to(length_130_1, total_data)
teeth_130_2 <- cut_to(length_130_2, total_data)
teeth_131 <- cut_to(length_131, total_data)
teeth_133_1 <- cut_to(length_133_1, total_data)
teeth_133_2 <- cut_to(length_133_2, total_data)
teeth_135 <- cut_to(length_135, total_data)
teeth_139 <- cut_to(length_139, total_data)
teeth_143 <- cut_to(length_143, total_data)

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
teeth_133["SampleID"] <- "130"
rm(teeth_133_1, teeth_133_2)

## combine all dfs into big frame
teeth_list <- mget(ls(pattern = "^teeth_*"))
teeth_total <- rbind.fill(teeth_list)

age <- aggregate(total_data$Age, list(total_data$Sample.ID), FUN = mean)

for(i in 1:length(age$Group.1)){
  age$Group.1[i] <- sub("^(([^_]*_){0}[^_]*).*", "\\1", age$Group.1[i])
}

colnames(age)[1] = "SampleID"
colnames(age)[2] = "Age"

teeth_total <- merge(age, teeth_total, by.y = "SampleID", by.x = "SampleID")

#add column that sets length as the max of width and length
teeth_total$length <- pmax(teeth_total$Height, teeth_total$Width)

#write teeth total to csv
write.csv(teeth_total, "data/teeth_total.csv") ##Elizabeth comment: Make row.names = false in this writeCSV

length_means <- aggregate(teeth_total$length, list(teeth_total$Age), FUN = mean)
colnames(length_means)[1] = "age"
colnames(length_means)[2] = "length"

#write teeth total to csv
write.csv(length_means, "data/length_means.csv") #also make row.names = FALSE here
