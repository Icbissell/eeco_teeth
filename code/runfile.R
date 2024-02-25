####################################################
#                                                  #
#     Run File for EECO Fish Data Analyses         #
#           2/13/2023                              #
#                                                  #
####################################################

## Follow this file in order to re-create analyses in this manuscript.

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


###########################################
#                                         #
#                 FIGURES                 #
#                                         #
###########################################

##### age_model_Figures.R: makes 2 figures, compares age-depth and IAR (can be run as source) #####
source('code/age_model_Figures.R')
##### range_charts.R: Range Chart Figures  (can be run as source) #####
source('code/range_charts.R')
##### iar_oxygen_size_figure.R can be run as source #####
source('code/iar_oxygen_size_figure.R')
##### Foote_rates.R (can be run as source) #####
source('code/Foote_rates.R')
##### CMR Figures shouldn't be run as source at this time, but has the bones of the figures #####
# source('code/CMR_Figures.R')
