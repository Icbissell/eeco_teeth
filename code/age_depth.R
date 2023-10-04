#####################################################
#                                                   #
#                 ageDepth.fn                       #
#                                                   #
#   Takes an age model (age/depth pointers) and     #
#   linearly interpolates ages between points for   #
#   a given set of depths (sample depths) based     #
#   on the pointers.                                #
#                                                   #
#####################################################

ageDepth.fn <- function(ages.pointer, depths.pointer, depths.out, plot.out = FALSE) {
  agemodel <- approx(x = depths.pointer, y = ages.pointer, xout = depths.out)
  
  if(plot.out == TRUE) {
    plot(agemodel, xlab = 'depth', ylab = 'age')
    points(depths.pointer, ages.pointer, pch = 16, col = 'red')
  }
  names(agemodel) <- c('depth', 'age')
  return(agemodel)  
}

###########################
#                         #
#       Example use       #
#                         #
###########################

# Generate a random age-depth model - this would be replaced by reading in a csv of age-depth pointers
ages.pointer <- sort(rnorm(20))
depths.pointer <- sort(rnorm(20))

# generate a set of "depths out" - this is an evenly-distributed sample set, for visualization, but it can be anything
depths.out = seq(from=min(depths.pointer), to = max(depths.pointer), length.out = 50)

# Generate the age-depth model
age.model <- ageDepth.fn(ages.pointer = ages.pointer, depths.pointer = depths.pointer, depths.out = depths.out, plot.out = TRUE)

# # Save the age.depth model output
# writeClipboard(age.model) #copies to clipboard
# write.csv(age.model, 'age.model.csv', row.names = FALSE)


## Just to show that depths.out can be random too and it also works
depths.out <- sort(rnorm(50))
age.model <- ageDepth.fn(ages.pointer = ages.pointer, depths.pointer = depths.pointer, depths.out = depths.out, plot.out = TRUE)


##### This is the inner workings of this #####

# ###########################################
# #   Functions to create age-depth models  #
# ###########################################
# 
# #age model input (copy/pasted from excel...)
# # another way to copy/paste would be: 
# # ages <- scan() # hit enter, then hit ctrl+v to paste whatever is on the clipboard.
# # Obviously you can also just call in a relevant csv file and name the objects/headers appropriately. 
# 
# ages.pointer <- readClipboard() # can really be any series to interpolate over a depth scale... 
# depths.pointer <- readClipboard()
# depths.out <- readClipboard()
# 
# agemodel <- approx(x = depths.pointer, y = ages.pointer, xout = depths.out)
# plot(agemodel)
# points(depths.pointer, ages.pointer, pch = 16, col = 'red')
# 
# writeClipboard(as.character(agemodel$x)) # depths
# writeClipboard(as.character(agemodel$y)) # ages/calculated series
# 
