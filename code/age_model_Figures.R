###############################
#                             #
#     Age Model Figures       #
#                             #
###############################

##### Figures comparing age models #####

par(mar = c(5.1,4.1,4.1,2.1)) #reset default margins

##### 2-panel age-depth plot #####
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


##### Line plots comparing the two different IARs #####
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


##### Reset graphical parameters #####

par(mfrow = c(1,1))
