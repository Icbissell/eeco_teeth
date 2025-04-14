########################################
#                                      #
#        IAR/Size/Oxygen Figure        #
#                                      #
########################################

## can now be run as source ##

library(mgcv) #for GAM

##### Scripts to run first: #####
# source('code/setup_age_models.R')
# source('code/compute_length_updated.R')

# writeFile <- 'pdf'
# writeFile <- 'jpg'
# writeFile <- 'off'
writeFile <- 'png'

fig.dims <- c(7, 11) #Set Figure-dimensions

if(writeFile == 'pdf') {
  pdf('plots/iar_oxy.pdf', height = fig.dims[1], width = fig.dims[2], useDingbats = FALSE)
  # cairo_pdf('plots/iar_oxy.pdf', height = fig.dims[1], width = fig.dims[2])
  }

if(writeFile == 'jpg') {
  jpeg('plots/iar_oxy.jpg', height = fig.dims[1], width = fig.dims[2], units = 'in', res = 300)
}

if(writeFile == 'png') {
  png('plots/iar_oxy.png', height = fig.dims[1], width = fig.dims[2], units = 'in', res =1000)
}

##### Graphical Parameters for the whole plot #####
#set plotting vars
yaxis.age <- rev(round(age.range)) # c(63, 41)
axis.scale <- 1.5
text.scale <- 1
pt.scale <- 2
txt.line <- 3.1

# Axis text
iar.axis.text <- expression(paste('IODP 1553 IAR (ich 38-150 ', mu, 'm ', 'cm'^'-2','Myr'^'-1',')'))

# Colors
rec.col <- 'gray90' # for the EECO rectangle
temp.col <- 'darkcyan' # Oxygen c urve
nieder.iar.col <- 'chocolate' # U1553 Niederbockstruck IAR
dsdp.596.col <- adjustcolor(palette.colors()[6], alpha=0.45) # DSDP 596 IAR
means.col <- adjustcolor('dodgerblue', alpha=0.25) # background of mean tooth length SE band

# not sure where this one came from
eo.col <- adjustcolor('lightskyblue', alpha.f = 0.3)


#### Set up blank plot with rectangles

## Set up 3-panel plot
par(mfrow = c(1, 3),
    oma = c(6, 4, 8, 0.5),  #set outer margins for axis labels
    mar = c(0, 0, 0, 0))  # set plot martins to be very squished together

#plot temperature (but hide it)


hide <- plot(x = o_roll$d18O, y = o_roll$age, type = 'n', pch = 16,
             col = temp.col, ylim = yaxis.age, cex = pt.scale,
             xlim = c(max(o_roll$d18O), min(o_roll$d18O)),
             bty = 'n', axes = FALSE, xlab = '', ylab = '')

#get rect coordinates to make rectangle
usr <- par("usr")
xleft <- usr[1]
xright <- usr[2]
ybottom <- 53
ytop <- 49

#plot rectangle
par(mfg = c(1, 1))
rect(xleft, ybottom, xright, ytop, col = rec.col, border = rec.col)

par(mfg = c(1, 2))
rect(xleft, ybottom, xright, ytop, col = rec.col, border = rec.col)

par(mfg = c(1, 3))
rect(xleft, ybottom, xright, ytop, col = rec.col, border = rec.col)

par(new = T, mfrow = c(1, 3),
    oma = c(6, 4, 8, 0.5),  #set outer margins for axis labels
    mar = c(0, 0, 0, 0))  # set plot martins to be very squished together

## #plot oxygen/temperature
par(mfg = c(1, 1))
plot(x = o_roll$d18O, y = o_roll$age, type = 'o', pch = 16, col = temp.col,
     ylim = yaxis.age, cex = pt.scale,
     xlim = c(max(o_roll$d18O), min(o_roll$d18O)),
     bty = 'n', axes = FALSE, xlab = '', ylab = '')

axis(1, cex.axis = axis.scale) # D18O values
mtext (text = expression(paste(delta, ''^'18', 'O', "(‰)")), side = 1, line = txt.line, cex = text.scale)

axis(2, at=seq(42,63, by=2), cex.axis = axis.scale) #age axis
mtext(text = 'Age (Ma)', side = 2, line = 2.5, cex = text.scale)

mtext("a", side = 3, line = 5.5, at = 0.8, cex = 1.4, font = 2)

### plot IAR
plot(x = chas_dataset$nieder_IAR, y = chas_dataset$nieder_ages, type = 'o', pch = 16,
     col = nieder.iar.col, cex = pt.scale,
     ylim = yaxis.age,
     xlim = c(0, max(chas_dataset$nieder_IAR)),
     bty = 'n', axes = FALSE, xlab = '', ylab = '')
axis(1, cex.axis = axis.scale) # IAR values
mtext (text = iar.axis.text, side = 1, line = txt.line, cex = text.scale)

# add 596
par(new = TRUE)
plot(x = IAR.596$ich_accum, y = IAR.596$age, type = 'o', 
     pch = 18, col=dsdp.596.col, ylim = yaxis.age, cex = pt.scale,
     xlim = c(0, max(IAR.596$ich_accum)),
     bty = 'n', axes = FALSE, xlab = '', ylab = '')
axis(3, cex.axis = axis.scale)
mtext (text = expression(paste('DSDP 596 IAR (ich >106 ', mu, 'm ','cm'^'-2','  ','Myr'^'-1', ')')), 
       side = 3, line = txt.line, cex = text.scale)
legend("topright", legend = c(expression(bold("IODP 1553")), "DSDP 596"), 
       col = c(nieder.iar.col, dsdp.596.col), 
       pch = c(16, 18), bty = "n", 
       inset = c(0.05, 0.02), cex = 1.4, pt.cex=2)

mtext("b", side = 3, line = 5.5, at = 0, cex = 1.4, font = 2)

### plot mean length

## Run the GAM - this is on neiderbockstruck et al ages
gam_5 <- gam(length~s(nieder_ages, k = 10, bs = 'cs'), data=teeth_total)

x_vals <- seq(min(teeth_total$nieder_ages), max(teeth_total$nieder_ages), length = 100)
new_data <- data.frame(nieder_ages = x_vals)
pred <- predict(gam_5, newdata = new_data, se.fit = TRUE)
y_pred <- pred$fit
se <- pred$se.fit

ci <- 1.96 * se  # Assuming a normal distribution, 1.96 corresponds to a 95% confidence level
lower <- y_pred - ci
upper <- y_pred + ci

xlims <- c(floor(min(lower, length_stats$length_mean)), ceiling(max(upper, length_stats$length_mean)))

# plot(x_vals, y_pred, xlab = "x", ylab = "Response", type = "l")
# lines(x_vals, lower, col = "blue", lty = "dashed")
# lines(x_vals, upper, col = "blue", lty = "dashed")

plot(y_pred, x_vals, type = "l", bty = 'n', axes = FALSE, xlab = '',
     ylab = '', ylim = yaxis.age, pch = 16, xlim = xlims)
     #xlim = c(min(floor(lower)), max(ceiling(upper))))
    # xlim = c(floor(min(y_pred)), ceiling(max(y_pred))))# , xlim = age.range, pch = 16)
points(length_stats$length_mean, length_stats$nieder_ages, pch = 16)
polygon(c(lower, rev(upper)), c(x_vals, rev(x_vals)), col=means.col, border = NA)
axis(1, cex.axis = axis.scale) # length values
mtext (text = expression(paste("Mean length (", mu, "m)")), side = 1, line = 2.7, cex = text.scale)

mtext("c", side = 3, line = 5.5, at = 120, cex = 1.4, font = 2)

# close file
if(writeFile != 'off') {
  dev.off()
}

##### Clean up and reset #####
par(mar = c(5.1,4.1,4.1,2.1)) #reset default margins
par(mfrow = c(1,1)) #reset to single plotting frame
par(oma = c(0,0,0,0))  #set outer margins for axis labels


rm(ytop, ybottom, y_pred, xright, xleft, 
   usr, upper, text.scale, se, pt.scale, 
   lower, hide, eo.col, ci, axis.scale, 
   new_data, xlims, txt.line)
