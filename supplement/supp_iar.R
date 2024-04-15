
writeFile <- 'pdf'
# writeFile <- 'jpg'
# writeFile <- 'off'

fig.dims <- c(7, 11) #Set Figure-dimensions

if(writeFile == 'pdf') {
  pdf('supplement_plot/iar_comp.pdf', height = fig.dims[1], width = fig.dims[2], useDingbats = FALSE)
}

if(writeFile == 'jpg') {
  jpeg('supplement_plot/iar_comp.pdf', height = fig.dims[1], width = fig.dims[2], units = 'in', res = 300)
}

iar.axis.text <- expression(paste('IODP 1553 IAR (ich 38-150 ', mu, 'm ', 'cm'^'-2','Myr'^'-1',')'))

### plot IAR
plot(chas_dataset$nieder_ages, chas_dataset$nieder_IAR, 
     type = 'o', pch = 16, 
     xlim = rev(c(min(chas_dataset$nieder_ages), max(chas_dataset$nieder_ages))), 
     ylab = "", xlab = "Age (Ma)", 
     col = palette.colors()[7], 
     ylim = c(0, 46000), lwd = 2)

mtext(iar.axis.text, side = 2, line = 2)

lines(chas_dataset$shipboard_ages, chas_dataset$shipboard_IAR, 
      type = 'o', col = palette.colors()[3], lwd = 2)

legend ('topleft', 
        legend=c('Niederbockstruck et al. model', 'Shipboard model'), 
        col=c(palette.colors()[7], palette.colors()[3]), lty=c(1,1),
        lwd = c(2,2), 
        bty = 'n')

# close file
if(writeFile != 'off') {
  dev.off()
}