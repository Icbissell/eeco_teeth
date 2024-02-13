########################################
#                                      #
#     OLD CMR CODE - Not for Github    #
#                                      #
########################################


########################################
#                                      #
#              OLD CODE                #
#                                      #
########################################

# #Calculations useful for plotting
# Pradrec.con.avg<-model.average(Pradrec.con, vcv=TRUE) # vcv=TRUE includes
# Pradrec.con.avg<-Pradrec.con.avg$estimates
# ### Corrected SE values for extinction just for pr.con ##
# pr.con.ext<-Pradrec.con.avg[1:20,]
# pr.con.ext$estimate<-1-pr.con.ext$estimate
# pr.con.ext$ucl<-1-pr.con.ext$ucl
# pr.con.ext$lcl<-1-pr.con.ext$lcl
# pr.con.orig<-Pradrec.con.avg[42:61,]
#
# pr.con.orig[pr.con.orig == Inf] <- 0
#
# #
# # # #Liberally trimmed dataset
# # Pradrec.lib<-Pradel.recruit()   #run models
# # Pradrec.lib  #display table
#
#
# ##### b) Pradel-Lambda model #####
# dp.pradlam<-process.data(rangesrev, model='Pradlambda', time.intervals=dt.rev)
# ddl.pradlam<-make.design.data(dp.pradlam)
# ddl.pradlam$p$effort<-effort
#
# Pradel.lambda<-function() {
#    #Create formulas for Phi, p, and f
#    Phi.time<-list(formula=~time)
#    Phi.dot<-list(formula=~1)
#    p.time<-list(formula=~time)
#    p.dot<-list(formula=~1)
#    p.effort<-list(formula=~effort)
#    Lambda.time<-list(formula=~time)
#    Lambda.dot<-list(formula=~1)
#    #create models to run by combining the above formulas, looking for objects with Phi. p. and f. at beginning
#    cml<-create.model.list('Pradlambda')
#    #run all the models
#    results<-mark.wrapper(cml,data=dp.pradlam,ddl=ddl.pradlam,output=FALSE,silent=TRUE)
#    return(results)
# }
#
# # #Original dataset
# # Pradlam.original<-Pradel.lambda()   #run models
# # Pradlam.original  #displays table
#
# # #Conservatively trimmed dataset
# # Pradlam.con<-Pradel.lambda()    #run models
# # Pradlam.con  #displays table
# #
# # #Liberally trimmed dataset
# Pradlam.lib<-Pradel.lambda()   #run models
# Pradlam.lib  #display table
#
#
# ##### c) POPAN Model #####
# dp.popan<-process.data(rangesrev, model='POPAN', time.intervals=dt.rev)
# ddl.popan<-make.design.data(dp.popan)
# ddl.popan$p$effort<-effort
#
# popan.fn<-function() {
#    #Create formulas for Phi, p, and f
#    Phi.time<-list(formula=~time)
#    Phi.dot<-list(formula=~1)
#    p.time<-list(formula=~time)
#    p.dot<-list(formula=~1)
#    p.effort<-list(formula=~effort)
#    pent.time<-list(formula=~time)
#    pent.dot<-list(formula=~1)
#    N.dot<-list(formula=~1)
#    #create models to run by combining the above formulas, looking for objects with Phi. p. and f. at beginning
#    cml<-create.model.list('POPAN')
#    #run all the models
#    results<-mark.wrapper(cml,data=dp.popan,ddl=ddl.popan,output=FALSE,silent=TRUE)
#    return(results)
# }
# # #Original dataset
# # popan.original<-popan.fn()   #run models
# # popan.original  #displays table
#
# # #Conservatively trimmed dataset
# # popan.con<-popan.fn()    #run models
# # popan.con  #displays table
# #
# # #Liberally trimmed dataset
# popan.lib<-popan.fn()   #run models
# popan.lib  #display table
#
#
# ##### Step 3: Calculate Model Averages #####
# Pradrec.lib.avg<-model.average(Pradrec.lib)
# Pradrec.con.avg<-model.average(Pradrec.con)
# Pradrec.orig.avg<-model.average(Pradrec.original)
#
# Pradlam.lib.avg<-model.average(Pradlam.lib)
# Pradlam.con.avg<-model.average(Pradlam.con)
# Pradlam.orig.avg<-model.average(Pradlam.original)
#
# Popan.lib.avg<-model.average(popan.lib)
# Popan.con.avg<-model.average(popan.con)
# Popan.orig.avg<-model.average(popan.original)
#
# ##### Step 4: Extract parameters for each set of models: #####
#
# #Pradel Recruitment: 61 parameters:
# # Phi: 1-20;
# # p: 21-41; (includes all AgeID's
# # f: 42-61;
# pr.orig.ext<-Pradrec.orig.avg[1:20,]
# pr.orig.ext[,2] <- 1-pr.orig.exit[,2]  #1-survival is extinction
# pr.orig.orig<-Pradrec.orig.avg[42:61,]
#
# ### Corrected SE values for extinction just for pr.con ##
# pr.con.ext<-Pradrec.con.avg[1:20,]
# pr.con.ext[,2]<-1-pr.con.ext[,2]
# pr.con.orig<-Pradrec.con.avg[42:61,]
#
# pr.lib.ext<-1-Pradrec.lib.avg[1:20,]
# pr.lib.orig<-Pradrec.lib.avg[42:61,]
#
# # Pradel Lambda: 61 parameters
# # Phi: 1-20;
# # p: 21-41; (includes all AgeID's
# # Lambda: 42-61;
# pl.orig.ext<-1-Pradlam.orig.avg[1:20,]
# pl.orig.orig<-Pradlam.orig.avg[42:61,]-Pradlam.orig.avg[1:20,] #lambda - phi
# pl.con.ext<-1-Pradlam.con.avg[1:20,]
# pl.con.orig<-Pradlam.con.avg[42:61,]-Pradlam.con.avg[1:20,]
# pl.lib.ext<-1-Pradlam.lib.avg[1:20,]
# pl.lib.orig<-Pradlam.lib.avg[42:61,]-Pradlam.lib.avg[1:20,]
#
# # POPAN: 62 parameters
# # Phi: 1-20;
# # p: 21-41; (includes all AgeID's
# # pent: 42-61;
# # N #metapopulation size
# pop.orig.ext<-1-Popan.orig.avg[1:20,]
# pop.orig.orig<-Popan.orig.avg[42:61,]
# pop.con.ext<-1-Popan.con.avg[1:20,]
# pop.con.orig<-Popan.con.avg[42:61,]
# pop.lib.ext<-1-Popan.lib.avg[1:20,]
# pop.lib.orig<-Popan.lib.avg[42:61,]
#
#
# ##### Plot this #####
#
# #this function isn't working - ack!
# error.bars <- function(x, y, upper, lower=upper, length.arr=0.1, ...){
#    if(length(x) != length(y) | length(y) != length(lower) | length(lower) != length (upper))
#       stop("vectors must be the same length")
#    arrows(x, y+upper, x, y-lower, angle=90, code=3, length=length.arr, ...)
# }
#
# AgeID.int <- c()
# for(i in 1:length(AgeID.unique)-1) {
#    AgeID.int[i] <- mean(c(AgeID.unique[i], AgeID.unique[i+1]))
# }
# AgeID.int<-rev(AgeID.int) #go from older to younger, as in capture history calculations
#
# ##### Cleaned Figure #####
#
# # select appropriate model output
# Pradrec.con<-Pradrec.con.p.effort
# # calculate model averages for plotting
# Pradrec.con.avg<-model.average(Pradrec.con, vcv=TRUE)
# # extract modeled origination and extinction parameters
# pr.con.ext<-Pradrec.con.avg[1:20,]
# pr.con.ext[,2]<-1-pr.con.ext[,2]
# pr.con.orig<-Pradrec.con.avg[42:61,]
#
# ### NOTE FIGURE SAVE LOCATION ASSUMES THE WD IS THE MARK WD. THIS IS NOT ALWAYS THE CASE ##
# pdf('../figures/cmr_orig_ext_accum_errbar_peffort.pdf', width=8, height=5, useDingbats = FALSE)
# # Set up plot parameters
# par(mar=c(5,4,4,4))
# xlims <- c(73, 43)
# ext.color<-'firebrick'
# orig.color<-'blue3'
# accum.color<-'gray30'
#
# # ext.color<-'red'
# # orig.color<-'green'
# # accum.color<-'gray30'
#
#
# #make blank plot and annotations
# plot(AgeID.int, pr.con.orig$estimate, type='n', xlim=xlims, ylim=c(-0.03,0.36),
#      main='CMR Evolutionary Rate Estimates',
#      xlab='', ylab='')
# rect(64.3, -0.05, 63, 0.39, col='gray80', border=NA)
# rect(61.8, -0.05, 55.8, 0.39, col='gray90', border=NA)
# rect(59.2, -0.05, 58, 0.39, col='gray79', border=NA)
# mtext(text='Per-morphotype rate estimate', side=2, line=2.5) #x-axis label
# mtext(text='Age (Ma)', side=1, line=2.5)
# text(63.5, 0.365, 'Pulse 1')
# text(58.5, 0.365, 'Pulse 2')
# abline(v=66.5, col='black', lwd=1)
# abline(h=0, col='gray60', lty=2, lwd=0.8)
# box()
#
# ## add extinction ##
# errbar(AgeID.int, pr.con.ext$estimate, type='o', add=TRUE,
#        yplus=pr.con.ext$estimate+pr.con.ext$se, yminus=pr.con.ext$estimate-pr.con.ext$se,
#        errbar.col=ext.color, col=ext.color, pch=16, lwd=1)
#
# #with ucl/lcl
# errbar(AgeID.int, pr.con.ext$estimate, type='o', add=TRUE,
#        yplus=pr.con.ext$lcl, yminus=pr.con.ext$ucl,
#        errbar.col=ext.color, col=ext.color, pch=17, lwd=1)
#
# #with se/lcl
# errbar(AgeID.int, pr.con.ext$estimate, type='o', add=TRUE,
#        yplus=pr.con.ext$estimate+pr.con.ext$se, yminus=pr.con.ext$ucl,
#        errbar.col=ext.color, col=ext.color, pch=17, lwd=1)
#
# ## add origination ##
# errbar(AgeID.int, pr.con.orig$estimate, type='o', add=TRUE,
#        yplus=pr.con.orig$estimate+pr.con.orig$se, yminus=pr.con.orig$estimate-pr.con.orig$se,
#        errbar.col=orig.color, col=orig.color, pch=17, lwd=1)
#
# #with ucl/lcl
# errbar(AgeID.int, pr.con.orig$estimate, type='o', add=TRUE,
#        yplus=pr.con.orig$ucl, yminus=pr.con.orig$lcl,
#        errbar.col=orig.color, col=orig.color, pch=17, lwd=1)
#
# #with se/lcl
# errbar(AgeID.int, pr.con.orig$estimate, type='o', add=TRUE,
#        yplus=pr.con.orig$estimate+pr.con.orig$se, yminus=pr.con.orig$lcl,
#        errbar.col=orig.color, col=orig.color, pch=17, lwd=1)
#
#
#
#
# #legend
# legend('topright', legend=c('Origination', 'Extinction', 'Accumulation'),
#        col=c(orig.color, ext.color, accum.color),
#        pch=c(16, 16, -1), lty=c(1,1,2), lwd=c(1,1,1.2), cex=0.9)
#
# # add accumulation curve
# par(new=T)
# # Accum plot
# plot(accum.596$age, filter(accum.596$teeth.mar, ma3), xlim=xlims, ylim=c((-250/12), 250), #y-axis scaled to match 0-value from above
#      lty=2, type='l', col=accum.color, lwd=1.5,
#      axes=F, xlab='', ylab='')
# axis(4)
# mtext(text=parse(text='Teeth %.% cm^-2 %.% myr^-1'), side=4, line=2.5)
# dev.off()
#
#
# ##### Plot code used for manuscript figure #####
# ### P.effort only! ###
#
# # select appropriate model output
# Pradrec.con<-Pradrec.con.p.effort
# # calculate model averages for plotting and pull parameter estimates
# Pradrec.con.avg<-model.average(Pradrec.con, vcv=TRUE)
# Pradrec.con.avg<-Pradrec.con.avg$estimates
# ### Corrected SE values for extinction just for pr.con ##
# pr.con.ext<-Pradrec.con.avg[1:20,]
# pr.con.ext$estimate<-1-pr.con.ext$estimate
# pr.con.ext$ucl<-1-pr.con.ext$ucl
# pr.con.ext$lcl<-1-pr.con.ext$lcl
# pr.con.orig<-Pradrec.con.avg[42:61,]
#
# pr.con.orig[pr.con.orig == Inf] <- 0
#
# #
#
# ### NOTE FIGURE SAVE LOCATION ASSUMES THE WD IS THE MARK WD. THIS IS NOT ALWAYS THE CASE ##
# pdf('../figures/cmr_orig_ext_accum_errbar_peffort.pdf', width=8, height=5, useDingbats = FALSE)
# # Set up plot parameters
# par(mar=c(5,4,4,4))
# xlims <- c(73, 43)
# ext.color<-'firebrick'
# orig.color<-'blue3'
# accum.color<-'gray30'
#
# # ext.color<-'red'
# # orig.color<-'green'
# # accum.color<-'gray30'
#
#
# #make blank plot and annotations
# plot(AgeID.int, pr.con.orig$estimate, type='n', xlim=xlims, ylim=c(-0.03,0.36),
#      main='CMR Evolutionary Rate Estimates',
#      xlab='', ylab='')
# rect(64.3, -0.05, 63, 0.39, col='gray80', border=NA)
# rect(60.8, -0.05, 55.8, 0.39, col='gray90', border=NA)
# rect(59.2, -0.05, 58, 0.39, col='gray79', border=NA)
# mtext(text='Per-morphotype rate estimate', side=2, line=2.5) #x-axis label
# mtext(text='Age (Ma)', side=1, line=2.5)
# text(63.5, 0.365, 'Pulse 1')
# text(58.5, 0.365, 'Pulse 2')
# abline(v=66.5, col='black', lwd=1)
# abline(h=0, col='gray60', lty=2, lwd=0.8)
# box()
#
# # # add extinction
# # errbar(AgeID.int, pr.con.ext$estimate, type='o', add=TRUE,
# #    yplus=pr.con.ext$estimate+pr.con.ext$se, yminus=pr.con.ext$estimate-pr.con.ext$se,
# #    errbar.col=ext.color, col=ext.color, pch=16, lwd=1)
# #
# #with ucl/lcl
# errbar(AgeID.int, pr.con.ext$estimate, type='o', add=TRUE,
#        yplus=pr.con.ext$lcl, yminus=pr.con.ext$ucl,
#        errbar.col=ext.color, col=ext.color, pch=17, lwd=1)
#
# # #with se/lcl
# # errbar(AgeID.int, pr.con.ext$estimate, type='o', add=TRUE,
# #    yplus=pr.con.ext$estimate+pr.con.ext$se, yminus=pr.con.ext$ucl,
# #    errbar.col=ext.color, col=ext.color, pch=16, lwd=1)
#
# # # add origination
# # errbar(AgeID.int, pr.con.orig$estimate, type='o', add=TRUE,
# #    yplus=pr.con.orig$estimate+pr.con.orig$se, yminus=pr.con.orig$estimate-pr.con.orig$se,
# #    errbar.col=orig.color, col=orig.color, pch=17, lwd=1)
# #
# #with ucl/lcl
# errbar(AgeID.int, pr.con.orig$estimate, type='o', add=TRUE,
#        yplus=pr.con.orig$ucl, yminus=pr.con.orig$lcl,
#        errbar.col=orig.color, col=orig.color, pch=17, lwd=1)
#
# # #with se/lcl
# # errbar(AgeID.int, pr.con.orig$estimate, type='o', add=TRUE,
# #    yplus=pr.con.orig$estimate+pr.con.orig$se, yminus=pr.con.orig$lcl,
# #    errbar.col=orig.color, col=orig.color, pch=17, lwd=1)
#
#
# #legend
# legend('topright', legend=c('Origination', 'Extinction', 'Accumulation'),
#        col=c(orig.color, ext.color, accum.color),
#        pch=c(17, 16, -1), lty=c(1,1,2), lwd=c(1,1,1.2), cex=0.9)
#
# # add accumulation curve
# par(new=T)
# # Accum plot
# plot(accum.596$age, filter(accum.596$teeth.mar, ma3), xlim=xlims, ylim=c((-250/12), 250), #y-axis scaled to match 0-value from above
#      lty=2, type='l', col=accum.color, lwd=1.5,
#      axes=F, xlab='', ylab='')
# axis(4)
# mtext(text=parse(text='Teeth %.% cm^-2 %.% myr^-1'), side=4, line=2.5)
# dev.off()
#
#
