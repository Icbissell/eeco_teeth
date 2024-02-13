#################################################################
#                                                               #
#       Code for calculating Extinction and Origination Rates   #
#           Capture-Mark-Recapture; Liow & Nichols 2010         #
#                                                               #
#################################################################
####################################################
#                                                  #
#     Relevant Libraries, MarkPath, and Functions  #
#                                                  #
####################################################

##### Requires setup_age_models.R to be run first #####

##### Libraries #####
library(RMark)
#library(zoo) #for rollmean
#library(stringr) #for strings

##### MarkPath and Working Directory #####
#Tell R where MARK software file is located - this will vary computer-to-computer
MarkPath='C:/Program Files (x86)/MARK/'

##### Functions #####
## Useful functions for handling RMark input

#### make.inp() - generates .inp file from counts table ####
make.inp<-function(x, filename, header=NULL) {  #make input file for RMark, x is a counts table
   mat<-as.vector(x)
   mat<-matrix(mat, nrow=length(x[1,]), ncol=length(x[,1]), byrow=T)
   rownames(mat)<-colnames(x)
   mat<-ifelse(mat>0, 1, 0)  #change to 1's and 0's
   if(is.null(header)) header=filename
   filefoo<-file(paste(filename, sep=''), 'w')
   writeLines(paste('/*', header, '*/', sep=''), filefoo)
   for(i in 1:length(mat[,1])) {
      mm<-mat[i,]
      writeLines(paste('/* ', rownames(mat)[i], ' */ ', paste(mm, sep='', collapse=''), ' 1;', sep=''), filefoo) }
   close(filefoo)
}

#### par.estimates() - calls and extracts parameter estimates from a model-list ####
par.estimates<-function(model.list, rank) {
   #model.list is the marklist to pull the model from
   #rank is model rank within model table
   #pars is vector of parameters, eg c('Phi', 'p', 'f')

   model<-model.list[[as.numeric(row.names(model.list$model.table)[rank])]]
   pars<-names(model$parameters)
   results<-model$results$real
   par.names<-rownames(results)
   par.names<-word(par.names,1,1) #extract first word of rownames

   for ( i in 1:length(pars)) {
      p <- pars[i]                                 #parameter [i] name
      par.index<-which(p == par.names)             #indices of parameter [i]
      par.output<-results[par.index,]              #call the output for parameter [i]
      assign(paste(p, '.df', sep=''), par.output)  #create object of par[i].df
   }

   pars.list<-c(paste(pars, '.df', sep=''))     # names for parameter dataframes
   pars.sep<-mget(pars.list)                    # make list from the objects in pars.list
   names(pars.sep)<-pars
   return(pars.sep)
}


##### 1. Set up MARK, build input (inp) files, etc #####

### This process runs on the "counts" objects (occurrence tables for range charts)
   # morph.counts.nieder
   # morph.counts.shipboard
## The default order is youngest to oldest; This needs to be reversed (oldest --> youngest) for CMR

morph.counts.nieder.rev <- morph.counts.nieder[length(morph.counts.nieder[,1]):1,] #Reversed ages
morph.counts.shipboard.rev <- morph.counts.shipboard[length(morph.counts.shipboard[,1]):1,] #Reversed ages

## 1.1 Pull metadata (ages, names, sample size) from counts table

# Niederbockstruck et al age model
ages.nieder <- as.numeric(rownames(morph.counts.nieder.rev))
ages.nieder.dt <- abs(diff(ages.nieder)) #diff finds difference between values. This is negative by default
ages.nieder.midpt <- rollmean(ages.nieder,2) #midpoint between ages - for plotting estimates

# Shipboard age model
ages.shipboard <- as.numeric(rownames(morph.counts.shipboard.rev))
ages.shipboard.dt <- abs(diff(ages.shipboard)) #diff finds difference between values. This is negative by default
ages.shipboard.midpt <- rollmean(ages.shipboard,2) #midpoint between ages - for plotting estimates

# Variables that are the same for both age models (names, sample size)
names <- colnames(morph.counts.nieder.rev)
samplesize<-as.vector(apply(morph.counts.nieder.rev, 1, sum)) #number of teeth considered in each time bin


## 1.2 Make and call in the inp file for RMark to run
# Niederbockstruck et al
make.inp(x=morph.counts.nieder.rev, filename='mark/nieder_teeth.inp',
         header='Niederbockstruck et al age model full dataset')

#shipboard
make.inp(x=morph.counts.shipboard.rev, filename='mark/shipboard_teeth.inp',
         header='Shipboard age model full dataset')


## Call in the inp files because RMark requires this extra step for some reason

teeth_inp.nieder<-convert.inp('mark/nieder_teeth') #don't need .inp extension
teeth_inp.shipboard<-convert.inp('mark/shipboard_teeth') #don't need .inp extension

##### 2. Pradel-Recruitment Model - Niederbockstruck et al age model #####

#### 2.1 Choose the relevant age model ####
# Pick both teeth_inp value and ages.dt value

# Niederbockstruck et al
teeth_inp <- teeth_inp.nieder
ages.dt <- ages.nieder.dt
ages.midpt <- ages.nieder.midpt

#### 2.2 Setup Pradrec model ####
dp.pradrec<-process.data(teeth_inp, model='Pradrec', time.intervals=ages.dt)
ddl.pradrec<-make.design.data(dp.pradrec)
ddl.pradrec$p$samplesize<-samplesize #add sample size variable to test for variable p (detection probability)

## 2.3 Pradel-recruitment function - tells MARK which variables to think about
Pradel.recruit<-function() {
   setwd('mark/') #need to put the mark files in here or the directories get very messy very quickly
   #Create formulas for Phi, p, and f
   Phi.time<-list(formula=~time)
   Phi.dot<-list(formula=~1)
   p.time<-list(formula=~time)
   p.dot<-list(formula=~1)
   #p.effort<-list(formula=~effort)
   p.samplesize <- list(formula =~ samplesize)
   #p.effortplussamplesize<-list(formula =~ effort + samplesize)
   f.time<-list(formula=~time)
   f.dot<-list(formula=~1)
   #create models to run by combining the above formulas, looking for objects with Phi. p. and f. at beginning
   cml<-create.model.list('Pradrec')
   #run all the models
   results<-mark.wrapper(cml,data=dp.pradrec,ddl=ddl.pradrec,output=FALSE,silent=TRUE)
   setwd('..') #go back to original working directory
   return(results)
}

#save output from LSS and sample size giant models...

# Actually run the recruitment model
Pradrec.teeth.nieder<-Pradel.recruit()   #run models
Pradrec.teeth.nieder  #displays table

#### 2.3 Save the Pradrec.teeth object as relevant age model! ####
save(Pradrec.teeth.nieder, file = 'data/Pradrec.teeth.nieder.RData')
# load('data/Pradrec.teeth.nieder.RData')

##### 3. Pradel-Recruitment Model - Shipboard age model #####

#### 3.1 Choose the relevant age model ####

## Shipboard age model ##
teeth_inp <- teeth_inp.shipboard
ages.dt <- ages.shipboard.dt
ages.midpt <- ages.shipboard.midpt

#### 3.2 Setup Pradrec model ####
dp.pradrec<-process.data(teeth_inp, model='Pradrec', time.intervals=ages.dt)
ddl.pradrec<-make.design.data(dp.pradrec)
ddl.pradrec$p$samplesize<-samplesize #add sample size variable to test for variable p (detection probability)

## 2.3 Pradel-recruitment function - tells MARK which variables to think about
Pradel.recruit<-function() {
   setwd('mark/') #need to put the mark files in here or the directories get very messy very quickly
   #Create formulas for Phi, p, and f
   Phi.time<-list(formula=~time)
   Phi.dot<-list(formula=~1)
   p.time<-list(formula=~time)
   p.dot<-list(formula=~1)
   #p.effort<-list(formula=~effort)
   p.samplesize <- list(formula =~ samplesize)
   #p.effortplussamplesize<-list(formula =~ effort + samplesize)
   f.time<-list(formula=~time)
   f.dot<-list(formula=~1)
   #create models to run by combining the above formulas, looking for objects with Phi. p. and f. at beginning
   cml<-create.model.list('Pradrec')
   #run all the models
   results<-mark.wrapper(cml,data=dp.pradrec,ddl=ddl.pradrec,output=FALSE,silent=TRUE)
   setwd('..') #go back to original working directory
   return(results)
}

#save output from LSS and sample size giant models...

# Actually run the recruitment model
Pradrec.teeth.shipboard<-Pradel.recruit()   #run models
Pradrec.teeth.shipboard  #displays table

#### 3.3 Save the Pradrec.teeth object as relevant age model! ####
save(Pradrec.teeth.shipboard, file = 'data/Pradrec.teeth.shipboard.RData')
# load('data/Pradrec.teeth.shipboard.RData')
