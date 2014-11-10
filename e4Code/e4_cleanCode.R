#e4_cleanCode.R
#Clean raw dataset

##Import data and check it out 
data<-read.table('e4Data/e4_potData.txt',header=T, sep="\t", stringsAsFactors=F)
datadic<-read.table('e4Data/e4_potData_dictionary.txt',header=T, sep="\t", stringsAsFactors=F)

#Check out dataframe; Note that...
#+ 'potid' and 'bk' columns need to be re-classified as factors
#+ 'mivi', 'pavi', 'sobi' have NA values ... If these are pots without that species planted, then these values should be zero. If these are missing values, then the rows need to be excluded
#+ there are notes that may mean some rows need to be excluded

#head(data)
#str(data)

#head(datadic)
#str(datadic)


##Remove rows with NAs

#+ Change 'NAs' to 0s in 'mivi', 'pavi', and 'sobi' columns according to the treatment type.  In other words, NA should be changed to 0 if the species' biomass is missing because it wasn't planted there in the first place due to its treatment assignment.
#Empty trt
#head(data[data$type=='Empty',])
data[data$type=='Empty',c('mivi','pavi','sobi')] <- 0

#Mivi trt
#head(data[data$type=='Mivi',])
data[data$type=='Mivi',c('pavi','sobi')] <- 0

#Pavi trt
#head(data[data$type=='Pavi',])
data[data$type=='Pavi',c('mivi','sobi')] <- 0

#Sobi trt
#head(data[data$type=='Sobi',])
data[data$type=='Sobi',c('mivi','pavi')] <- 0

#CompEmpty trt
#head(data[data$type=='CompEmpty',])
data[data$type=='CompEmpty',c('pavi','sobi')] <- 0

#CompPavi trt
#head(data[data$type=='CompPavi',])
data[data$type=='CompPavi',c('sobi')] <- 0

#CompSobi trt
#head(data[data$type=='CompSobi',])
data[data$type=='CompSobi',c('pavi')] <- 0


#+ Remove rows with an 'NA' in any column (other than 'notes')
firstdatacol<-which(colnames(data)=='mivi')
lastdatacol<-which(colnames(data)=='notes') - 1
#sum(is.na(data[,c(firstdatacol:lastdatacol)])) # the number of NAs that need to be removed

removerow<-which(is.na(data$mivi)) # which row is it?
data<-data[-removerow,] # remove the row

#dim(data) # new dataframe dimensions

#+ Remove the row with the note about the nh and no outliers
#data[!is.na(data$notes),] # figure out the potid for that row

removerow<-which(data$potid=='145') # which row is it?
data<-data[-removerow,] # remove the row

#dim(data) # new dataframe dimensions



##Calculate aggregate biomass columns

#+ Create a total aboveground biomass column by adding Microstegium biomass, Panicum biomass, Sorghum biomass per observation (pot).
#+ Create 'compabund' (aka Neighbor abundance) column by adding Panicum biomass and Sorghum biomass per observation (pot).

data$total <- data$mivi + data$pavi + data$sobi
data$compabund <- data$pavi + data$sobi

#head(data) #check out the dataframe



##Add relmivi column

## Calculate the relative Mv abundance, add it as a column  
#str(data)
data$relmivi <- (data$mivi / data$total) * 100

# Force pots with mivi = 0 and total = 0 to have relmivi = 0 too
tmp<-data[data$mivi == 0,]
#View(tmp)
len<-length(data[data$mivi == 0,'relmivi'])
data[data$mivi == 0,'relmivi'] <- rep(0,30)




