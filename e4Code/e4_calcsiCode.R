#e4_calcsiCode.R
#Calculate the impact of invader presence on soil properties (Si1 - Si1) and create new df for this info  

#str(data)

##Start by creating 3 cols, each defines the data subset for each comptrt: N, P, S.
data$ssN<-rep(NA, dim(data)[1]) # holds N filter
data$ssP<-rep(NA, dim(data)[1]) # holds P filter
data$ssS<-rep(NA, dim(data)[1]) # holds S filter
##Populate columns with a key to the rows they need
data[data$type=='Empty' | data$type=='CompEmpty', 'ssN'] <- 1
data[data$type=='Empty' | data$type=='Pavi' | data$type=='CompPavi', 'ssP'] <- 1
data[data$type=='Empty' | data$type=='Sobi' | data$type=='CompSobi', 'ssS'] <- 1
#View(data)

#+ Within each bk * neighbor treatment, calculate the deltaS relative to (1) empty pots and (2) full, non-mv pots
BK <-unique(data$bk) # list of bks to loop through
e<-0

SS <- c('ssN','ssP','ssS')
f<-0

SCOLS <- colnames(data)[11:17] # list of S columns to loop through
g<-0

MVTRT <- unique(data$mvtrt)[c(-1,-6)] # list of mvtrts to loop through as Si1 values, exclude mvtrt = 6 and 0
h<-0

# ititialize lists 
mylist<-list() # to store results 
mylist.check<-list() # to check the looping stuff

# define column names
storecols<-c('sFpid', 's0Epid', 's0Ppid',
             'scol', 
             'sF', 's0E', 's0P',
             'bk','julydate','type','comptrt','mvtrt','mivi','compabund','total', 'relmivi')
checkcols<-c('iter',
             'sFrow','sFcol',
             's0Erow','s0Ecol',
             's0Prow','s0Pcol')

# 1. loop through each bk, for the current bk...
for (e in 1:length(BK)){ 
  # 1a. subset data by the current BK
  sub1<-data[data$bk == BK[e],] 
  
  # 2. loop through each SS, for the current SS...
  for (f in 1:length(SS)){ 
    # 2a. subset sub1 by the current SS
    ssfilter<-which(sub1[,SS[f]] == 1)
    sub2<-sub1[ssfilter,]
    
    # 3. loop through each S column (nhdi, nodi, etc), for the current S column....
    for (g in 1:length(SCOLS)){ 
      
      # 4. loop through each S column value associated with mvtrt 1,2,4,5, for the current value...
      for (h in 1:length(MVTRT)){ 
        
        # 5. check the looping stuff
        iter <- paste(BK[e], SS[f], SCOLS[g], MVTRT[h], sep='_')
        
        sFrow<-sub2[sub2$mvtrt == MVTRT[h], ]
        sFdim<-dim(sFrow)
        
        s0Erow <- sub2[sub2$mvtrt == 0 & sub2$type == 'Empty', ]
        s0Edim <- dim(s0Erow)
        
        s0Prow <- sub2[sub2$mvtrt == 0 & sub2$type != 'Empty', ]
        s0Pdim <- dim(s0Prow)
        
        mydata.check <- c(iter, sFdim, s0Edim, s0Pdim) # make a vector to store that data
        mylist.check [[as.character(iter)]] <- mydata.check # put that vector into the initalized list and at the same time, give it a unique name (as.character(iter))
        
        # 6. assign this info:
        
        # 6.1 sF potid, sF info, sF value
        if(sFdim[1] == 1){
           
           sFpid <- sFrow[ ,'potid']
           sFinfo <- sFrow[, c('bk','julydate','type','comptrt','mvtrt','mivi','compabund','total','relmivi')] # 9 elements
           sF <- sFrow[ , SCOLS[g]]
         } else{
           sFpid <- NA
           sFinfo <- rep(NA,9)
           sF<-NA
         }
        
        # 6.2 s0E potid, s0E value
        s0Epid<-s0Erow[, 'potid']
        s0E <- s0Erow[, SCOLS[g]]
        
        # 6.3 s0P potid, s0P value
        if(s0Pdim[1] == 1){
          s0Ppid <- s0Prow[, 'potid']
          s0P <- s0Prow[, SCOLS[g]]
        } else{
          s0Ppid <- NA
          s0P <- NA
        }
        
        # 6.4 S column name (e.g. nhdi)
        scol<-SCOLS[g] 
        
        # 7. store the assigned info in a row, add the new row to previous rows
        mydata <- c(sFpid, s0Epid, s0Ppid, # 3 elements
                    scol, # 1 element
                    sF, s0E, s0P, # 3 elements
                    sFinfo) # 9 elements
        mylist [[as.character(iter)]] <- mydata # put that vector into the initalized list and at the same time, give it a unique name (as.character(iter))
        
      }
    }
  }
}

#Look at the dimensions of sF, s0E, and s0P through each unique iteration to make sure the loops worked
finaldata.check <- do.call(rbind, mylist.check)
colnames(finaldata.check) <- checkcols

#check sF
unique(finaldata.check[,'sFrow']) # there are some iterations with 0 rows
messedup<-which(finaldata.check[,'sFrow']==0)
finaldata.check[messedup,] # bk 5, ssN, mvtrt 5

#check s0E
unique(finaldata.check[,'s0Erow']) # looks good

#check s0P
unique(finaldata.check[,'s0Prow']) # there are some iterations with 0 rows
messedup<-which(finaldata.check[,'s0Prow']==0)
finaldata.check[messedup,] # all ssN

#ok, I added IF statements that says if there is no sF or s0P value, then do the normal stuff, if not, then insert NAs

##
#now, look at the results
finaldata <- do.call(rbind, mylist)
colnames(finaldata) <- storecols
#View(finaldata)

#+ Re-structure the resulting dataset
#str(finaldata)

ul<-unlist(finaldata, use.names=F)
df<-data.frame(matrix(ul, ncol = length(storecols), byrow=F), stringsAsFactors=F)
colnames(df)<-storecols
#str(df)

#convert potids from character strings to numbers
df$sFpid<-as.numeric(df$sFpid)
df$s0Epid<-as.numeric(df$s0Epid)
df$s0Ppid<-as.numeric(df$s0Ppid)
df$bk<-as.numeric(df$bk)
df$julydate<-as.numeric(df$julydate)
#str(df)

#convert each to the appropriate form
df$sFpid<-as.factor(df$sFpid)
df$s0Epid<-as.factor(df$s0Epid)
df$s0Ppid<-as.factor(df$s0Ppid)
df$scol<-as.factor(df$scol)
df$sF<-as.numeric(df$sF)
df$s0E<-as.numeric(df$s0E)
df$s0P<-as.numeric(df$s0P)
df$bk<-as.factor(df$bk)
df$julydate<-as.factor(df$julydate)
df$type<-as.factor(df$type)
df$comptrt<-as.factor(df$comptrt)
df$mvtrt<-as.factor(df$mvtrt)
df$mivi<-as.numeric(df$mivi)
df$compabund<-as.numeric(df$compabund)
df$total<-as.numeric(df$total)
df$relmivi<-as.numeric(df$relmivi)
#str(df)
#head(df) #looks good

#+ Remove rows with sFpid == NA
df2<-df[!is.na(df$sFpid),]
#head(df2)

#+ Calculate the deltas: delta.sE and delta.sP
# delta.sE
df2$delta.sE <- df2$sF - df2$s0E
# delta.sP
df2$delta.sP <- df2$sF - df2$s0P

#+ Update the name of this data.frame
datas <- df2
#View(datas)


