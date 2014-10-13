#e4_calcsiCode.R
#Calculate the impact of invader presence on soil properties (Si1 - Si1) and create new df for this info  

##Start by creating a column to subset the data by neighbor treatment.  
data$ss1 <- rep(NA,dim(data)[1]) # create the new column to hold factors to separate rows by neighbor treatment that excludes 100% Mivi but includes 0% Mivi
data[data$type=='Empty' | data$type=='CompEmpty', 'ss1'] <- 'E' # populate that column
data[data$type=='Pavi' | data$type=='CompPavi', 'ss1'] <- 'P'
data[data$type=='Sobi' | data$type=='CompSobi', 'ss1'] <- 'S'
data[data$type=='Mivi', 'ss1'] <- 'Mv100'
#View(data)


#+ Within each bk * neighbor treatment, subtract the Si0 from each Si1. 
BK <-unique(data$bk) # list of bks to loop through
e<-0

SS1 <- unique(data$ss1)[-1] # list of ss1s to loop through, exclude Mv100
f<-0

SCOLS <- colnames(data)[11:17] # list of S columns to loop through
g<-0

MVTRT <- unique(data$mvtrt)[c(-1,-6)] # list of mvtrts to loop through as Si1 values, exclude mvtrt = 6 and 0
h<-0

# ititialize lists 
mylist<-list() # to store results 
mylist.check<-list() # to check the looping stuff

# define column names
storecols<-c('s1pid', 's0pid', 'scol', 's1', 's0', 'bk','julydate','type','comptrt','mvtrt','mivi','compabund','total','ss1')
checkcols<-c('iter','s1row','s1col','s0row','s0col')

for (e in 1:length(BK)){ # 1. loop through each bk, for the current bk...
  for (f in 1:length(SS1)){ # 2. loop through each ss1, for the current ss1....
    ss<-data[data$bk == BK[e] & data$ss1 == SS1[f], ] # 3. Subset data by bk * ss1
    for (g in 1:length(SCOLS)){ # 2. loop through each S column (nhdi, nodi, etc), for the current S column....
      for (h in 1:length(MVTRT)){ # 3. loop through each S column value associated with mvtrt 1,2,4,5, for the current value...
        
        # check the looping stuff
        iter <- paste(BK[e], SS1[f], SCOLS[g], MVTRT[h], sep='_')
        s1dim <- dim(ss[ss$mvtrt == MVTRT[h], ])
        s0dim <- dim(ss[ss$mvtrt == 0, ])
        mydata.check <- c(iter, s1dim, s0dim) # make a vector to store that data
        mylist.check [[as.character(iter)]] <- mydata.check # put that vector into the initalized list and at the same time, give it a unique name (as.character(iter))
        
        # 4. assign this info:
        
        if(s1dim[1] == 1){
          # 4.1 potid for Si1 and associated info
          s1pid<-ss[ss$mvtrt == MVTRT[h], 'potid']
          s1info<-ss[ss$mvtrt == MVTRT[h], c('bk','julydate','type','comptrt','mvtrt','mivi','compabund','total','ss1')] # 8 elements
          
          # 4.2 s1 value associated with mvtrt (1-5)
          s1 <- ss[ss$mvtrt == MVTRT[h], SCOLS[g]]
        } else{
          # 4.1 potid for Si1
          s1pid<-NA
          s1info<-rep(NA,8)
          
          # 4.2 s1 value associated with mvtrt (1-5)
          s1<-NA
        }
        
        # 4.3 potid Si0 and comma separated Si1,Si0 potids
        s0pid<-ss[ss$mvtrt==0, 'potid']
        
        # 4.4 S column name (e.g. nhdi)
        scol<-SCOLS[g] 
        
        # 4.5 s0 value associated with mvtrt = 0, calc s1 - s0
        s0 <- ss[ss$mvtrt == 0, SCOLS[g]]
        
        # 5. store the assigned info in a row, add the new row to previous rows
        mydata <- c(s1pid, s0pid, scol, s1, s0, s1info) # 14 cells, make a vector to store that data
        mylist [[as.character(iter)]] <- mydata # put that vector into the initalized list and at the same time, give it a unique name (as.character(iter))
        
      }
    }
  }
}

#Look at the dimensions of s1 and s0 through each unique iteration to make sure the loops worked
finaldata.check <- do.call(rbind, mylist.check)
colnames(finaldata.check) <- checkcols

#unique(finaldata.check[,'s1row']) # there are some iterations with 0 s1rows
messedup<-which(finaldata.check[,'s1row']==0)
#finaldata.check[messedup,] 

#ok, so I am adding an IF statement that says if s1=1, then do the normal stuff, if not, then (1) insert NA in s1 and s1-s0 cells and (2) print a warning.
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
df$s1pid<-as.numeric(df$s1pid)
df$s0pid<-as.numeric(df$s0pid)
df$bk<-as.numeric(df$bk)
df$julydate<-as.numeric(df$julydate)
#str(df)

#convert each to the appropriate form
df$s1pid<-as.factor(df$s1pid)
df$s0pid<-as.factor(df$s0pid)
df$scol<-as.factor(df$scol)
df$s1<-as.numeric(df$s1)
df$s0<-as.numeric(df$s0)
df$bk<-as.factor(df$bk)
df$julydate<-as.factor(df$julydate)
df$type<-as.factor(df$type)
df$comptrt<-as.factor(df$comptrt)
df$mvtrt<-as.factor(df$mvtrt)
df$mivi<-as.numeric(df$mivi)
df$compabund<-as.numeric(df$compabund)
df$total<-as.numeric(df$total)
df$ss1<-as.factor(df$ss1)
#str(df)

#head(df) #looks good

#+ Order df by s1pid and remove rows with s1pid == NA
df2<-df[!is.na(df$s1pid),]
#head(df2)

#+ Finally, subtract s1 - s0, populate s1s0 column with the resulting values.  Re-name the dataframe.
df2$s1s0 <- df2$s1 - df2$s0
datas <- df2
#View(datas)


