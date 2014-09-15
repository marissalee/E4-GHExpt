#E4 Lachat data analysis
setwd("~/Desktop/Lachat Data/R stuff")
e4 <- read.table("E4_soil.txt",header=T)
View(e4)

#eliminate NA's in dataset
query1=is.na(e4[,6:12])
query2=apply(query1,1,sum)
e4 <- e4[query2==0,]

#make a comptrt=N dataset
e4N <- e4[e4$comptrt=='N',]

#make a denstrt=L1,L2,L3,L4,L5 dataset
e4L <- e4[!e4$denstrt=='L0',]

-----------------------------------------------------------------------------
library(lattice)

## Working with e4N dataset (no competition)
  #plot data by bk 


xyplot(nhdi~denstrt|factor(bk), data=e4N, panel=function(x,y){
  panel.xyplot(x,y) #write your own panel fxn
  panel.lmline(x,y,lty=2) #produces lines from the linear model
}) 

xyplot(nodi~denstrt|factor(bk), data=e4N, panel=function(x,y){
  panel.xyplot(x,y) #write your own panel fxn
  panel.lmline(x,y,lty=2) #produces lines from the linear model
}) 

xyplot(totdi~denstrt|factor(bk), data=e4N, panel=function(x,y){
  panel.xyplot(x,y) #write your own panel fxn
  panel.lmline(x,y,lty=2) #produces lines from the linear model
}) 

xyplot(percnodi~denstrt|factor(bk), data=e4N, panel=function(x,y){
  panel.xyplot(x,y) #write your own panel fxn
  panel.lmline(x,y,lty=2) #produces lines from the linear model
}) 

xyplot(ammonifd~denstrt|factor(bk), data=e4N, panel=function(x,y){
  panel.xyplot(x,y) #write your own panel fxn
  panel.lmline(x,y,lty=2) #produces lines from the linear model
}) 

xyplot(nitrifd~denstrt|factor(bk), data=e4N, panel=function(x,y){
  panel.xyplot(x,y) #write your own panel fxn
  panel.lmline(x,y,lty=2) #produces lines from the linear model
})

xyplot(minzd~denstrt|factor(bk), data=e4N, panel=function(x,y){
  panel.xyplot(x,y) #write your own panel fxn
  panel.lmline(x,y,lty=2) #produces lines from the linear model
})

----------------------------------------------------------

## Working with e4L dataset (no L0)
  #plot data by bk
############  
xyplot(nhdi~comptrt|denstrt, data=e4L, panel=function(x,y){
  panel.xyplot(x,y) #write your own panel fxn
  panel.lmline(x,y,lty=2) #produces lines from the linear model
})

xyplot(nhdi~denstrt|comptrt, data=e4L, panel=function(x,y){
  panel.xyplot(x,y) #write your own panel fxn
  panel.lmline(x,y,lty=2) #produces lines from the linear model
})

#########
xyplot(nitrifd~comptrt|denstrt, data=e4L, panel=function(x,y){
  panel.xyplot(x,y) #write your own panel fxn
  panel.lmline(x,y,lty=2) #produces lines from the linear model
})

xyplot(nitrifd~denstrt|comptrt, data=e4L, panel=function(x,y){
  panel.xyplot(x,y) #write your own panel fxn
  panel.lmline(x,y,lty=2) #produces lines from the linear model
})
