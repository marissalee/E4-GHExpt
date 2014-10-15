#e4 figures - regressions
# 2/18/13
setwd("~/Desktop/E4_R")
data<-read.table('e4_trimmedData.txt',header=T)
data$bk<-as.factor(data$bk)
data$Mvtrt<-as.factor(data$Mvtrt)

##################################
#make datasets
# dataset1: No competition treatment, all Mvtrt levels
Empty<-data[data$type=='Empty',]
Mivi<-data[data$type=='Mivi',]
CompEmpty<-data[data$type=='CompEmpty',]
dataset1<-rbind(Empty, CompEmpty, Mivi)
colnames(dataset1)
write.table(dataset1,file='dataset1.txt', sep='\t')
# dataset2: Competition treatment, all Mvtrt levels
CompPavi<-data[data$type=='CompPavi',]
Pavi<-data[data$type=='Pavi',]
CompSobi<-data[data$type=='CompSobi',]
Sobi<-data[data$type=='Sobi',]
dataset2<-rbind(Mivi, CompPavi, Pavi, CompSobi, Sobi)
write.table(dataset2,file='dataset2.txt', sep='\t')
# dataset1b: No competition treatment, middle Mvtrt levels
dataset1b<-rbind(CompEmpty)
write.table(dataset1b,file='dataset1b.txt', sep='\t')
# dataset2b: Competition treatment, middle Mvtrt levels
dataset2b<-rbind(CompPavi, CompSobi)
write.table(dataset2b,file='dataset2b.txt', sep='\t')
# dataset3: Monocultures
dataset3<-rbind(Empty,Sobi,Pavi,Mivi)
write.table(dataset3,file='dataset3.txt', sep='\t')

##################################
##################################
def.par <- par(no.readonly = TRUE) # save default, for resetting...
ylabs<-c('Soil moisture\n (%)','Nitrate\n (ugN/G)','Ammonium:\nNitrate ratio','Nitrification\nrate (ugN/G*d)','Mineralization\nrate (ugN/G*d)')
ylim1<-c(50,100)
ylim2<-c(0,120)
ylim3<-c(0,1)
ylim4<-c(-10,10)
ylim5<-c(-10,10)
ylims<-rbind(ylim1,ylim2,ylim3,ylim4,ylim5)

nf <- layout(matrix(c(1,2,3,4,5),
                    5,1), 
             widths=c(1), 
             heights=c(1,1,1,1,1.3), 
             respect=F)
layout.show(nf)

##################################
#make panels (dataset1 and dataset2)

#################################################################
#panel1: MVabund_alone
x<-dataset1$Mvabund
y1<-dataset1$soilmoi
y2<-dataset1$nodi
y3<-dataset1$nh.no
y4<-dataset1$nitrifd
y5<-dataset1$minzd
ys<-cbind(y1,y2,y3,y4,y5)
AT<-seq(0,50,10)
par(mar=c(0.1,5,0.5,1)) # mar=c(bottom, left, top, right)
for (i in (1:4)){
  plot(x=x,y=ys[,i], ylab = ylabs[i], xlab='', xaxt = "n",type= 'n', ylim=ylims[i,])
  axis(1, at=AT, tick=T, labels=F, tcl=-.2)
  points(x=x, y=ys[,i], pch=19)
  lines(stats::lowess(data.frame(x=x,y=ys[,i])))
}
par(mar=c(4,5,0.5,1))
plot(x=x,y=ys[,5], ylab = ylabs[5], xlab='M.v. abundance (g)\nNo competition; Empty and Mono Pots',type= 'n', ylim=ylims[5,])
points(x=x, y=ys[,5], pch=19)
lines(stats::lowess(data.frame(x=x,y=ys[,5])))

#################################################################
#panel2: MVabund_incomp with dataset2
P<-which(dataset2$comptrt=='P')
S<-which(dataset2$comptrt=='S')
N<-which(dataset2$comptrt=='N')
x<-dataset2$Mvabund
y1<-dataset2$soilmoi
y2<-dataset2$nodi
y3<-dataset2$nh.no
y4<-dataset2$nitrifd
y5<-dataset2$minzd
ys<-cbind(y1,y2,y3,y4,y5)
AT<-seq(0,50,10)
par(mar=c(0.1,5,0.5,1)) # mar=c(bottom, left, top, right)
for (i in (1:4)){
  plot(x=x,y=ys[,i], ylab = ylabs[i], xlab='', xaxt = "n",type= 'n',ylim=ylims[i,])
  axis(1, at=AT, tick=T, labels=F, tcl=-.2)
  points(x=x[P], y=ys[P,i], pch=19, col=1)
  points(x=x[S], y=ys[S,i], pch=1, col=2)
  points(x=x[N], y=ys[N,i], pch=2)
  lines(stats::lowess(data.frame(x=x[c(P,N)],y=ys[c(P,N),i])), lty=1, col=1)
  lines(stats::lowess(data.frame(x=x[c(S,N)],y=ys[c(S,N),i])), lty=2, col=2)
}
par(mar=c(4,5,0.5,1))
plot(x=x,y=ys[,5], ylab = ylabs[5], xlab='M.v. abundance (g)\nCompetition; Mono Pots',type= 'n',ylim=ylims[5,])
points(x=x[P], y=ys[P,5], pch=19, col=1)
points(x=x[S], y=ys[S,5], pch=1, col=2)
points(x=x[N], y=ys[N,5], pch=2)
lines(stats::lowess(data.frame(x=x[c(P,N)],y=ys[c(P,N),5])), lty=1, col=1)
lines(stats::lowess(data.frame(x=x[c(S,N)],y=ys[c(S,N),5])), lty=2, col=2)

#################################################################
#panel3: MVrelabund_incomp with dataset2
x<-dataset2$MVrelabund
AT<-seq(0,100,20)
par(mar=c(0.1,5,0.5,1)) # mar=c(bottom, left, top, right)
for (i in (1:4)){
  plot(x=x,y=ys[,i], ylab = ylabs[i], xlab='', xaxt = "n",type= 'n',ylim=ylims[i,])
  axis(1, at=AT, tick=T, labels=F,tcl=-.2)
  points(x=x[P], y=ys[P,i], pch=19, col=1)
  points(x=x[S], y=ys[S,i], pch=1, col=2)
  points(x=x[N], y=ys[N,i], pch=2)
  lines(stats::lowess(data.frame(x=x[c(P,N)],y=ys[c(P,N),i])), lty=1, col=1)
  lines(stats::lowess(data.frame(x=x[c(S,N)],y=ys[c(S,N),i])), lty=2, col=2)
}
par(mar=c(4,5,0.5,1))
plot(x=x,y=ys[,5], ylab = ylabs[5], xlab='M.v. relative abundance (%)\nCompetition; Mono Pots',type= 'n',ylim=ylims[5,])
points(x=x[P], y=ys[P,5], pch=19, col=1)
points(x=x[S], y=ys[S,5], pch=1, col=2)
points(x=x[N], y=ys[N,5], pch=2)
lines(stats::lowess(data.frame(x=x[c(P,N)],y=ys[c(P,N),5])), lty=1, col=1)
lines(stats::lowess(data.frame(x=x[c(S,N)],y=ys[c(S,N),5])), lty=2, col=2)

##################################
#make panels (dataset1b and dataset2b)

#################################################################
#panel1: MVabund_alone
x<-dataset1b$Mvabund
y1<-dataset1b$soilmoi
y2<-dataset1b$nodi
y3<-dataset1b$nh.no
y4<-dataset1b$nitrifd
y5<-dataset1b$minzd
ys<-cbind(y1,y2,y3,y4,y5)
AT<-seq(0,50,10)
par(mar=c(0.1,5,0.5,1)) # mar=c(bottom, left, top, right)
for (i in (1:4)){
  plot(x=x,y=ys[,i], ylab = ylabs[i], xlab='', xaxt = "n",type= 'n',ylim=ylims[i,])
  axis(1, at=AT, tick=T, labels=F)
  points(x=x, y=ys[,i], pch=19)
  lines(stats::lowess(data.frame(x=x,y=ys[,i])))
}
par(mar=c(4,5,0.5,1))
plot(x=x,y=ys[,5], ylab = ylabs[5], xlab='M.v. abundance (g)\nNo competition; No Empty or Mono Pots',type= 'n',ylim=ylims[5,])
points(x=x, y=ys[,5], pch=19)
lines(stats::lowess(data.frame(x=x,y=ys[,5])))

#################################################################
#panel2: MVabund_incomp with dataset2b
P<-which(dataset2b$comptrt=='P')
S<-which(dataset2b$comptrt=='S')
x<-dataset2b$Mvabund
y1<-dataset2b$soilmoi
y2<-dataset2b$nodi
y3<-dataset2b$nh.no
y4<-dataset2b$nitrifd
y5<-dataset2b$minzd
ys<-cbind(y1,y2,y3,y4,y5)
AT<-seq(0,50,10)
par(mar=c(0.1,5,0.5,1)) # mar=c(bottom, left, top, right)
for (i in (1:4)){
  plot(x=x,y=ys[,i], ylab = ylabs[i], xlab='', xaxt = "n",type= 'n',ylim=ylims[i,])
  axis(1, at=AT, tick=T, labels=F)
  points(x=x[P], y=ys[P,i], pch=19)
  points(x=x[S], y=ys[S,i], pch=1, col=2)
  lines(stats::lowess(data.frame(x=x[P],y=ys[P,i])), lty=1)
  lines(stats::lowess(data.frame(x=x[S],y=ys[S,i])), lty=2, col=2)
}
par(mar=c(4,5,0.5,1))
plot(x=x,y=ys[,5], ylab = ylabs[5], xlab='M.v. abundance (g)\nCompetition; No Mono Pots',type= 'n',ylim=ylims[5,])
points(x=x[P], y=ys[P,5], pch=19)
points(x=x[S], y=ys[S,5], pch=1, col=2)
lines(stats::lowess(data.frame(x=x[P],y=ys[P,i])), lty=1)
lines(stats::lowess(data.frame(x=x[S],y=ys[S,i])), lty=2, col=2)

#################################################################
#panel3: MVrelabund_incomp with dataset2
x<-dataset2b$MVrelabund
AT<-seq(0,100,20)
par(mar=c(0.1,5,0.5,1)) # mar=c(bottom, left, top, right)
for (i in (1:4)){
  plot(x=x,y=ys[,i], ylab = ylabs[i], xlab='', xaxt = "n",type= 'n',ylim=ylims[i,])
  axis(1, at=AT, tick=T, labels=F)
  points(x=x[P], y=ys[P,i], pch=19)
  points(x=x[S], y=ys[S,i], pch=1, col=2)
  lines(stats::lowess(data.frame(x=x[P],y=ys[P,i])), lty=1)
  lines(stats::lowess(data.frame(x=x[S],y=ys[S,i])), lty=2, col=2)
}
par(mar=c(4,5,0.5,1))
plot(x=x,y=ys[,5], ylab = ylabs[5], xlab='M.v. relative abundance (%)\nCompetition; No Mono Pots',type= 'n',ylim=ylims[5,])
points(x=x[P], y=ys[P,5], pch=19)
points(x=x[S], y=ys[S,5], pch=1, col=2)
lines(stats::lowess(data.frame(x=x[P],y=ys[P,i])), lty=1)
lines(stats::lowess(data.frame(x=x[S],y=ys[S,i])), lty=2, col=2)

#################################################################
#panel2: MVabund_incomp with dataset2b - P and S are separate panels
x<-dataset2b$Mvabund
AT<-seq(0,50,10)

#pavi
par(mar=c(0.1,5,0.5,1)) # mar=c(bottom, left, top, right)
for (i in (1:4)){
  plot(x=x[P],y=ys[P,i], ylab = ylabs[i], xlab='', xaxt = "n",type= 'n',ylim=ylims[i,])
  axis(1, at=AT, tick=T, labels=F)
  points(x=x[P], y=ys[P,i], pch=19)
  lines(stats::lowess(data.frame(x=x[P],y=ys[P,i])), lty=1)
}
par(mar=c(4,5,0.5,1))
plot(x=x,y=ys[,5], ylab = ylabs[5], xlab='M.v. abundance (g)\nCompetition w/ Pavi; No Mono Pots',type= 'n',ylim=ylims[5,])
points(x=x[P], y=ys[P,5], pch=19)
lines(stats::lowess(data.frame(x=x[P],y=ys[P,i])), lty=1)

#sobi
par(mar=c(0.1,5,0.5,1)) # mar=c(bottom, left, top, right)
for (i in (1:4)){
  plot(x=x[S],y=ys[S,i], ylab = ylabs[i], xlab='', xaxt = "n",type= 'n',ylim=ylims[i,])
  axis(1, at=AT, tick=T, labels=F)
  points(x=x[S], y=ys[S,i], pch=1, col=2)
  lines(stats::lowess(data.frame(x=x[S],y=ys[S,i])), lty=2, col=2)
}
par(mar=c(4,5,0.5,1))
plot(x=x[S],y=ys[S,5], ylab = ylabs[5], xlab='M.v. abundance (g)\nCompetition w/ Sobi; No Mono Pots',type= 'n',ylim=ylims[5,])
points(x=x[S], y=ys[S,5], pch=1, col=2)
lines(stats::lowess(data.frame(x=x[S],y=ys[S,i])), lty=2, col=2)

#################################################################
#panel3: MVrelabund_incomp with dataset2
x<-dataset2b$MVrelabund
AT<-seq(0,100,20)

#pavi
par(mar=c(0.1,5,0.5,1)) # mar=c(bottom, left, top, right)
for (i in (1:4)){
  plot(x=x[P],y=ys[P,i], ylab = ylabs[i], xlab='', xaxt = "n",type= 'n',ylim=ylims[i,])
  axis(1, at=AT, tick=T, labels=F)
  points(x=x[P], y=ys[P,i], pch=19)
  lines(stats::lowess(data.frame(x=x[P],y=ys[P,i])), lty=1)
}
par(mar=c(4,5,0.5,1))
plot(x=x[P],y=ys[P,5], ylab = ylabs[5], xlab='M.v. relative abundance (%)\nCompetition w/ Pavi; No Mono Pots',type= 'n',ylim=ylims[5,])
points(x=x[P], y=ys[P,5], pch=19)
lines(stats::lowess(data.frame(x=x[P],y=ys[P,i])), lty=1)

#sobi
par(mar=c(0.1,5,0.5,1)) # mar=c(bottom, left, top, right)
for (i in (1:4)){
  plot(x=x[S],y=ys[S,i], ylab = ylabs[i], xlab='', xaxt = "n",type= 'n',ylim=ylims[i,])
  axis(1, at=AT, tick=T, labels=F)
  points(x=x[S], y=ys[S,i], pch=1, col=2)
  lines(stats::lowess(data.frame(x=x[S],y=ys[S,i])), lty=2, col=2)
}
par(mar=c(4,5,0.5,1))
plot(x=x[S],y=ys[S,5], ylab = ylabs[5], xlab='M.v. relative abundance (%)\nCompetition w/ Sobi; No Mono Pots',type= 'n',ylim=ylims[5,])
points(x=x[S], y=ys[S,5], pch=1, col=2)
lines(stats::lowess(data.frame(x=x[S],y=ys[S,i])), lty=2, col=2)

