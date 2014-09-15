#e4 figures2b - regressions
# 2/24/13
setwd("~/Desktop/E4_R")
data<-read.table('e4_trimmedData.txt',header=T)
data$bk<-as.factor(data$bk)
data$Mvtrt<-as.factor(data$Mvtrt)
data$compabund<-data$pavi + data$sobi

##################################
#make datasets
# dataset1: No competition treatment, all Mvtrt levels
Empty<-data[data$type=='Empty',]
Mivi<-data[data$type=='Mivi',]
CompEmpty<-data[data$type=='CompEmpty',]
dataset1<-rbind(Empty, CompEmpty, Mivi)
write.table(dataset1,file='dataset1.txt', sep='\t')
# dataset2: Competition treatment, all Mvtrt levels
CompPavi<-data[data$type=='CompPavi',]
Pavi<-data[data$type=='Pavi',]
CompSobi<-data[data$type=='CompSobi',]
Sobi<-data[data$type=='Sobi',]
dataset2<-rbind(Mivi, CompPavi, Pavi, CompSobi, Sobi)
write.table(dataset2,file='dataset2.txt', sep='\t')
# dataset3: Monocultures
dataset3<-rbind(Empty,Sobi,Pavi,Mivi)
write.table(dataset3,file='dataset3.txt', sep='\t')

##################################
##################################
def.par <- par(no.readonly = TRUE) # save default, for resetting...
ylabs<-c('Competitor\n biomass (g)','Soil moisture\n (%)','Nitrate\n (ugN/G)','Ammonium:\nNitrate ratio','Nitrification\nrate (ugN/G*d)','Mineralization\nrate (ugN/G*d)')
ylim1<-c(0,120)
ATy1<-seq(0,120,20)
ylim2<-c(50,100)
ATy2<-seq(50,100,20)
ylim3<-c(0,120)
ATy3<-seq(0,120,20)
ylim4<-c(0,1)
ATy4<-seq(0,1,0.2)
ylim5<-c(-10,10)
ATy5<-seq(-10,10,2)
ylim6<-c(-10,10)
ATy6<-seq(-10,10,2)
ylims<-rbind(ylim1,ylim2,ylim3,ylim4,ylim5,ylim6)
ATys<-list(ATy1,ATy2,ATy3,ATy4,ATy5,ATy6)

nf <- layout(matrix(c(1:30),
                    6,5), 
             widths=c(1.3,1,1,1,1), 
             heights=c(1,1,1,1,1,1.5), 
             respect=F)
layout.show(nf)

##################################
#make panels (dataset1 and dataset2)

#################################################################
#panel1: MVabund_alone
E<-which(dataset1$type=='Empty')
ME<-which(dataset1$type=='CompEmpty')
M<-which(dataset1$type=='Mivi')
x<-dataset1$Mvabund
y1<-dataset1$Mvabund
y2<-dataset1$soilmoi
y3<-dataset1$nodi
y4<-dataset1$nh.no
y5<-dataset1$nitrifd
y6<-dataset1$minzd
ys<-cbind(y1,y2,y3,y4,y5,y6)
AT<-seq(0,50,10)
par(mar=c(1,5,1,1)) # mar=c(bottom, left, top, right)
plot(x=x,y=ys[,1], ylab = ylabs[1], xlab='', xaxt = "n",yaxt='n',type= 'n', ylim=ylims[1,])
legend('center', c('Empty','M.v.','P.v.','S.b.','M.v. + Empty','M.v. + P.v.','M.v. + S.b.'), pch=c(1,19,19,19,2,2,2),col=c(1,1,'grey56','grey82',1,'grey56','grey82'), cex=.8, bty='n')

for (i in (2:6)){
  if(i!=6){
  	plot(x=x,y=ys[,i], ylab = ylabs[i], xlab='', xaxt = "n",yaxt='n',type= 'n', ylim=ylims[i,])
  	axis(1, at=AT, tick=T, labels=F, tcl=-.2)
  	axis(2, at=ATys[[i]],tick=T,labels=T, tcl=-.2)
  	points(x=x[E], y=ys[E,i], pch=1)
  	points(x=x[ME], y=ys[ME,i], pch=2)
  	points(x=x[M], y=ys[M,i], pch=19)
  	lines(stats::lowess(data.frame(x=x[c(E,ME,M)],y=ys[c(E,ME,M),i])))
  }
  if(i==6){
	par(mar=c(5,5,1,1))
	plot(x=x,y=ys[,i], ylab = ylabs[i], xlab='M.v. abundance (g)\nGrown alone',type='n', ylim=ylims[i,])
	points(x=x[E], y=ys[E,i], pch=1)
  	points(x=x[ME], y=ys[ME,i], pch=2)
  	points(x=x[M], y=ys[M,i], pch=19)
  	lines(stats::lowess(data.frame(x=x[c(E,ME,M)],y=ys[c(E,ME,M),i])))
	}
}

#################################################################
#panel2: MVabund_incomp with dataset2
P<-which(dataset2$type=='Pavi')
MP<-which(dataset2$type=='CompPavi')
S<-which(dataset2$type=='Sobi')
MS<-which(dataset2$type=='CompSobi')
M<-which(dataset2$type=='Mivi')
x<-dataset2$Mvabund
y1<-dataset2$compabund
y2<-dataset2$soilmoi
y3<-dataset2$nodi
y4<-dataset2$nh.no
y5<-dataset2$nitrifd
y6<-dataset2$minzd
ys<-cbind(y1,y2,y3,y4,y5,y6)
AT<-seq(0,50,10)
#pavi panel
par(mar=c(1,1,1,1)) # mar=c(bottom, left, top, right)
for (i in (1:6)){
  if(i!=6){
  	plot(x=x,y=ys[,i], ylab='', xlab='', xaxt = "n", yaxt='n',type= 'n',ylim=ylims[i,])
  	axis(1, at=AT, tick=T, labels=F, tcl=-.2)
  	axis(2, at=ATys[[i]], tick=T, labels=F, tcl=-.2)
  	points(x=x[MP], y=ys[MP,i], pch=2, col='grey56')
  	lines(stats::lowess(data.frame(x=x[c(MP)],y=ys[c(MP),i])), lty=1)
  	}
 if(i==6){
 	par(mar=c(5,1,1,1))
 	plot(x=x,y=ys[,i], ylab='',yaxt='n', xlab='M.v. abundance (g)\nGrown with P.v.',type= 'n',ylim=ylims[i,])
 	axis(2, at=ATys[[i]], tick=T, labels=F, tcl=-.2)
 	points(x=x[MP], y=ys[MP,i], pch=2, col='grey56')
 	lines(stats::lowess(data.frame(x=x[c(MP)],y=ys[c(MP),i])), lty=1)
 	} 
}
#sobi panel
par(mar=c(1,1,1,1)) # mar=c(bottom, left, top, right)
for (i in (1:6)){
  if(i!=6){
  	plot(x=x,y=ys[,i], ylab='', xlab='', xaxt = "n", yaxt='n',type= 'n',ylim=ylims[i,])
  	axis(1, at=AT, tick=T, labels=F, tcl=-.2)
  	axis(2, at=ATys[[i]], tick=T, labels=F, tcl=-.2)
  	points(x=x[MS], y=ys[MS,i], pch=2, col='grey82')
  	lines(stats::lowess(data.frame(x=x[c(MS)],y=ys[c(MS),i])), lty=1)
  	}
 if(i==6){
 	par(mar=c(5,1,1,1))
 	plot(x=x,y=ys[,i], ylab='',yaxt='n', xlab='M.v. abundance (g)\nGrown with S.b.',type= 'n',ylim=ylims[i,])
 	axis(2, at=ATys[[i]], tick=T, labels=F, tcl=-.2)
 	points(x=x[MS], y=ys[MS,i], pch=2, col='grey82')
 	lines(stats::lowess(data.frame(x=x[c(MS)],y=ys[c(MS),i])), lty=1)
 	} 
}


#################################################################
#panel3: MVrelabund_incomp with dataset2
x<-dataset2$MVrelabund
AT<-seq(0,100,20)
#pavi panel
par(mar=c(1,1,1,1)) # mar=c(bottom, left, top, right)
for (i in (1:6)){
  if(i!=6){
  	plot(x=x,y=ys[,i], ylab='', xlab='', xaxt = "n", yaxt='n',type= 'n',ylim=ylims[i,])
  	axis(1, at=AT, tick=T, labels=F, tcl=-.2)
  	axis(2, at=ATys[[i]], tick=T, labels=F, tcl=-.2)
  	points(x=x[MP], y=ys[MP,i], pch=2, col='grey56')
  	lines(stats::lowess(data.frame(x=x[c(MP)],y=ys[c(MP),i])), lty=1)
  	}
 if(i==6){
 	par(mar=c(5,1,1,1))
 	plot(x=x,y=ys[,i], ylab='',yaxt='n', xlab='M.v. relative abundance (%)\nGrown with P.v.',type= 'n',ylim=ylims[i,])
 	axis(2, at=ATys[[i]], tick=T, labels=F, tcl=-.2)
 	points(x=x[MP], y=ys[MP,i], pch=2, col='grey56')
 	lines(stats::lowess(data.frame(x=x[c(MP)],y=ys[c(MP),i])), lty=1)
 	} 
}
#sobi panel
par(mar=c(1,1,1,1)) # mar=c(bottom, left, top, right)
for (i in (1:6)){
  if(i!=6){
  	plot(x=x,y=ys[,i], ylab='', xlab='', xaxt = "n", yaxt='n',type= 'n',ylim=ylims[i,])
  	axis(1, at=AT, tick=T, labels=F, tcl=-.2)
  	axis(2, at=ATys[[i]], tick=T, labels=F, tcl=-.2)
  	points(x=x[MS], y=ys[MS,i], pch=2, col='grey82')
  	lines(stats::lowess(data.frame(x=x[c(MS)],y=ys[c(MS),i])), lty=1)
  	}
 if(i==6){
 	par(mar=c(5,1,1,1))
 	plot(x=x,y=ys[,i], ylab='',yaxt='n', xlab='M.v. relative abundance (%)\nGrown with S.b.',type= 'n',ylim=ylims[i,])
 	axis(2, at=ATys[[i]], tick=T, labels=F, tcl=-.2)
 	points(x=x[MS], y=ys[MS,i], pch=2, col='grey82')
 	lines(stats::lowess(data.frame(x=x[c(MS)],y=ys[c(MS),i])), lty=1)
 	} 
}



