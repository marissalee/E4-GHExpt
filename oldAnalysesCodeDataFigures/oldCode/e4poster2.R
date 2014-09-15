#E4poster, take 2
#n.txt = no competitor, miviind=0:6
#p.txt = pavi competitor, miviind=0:6; when miviind=0, paviind=6
#s.txt = sobi competitor, miviind=0:6; when miviind=0, sobiind=6

setwd("~/Desktop/E4_R")
n0 <- read.table("n.txt",header=T)
p0 <- read.table("p.txt",header=T)
s0 <- read.table("s.txt",header=T)
all0<- read.table("all.txt", header=T)

#dataset cleaning
n1 <- n0[!is.na(n0$nitrifd),] #elim rows with NA in nitrifd column
n4 <- n1[!is.na(n1$mivi),] #elim rows with NA in mivi column
n2 <- n4[is.na(n4$notes),] #elim rows without NA in the notes column
n <- n2

p1 <- p0[!is.na(p0$nitrifd),] #elim rows with NA in nitrifd column
p4 <- p1[!is.na(p1$mivi),] #elim rows with NA in mivi column
p2 <- p4[is.na(p4$notes),] #elim rows without NA in the notes column
p <- p2

s1 <- s0[!is.na(s0$nitrifd),] #elim rows with NA in nitrifd column
s4 <- s1[!is.na(s1$mivi),] #elim rows with NA in mivi column
s2 <- s4[is.na(s4$notes),] #elim rows without NA in the notes column
s <- s2

all1 <- all0[!is.na(all0$nitrifd),] #elim rows with NA in nitrifd column
all4 <- all1[!is.na(all1$mivi),] #elim rows with NA in mivi column
all2 <- all4[is.na(all4$notes),] #elim rows without NA in the notes column
all <- all2

#########################
#Mivi biomass as metric for Abs abund
plot(mivi~miviind, data=n, type='n',xlab='Abs. abund (#ind.)', ylab='Abs. abund (shoot biomass, g)')
points(x=n$miviind, y=n$mivi, col='black', pch=1, cex=.7)
points(x=p$miviind, y=p$mivi, col='black', pch=16, cex=.7)
points(x=s$miviind, y=s$mivi, col='gray', pch=16, cex=.7)
lines(stats::lowess(x=n$miviind, y=n$mivi), col='black', lty=1)
lines(stats::lowess(x=p$miviind, y=p$mivi), col='black', lty=2)
lines(stats::lowess(x=s$miviind, y=s$mivi), col='gray', lty=2)
legend('topleft', legend= c('Alone','with Panicum','with Sorghum'), lty=c(1,2,2),pch=c(1,16,16), col=c('black','black', 'grey'), cex=0.7)

#########################

plot(nhdi~mivi, data=n, xlab='Abs. abund (shoot biomass, g)', ylab='Soil NH4+ (ugN*g-1)', cex=.7)
lines(stats::lowess(x=n$mivi, y=n$nhdi), col='black', lty=1)
lines(stats::lowess(x=p$mivi, y=p$nhdi), col='black', lty=2)
lines(stats::lowess(x=s$mivi, y=s$nhdi), col='gray', lty=2)

plot(nodi~mivi, data=n, xlab='Abs. abund (shoot biomass, g)', ylab='Soil NO3- (ugN*g-1)', cex=.7)
lines(stats::lowess(x=n$mivi, y=n$nodi), col='black', lty=1)
lines(stats::lowess(x=p$mivi, y=p$nodi), col='black', lty=2)
lines(stats::lowess(x=s$mivi, y=s$nodi), col='gray', lty=2)

plot(nitrifd~mivi, data=n, xlab='Abs. abund (shoot biomass, g)', ylab='Nitrification rate (ugN*g-1*d-1)', cex=.7)
lines(stats::lowess(x=n$mivi, y=n$nitrifd), col='black', lty=1)
lines(stats::lowess(x=p$mivi, y=p$nitrifd), col='black', lty=2)
lines(stats::lowess(x=s$mivi, y=s$nitrifd), col='gray', lty=2)

plot(minzd~mivi, data=n, xlab='Abs. abund (shoot biomass, g)', ylab='N Mineralization rate (ugN*g-1*d-1)', cex=.7)
lines(stats::lowess(x=n$mivi, y=n$minzd), col='black', lty=1)
lines(stats::lowess(x=p$mivi, y=p$minzd), col='black', lty=2)
lines(stats::lowess(x=s$mivi, y=s$minzd), col='gray', lty=2)

plot(soilmoi~mivi, data=n, xlab='Abs. abund (shoot biomass, g)', ylab='Soil moisture (%)', cex=.7)
lines(stats::lowess(x=n$mivi, y=n$soilmoi), col='black', lty=1)
lines(stats::lowess(x=p$mivi, y=p$soilmoi), col='black', lty=2)
lines(stats::lowess(x=s$mivi, y=s$soilmoi), col='gray', lty=2)

mtext('Microstegium alone', line=.5, adj=1)






