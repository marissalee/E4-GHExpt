#E4_R_Code
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

#datasets
n
p
s
all


#########################
#Mivi biomass as metric for Abs abund
plot(mivi~miviind, data=n, xlab='Abs. abund (#ind.)', ylab='Abs. abund (shoot biomass, g)')

#########################
#Abs abund vs Nitrif
plot(nitrifd~mivi, data=n, type='n', xlab='Abs. abund (shoot biomass, g)', ylab='Net nitrification potential (ugN*g-1*d-1)', ylim=c(-7,11))
points(x=n$mivi, y=n$nitrifd, pch=1, col='black', cex=.7)
points(x=p$mivi, y=p$nitrifd, pch=16, col='grey', cex=.7)
points(x=s$mivi, y=s$nitrifd, pch=16, col='black', cex=.7)
lines(stats::lowess(x=n$mivi, y=n$nitrifd))
lines(stats::lowess(x=p$mivi, y=p$nitrifd), col='gray')
lines(stats::lowess(x=s$mivi, y=s$nitrifd), col='black', lty=2)
legend('topright', legend= c('Alone','with Panicum','with Sorghum'), pch=c(1,16,16), lty=c(1,1,2), col=c('black', 'grey','black'), cex=0.7)
#Rel abund vs Nitrif
plot(nitrifd~percmivibiom, data=n, type='n', xlab='Rel. abund (shoot biomass, %)', ylab='Net nitrification potential (ugN*g-1*d-1)', ylim=c(-7,11))
points(x=p$percmivibiom, y=p$nitrifd, pch=16, col='grey', cex=.7)
points(x=s$percmivibiom, y=s$nitrifd, pch=16, col='black', cex=.7)
lines(stats::lowess(x=p$percmivibiom, y=p$nitrifd),col='gray')
lines(stats::lowess(x=s$percmivibiom, y=s$nitrifd), lty=2)
legend('topright', legend= c('with Panicum','with Sorghum'), pch=c(16,16), lty=c(1,2), col=c('grey','black'), cex=0.7)

#Abs abund vs inorganic N
plot(totdi~mivi, data=n, type='n', xlab='Abs. abund (shoot biomass, g)', ylab='Soil inorganic N (ugN*g-1)', ylim=c(0,115))
points(x=n$mivi, y=n$totdi, pch=1, col='black', cex=.7)
points(x=p$mivi, y=p$totdi, pch=16, col='grey', cex=.7)
points(x=s$mivi, y=s$totdi, pch=16, col='black', cex=.7)
lines(stats::lowess(x=n$mivi, y=n$totdi))
lines(stats::lowess(x=p$mivi, y=p$totdi), col='gray')
lines(stats::lowess(x=s$mivi, y=s$totdi), col='black', lty=2)
legend('topright', legend= c('Alone','with Panicum','with Sorghum'), lty=c(1,1,2), pch=c(1,16,16), col=c('black', 'grey','black'), cex=0.7)
#Rel abund vs inorganic N
plot(totdi~percmivibiom, data=n, type='n', xlab='Rel. abund (shoot biomass, %)', ylab='Soil inorganic N (ugN*g-1)', ylim=c(0,115))
points(x=p$percmivibiom, y=p$totdi, pch=16, col='grey', cex=.7)
points(x=s$percmivibiom, y=s$totdi, pch=16, col='black', cex=.7)
lines(stats::lowess(x=p$percmivibiom, y=p$totdi), col='gray')
lines(stats::lowess(x=s$percmivibiom, y=s$totdi), lty=2)
legend('topright', legend= c('with Panicum','with Sorghum'), pch=c(16,16), lty=c(1,2),col=c('grey','black'), cex=0.7)

#Abs abund vs soilmoi
plot(soilmoi~mivi, data=n, type='n', xlab='Abs. abund (shoot biomass, g)', ylab='Soil Moisture (%)', ylim=c(45,100))
points(x=n$mivi, y=n$soilmoi, pch=1, col='black', cex=.7)
points(x=p$mivi, y=p$soilmoi, pch=16, col='grey', cex=.7)
points(x=s$mivi, y=s$soilmoi, pch=16, col='black', cex=.7)
lines(stats::lowess(x=n$mivi, y=n$soilmoi))
lines(stats::lowess(x=p$mivi, y=p$soilmoi), col='gray')
lines(stats::lowess(x=s$mivi, y=s$soilmoi), col='black', lty=2)
legend('topright', legend= c('Alone','with Panicum','with Sorghum'), lty=c(1,1,2), pch=c(1,16,16), col=c('black', 'grey','black'), cex=0.7)
#Rel abund vs soilmoi
plot(soilmoi~percmivibiom, data=n, type='n', xlab='Rel. abund (shoot biomass, %)', ylab='Soil moisture (%)', ylim=c(45,100))
points(x=p$percmivibiom, y=p$soilmoi, pch=16, col='grey', cex=.7)
points(x=s$percmivibiom, y=s$soilmoi, pch=16, col='black', cex=.7)
lines(stats::lowess(x=p$percmivibiom, y=p$soilmoi), col='gray')
lines(stats::lowess(x=s$percmivibiom, y=s$soilmoi), col='black', lty=2)
legend('topright', legend= c('with Panicum','with Sorghum'), pch=c(16,16), lty=c(1,2), col=c('grey','black'), cex=0.7)

#Abs abund vs minzd
plot(minzd~mivi, data=n, type='n', xlab='Abs. abund (shoot biomass, g)', ylab='Net N mineralization potential (ugN*g-1*d-1)', ylim=c(-9,14))
points(x=n$mivi, y=n$minzd, pch=1, col='black', cex=.7)
points(x=p$mivi, y=p$minzd, pch=16, col='grey', cex=.7)
points(x=s$mivi, y=s$minzd, pch=16, col='black', cex=.7)
lines(stats::lowess(x=n$mivi, y=n$minzd))
lines(stats::lowess(x=p$mivi, y=p$minzd), col='gray')
lines(stats::lowess(x=s$mivi, y=s$minzd), col='black', lty=2)
legend('topright', legend= c('Alone','with Panicum','with Sorghum'), lty=c(1,1,2),pch=c(1,16,16), col=c('black', 'grey','black'), cex=0.7)
#Rel abund vs minzd
plot(minzd~percmivibiom, data=n, type='n', xlab='Rel. abund (shoot biomass, %)', ylab='Net N mineralization potential (ugN*g-1*d-1)', ylim=c(-9,14))
points(x=p$percmivibiom, y=p$minzd, pch=16, col='grey', cex=.7)
points(x=s$percmivibiom, y=s$minzd, pch=16, col='black', cex=.7)
lines(stats::lowess(x=p$percmivibiom, y=p$minzd), col='gray')
lines(stats::lowess(x=s$percmivibiom, y=s$minzd), col='black', lty=2)
legend('topright', legend= c('with Panicum','with Sorghum'), lty=c(1,2),pch=c(16,16), col=c('grey','black'), cex=0.7)

#Abs abund vs nhd
plot(nhdi~mivi, data=n, type='n', xlab='Abs. abund (shoot biomass, g)', ylab='Soil NH4+ (ugN*g-1)')
points(x=n$mivi, y=n$nhdi, pch=1, col='black', cex=.7)
points(x=p$mivi, y=p$nhdi, pch=16, col='grey', cex=.7)
points(x=s$mivi, y=s$nhdi, pch=16, col='black', cex=.7)
lines(stats::lowess(x=n$mivi, y=n$nhdi))
lines(stats::lowess(x=p$mivi, y=p$nhdi), col='gray')
lines(stats::lowess(x=s$mivi, y=s$nhdi), col='black', lty=2)
legend('topright', legend= c('Alone','with Panicum','with Sorghum'), lty=c(1,1,2),pch=c(1,16,16), col=c('black', 'grey','black'), cex=0.7)
#Rel abund vs nhd
plot(nhdi~percmivibiom, data=n, type='n', xlab='Rel. abund (shoot biomass, %)', ylab='Soil NH4+ (ugN*g-1)', ylim=c(-9,14))
points(x=p$percmivibiom, y=p$nhdi, pch=16, col='grey', cex=.7)
points(x=s$percmivibiom, y=s$nhdi, pch=16, col='black', cex=.7)
lines(stats::lowess(x=p$percmivibiom, y=p$nhdi), col='gray')
lines(stats::lowess(x=s$percmivibiom, y=s$nhdi), col='black', lty=2)
legend('topright', legend= c('with Panicum','with Sorghum'), lty=c(1,2),pch=c(16,16), col=c('grey','black'), cex=0.7)

#Abs abund vs nodi
plot(nodi~mivi, data=n, type='n', xlab='Abs. abund (shoot biomass, g)', ylab='Soil NO3- (ugN*g-1)')
points(x=n$mivi, y=n$nodi, pch=1, col='black', cex=.7)
points(x=p$mivi, y=p$nodi, pch=16, col='grey', cex=.7)
points(x=s$mivi, y=s$nodi, pch=16, col='black', cex=.7)
lines(stats::lowess(x=n$mivi, y=n$nodi))
lines(stats::lowess(x=p$mivi, y=p$nodi), col='gray')
lines(stats::lowess(x=s$mivi, y=s$nodi), col='black', lty=2)
legend('topright', legend= c('Alone','with Panicum','with Sorghum'), lty=c(1,1,2),pch=c(1,16,16), col=c('black', 'grey','black'), cex=0.7)
#Rel abund vs nodi
plot(nodi~percmivibiom, data=n, type='n', xlab='Rel. abund (shoot biomass, %)', ylab='Soil NO3- (ugN*g-1)')
points(x=p$percmivibiom, y=p$nodi, pch=16, col='grey', cex=.7)
points(x=s$percmivibiom, y=s$nodi, pch=16, col='black', cex=.7)
lines(stats::lowess(x=p$percmivibiom, y=p$nodi), col='gray')
lines(stats::lowess(x=s$percmivibiom, y=s$nodi), col='black', lty=2)
legend('topright', legend= c('with Panicum','with Sorghum'), lty=c(1,2),pch=c(16,16), col=c('grey','black'), cex=0.7)



plot(nitrifd~mivi, data=n, xlab='Abs. abund (shoot biomass, g)', ylab='Net nitrification potential (ugN*g-1*d-1)', ylim=c(-7,11))
mtext('Microstegium alone', line=.5, adj=1)
plot(nitrifd~mivi, data=p,xlab='Abs. abund (shoot biomass, g)', ylab='Net nitrification potential (ugN*g-1*d-1)', ylim=c(-7,11))
mtext('Microstegium with Panicum', line=.5, adj=1)
plot(nitrifd~mivi, data=s,xlab='Abs. abund (shoot biomass, g)', ylab='Net nitrification potential (ugN*g-1*d-1)', ylim=c(-7,11))
mtext('Microstegium with Sorghum', line=.5, adj=1)

#Rel abund vs Nitrif
plot(nitrifd~percmivibiom, data=p,xlab='Rel. abund (shoot biomass, %)', ylab='Net nitrification potential (ugN*g-1*d-1)', ylim=c(-7,11))
mtext('Microstegium with Panicum', line=.5, adj=1)
plot(nitrifd~percmivibiom, data=s,xlab='Rel. abund (shoot biomass, %)', ylab='Net nitrification potential (ugN*g-1*d-1)', ylim=c(-7,11))
mtext('Microstegium with Sorghum', line=.5, adj=1)


##########################
#Abs abund vs inorganicN
plot(totdi~mivi, data=n, xlab='Abs. abund (shoot biomass, g)', ylab='Soil Inorganic Nitrogen (ugN*g-1)', ylim=c(0,115))
mtext('Microstegium alone', line=.5, adj=1)
plot(totdi~mivi, data=p,xlab='Abs. abund (shoot biomass, g)', ylab='Soil Inorganic Nitrogen (ugN*g-1)', ylim=c(0,115))
mtext('Microstegium with Panicum', line=.5, adj=1)
plot(totdi~mivi, data=s,xlab='Abs. abund (shoot biomass, g)', ylab='Soil Inorganic Nitrogen (ugN*g-1)', ylim=c(0,115))
mtext('Microstegium with Sorghum', line=.5, adj=1)

#Rel abund vs inorganicN
plot(totdi~percmivibiom, data=p,xlab='Rel. abund (shoot biomass, %)', ylab='Soil Inorganic Nitrogen (ugN*g-1)', ylim=c(0,115))
mtext('Microstegium with Panicum', line=.5, adj=1)
plot(totdi~percmivibiom, data=s,xlab='Rel. abund (shoot biomass, %)', ylab='Soil Inorganic Nitrogen (ugN*g-1)', ylim=c(0,115))
mtext('Microstegium with Sorghum', line=.5, adj=1)

###########################
#Abs abund vs minz
plot(minzd~mivi, data=n, xlab='Abs. abund (shoot biomass, g)', ylab='Net N mineralization potential (ugN*g-1*d-1)', ylim=c(-10,10))
mtext('Microstegium alone', line=.5, adj=1)
plot(minzd~mivi, data=p,xlab='Abs. abund (shoot biomass, g)', ylab='Net N mineralization potential (ugN*g-1*d-1)', ylim=c(-10,10))
mtext('Microstegium with Panicum', line=.5, adj=1)
plot(minzd~mivi, data=s,xlab='Abs. abund (shoot biomass, g)', ylab='Net N mineralization potential (ugN*g-1*d-1)', ylim=c(-10,10))
mtext('Microstegium with Sorghum', line=.5, adj=1)

#Rel abund vs inorganicN
plot(minzd~percmivibiom, data=p,xlab='Rel. abund (shoot biomass, %)', ylab='Net N mineralization potential (ugN*g-1*d-1)', ylim=c(-10,10))
mtext('Microstegium with Panicum', line=.5, adj=1)
plot(minzd~percmivibiom, data=s,xlab='Rel. abund (shoot biomass, %)', ylab='Net N mineralization potential (ugN*g-1*d-1)', ylim=c(-10,10))
mtext('Microstegium with Sorghum', line=.5, adj=1)

###########################
#Abs abund vs soilmoi
plot(soilmoi~mivi, data=n, xlab='Abs. abund (shoot biomass, g)', ylab='Soil Moisture (%)',ylim=c(45,85))
mtext('Microstegium alone', line=.5, adj=1)
plot(soilmoi~mivi, data=p,xlab='Abs. abund (shoot biomass, g)', ylab='Soil Moisture (%)',ylim=c(45,85))
mtext('Microstegium with Panicum', line=.5, adj=1)
plot(soilmoi~mivi, data=s,xlab='Abs. abund (shoot biomass, g)', ylab='Soil Moisture (%)',ylim=c(45,85))
mtext('Microstegium with Sorghum', line=.5, adj=1)

#Rel abund vs soilmoi
plot(soilmoi~percmivibiom, data=p,xlab='Rel. abund (shoot biomass, %)', ylab='Soil Moisture (%)',ylim=c(45,85))
mtext('Microstegium with Panicum', line=.5, adj=1)
plot(soilmoi~percmivibiom, data=s,xlab='Rel. abund (shoot biomass, %)', ylab='Soil Moisture (%)',ylim=c(45,85))
mtext('Microstegium with Sorghum', line=.5, adj=1)

###########################
#total biomass vs nitrifd
plot(nitrifd~total, data=all, xlab='Abs. abund (shoot biomass, g)', ylab='Nitrification')
mtext('All pots', line=.5, adj=1)
plot(minzd~total, data=all, xlab='Abs. abund (shoot biomass, g)', ylab='Net N mineralization')
mtext('All pots', line=.5, adj=1)
plot(totdi~total, data=all, xlab='Abs. abund (shoot biomass, g)', ylab='Soil inorganic N (ugN*g-1)')
mtext('All pots', line=.5, adj=1)
plot(soilmoi~total, data=all, xlab='Abs. abund (shoot biomass, g)', ylab='Soil Moisture (%)')
mtext('All pots', line=.5, adj=1)


