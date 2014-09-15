#E4
#Can I explain N conc and fluxes based on plant biomass alone?  does it help to include spp?

################################################
setwd("~/Desktop/Lachat Data/R stuff")
e4v <- read.table("E4_veg.txt",header=T)
e4m <- read.table("E4_minz.txt",header=T)
e4.1 <- cbind(e4v,e4m)
e4 <- cbind(e4.1[,1:14],e4.1[,20:26])

#################################################

xyplot(nhdi ~ total, data=e4, smooth=F, reg.line=F, legend.coords="bottomright")
abline(lm(nhdi~total, data=e4), lty=2)
summary(lm(nhdi~total, data=e4))

xyplot(nodi ~ total, data=e4, smooth=F, reg.line=F, legend.coords="bottomright")
abline(lm(nhdi~total, data=e4), lty=2)
summary(lm(nhdi~total, data=e4))

xyplot(totdi ~ total, data=e4, smooth=F, reg.line=F, legend.coords="bottomright")
abline(lm(nhdi~total, data=e4), lty=2)
summary(lm(nhdi~total, data=e4))

xyplot(percnodi ~ total, data=e4, smooth=F, reg.line=F, legend.coords="bottomright")
abline(lm(nhdi~total, data=e4), lty=2)
summary(lm(nhdi~total, data=e4))

xyplot(nitrifd ~ total|comptrt, data=e4, smooth=F, reg.line=F, legend.coords="bottomright")
abline(lm(nhdi~total, data=e4), lty=2)
summary(lm(nhdi~total, data=e4))

xyplot(minzd ~ total, data=e4, smooth=F, reg.line=F, legend.coords="bottomright")
abline(lm(nhdi~total, data=e4), lty=2)
summary(lm(nhdi~total, data=e4))
