#e4_makeFig4.R
#Make Figure 4: Mixture plant biomass vs soil measure (sF) or soil measure difference relative to the 2 baselines, delta.sE (empty pots) and delta.sP (non-M.v. full pots)

## Load libraries
library(ggplot2)
library(reshape2)
library(grid)
library(gridExtra)

## My plot theme
source('e4CodePackage_100614/mytheme.R')

## Remove unnecessary cols  
#str(datas)
removecols<-c('s0Epid','s0Ppid','s0E','s0P','bk','julydate','type','delta.sE','delta.sP')
indx<-colnames(datas) %in% removecols
datas.r<-datas[,!indx]
#str(datas.r)

## Make it so that total in comptrt = N is NA (so that this facet doesn't print duplicate information)
datas.r[datas.r$comptrt == 'N','total'] <- NA

#colnames(datas.r)
ru.datas <- data.frame(sFpid=datas.r$sFpid,
                       comptrt=datas.r$comptrt,
                       mvtrt=datas.r$mvtrt,
                       mivi=datas.r$mivi,
                       compabund=datas.r$compabund,
                       total=datas.r$total,
                       soilmeas=datas.r$scol,
                       sF=datas.r$sF)
data4.1 <- ru.datas
#View(data4.1)


## Reshape so that plant biomass values are all in one column (biomval), with an identifier column to identify what type of biomass that value represents (biommeas)  
data4.2 <- melt(data4.1, measure.vars=c('mivi','compabund','total'), id.vars=c('sFpid','comptrt','mvtrt','soilmeas','sF'))
#View(data4.2)

#colnames(data4.2)
colnames(data4.2)[6]<-'biommeas'
colnames(data4.2)[7]<-'biomval'


## Plot sF (the final soil measure) vs M.v. biomass and total biomass
# reorder the factors within soilmeas
data4.3 <- transform(data4.2, soilmeas = factor(soilmeas, 
                                                levels=c("nhdi", "nodi", "totdi", 
                                                         "ammonifd", "nitrifd","minzd",
                                                         "soilmoi")))
data4.3 <- transform(data4.3, comptrt = factor(comptrt, 
                                               levels=c("N","P","S"),
                                               labels=c("Neighbor = None","Neighbor = Panicum","Neighbor = Sorghum")))
data4.3 <- transform(data4.3, biommeas = factor(biommeas, 
                                               levels=c("mivi","comptrt","total"),
                                               labels=c("Biomass = M.v.","Biomass = Neighbor","Biomass = Total")))

sub1<-subset(data4.3, biommeas == "Biomass = M.v." | biommeas == "Biomass = Total")

## For nodi
sub2<-subset(sub1, soilmeas == 'nodi')
fig4a <- ggplot(sub2, aes(x=biomval, y=sF, shape=as.factor(mvtrt))) + 
  mytheme + theme(legend.position = c(0.5, 0.5), legend.justification = c(1, 0), legend.background=element_rect(colour='black')) +
  ggtitle('a') +
  ylab("Nitrate (ugN/G)") + 
  xlab("Dry aboveground plant biomass (g)") + 
  facet_grid(comptrt ~ biommeas , scales='free') +
  scale_shape_manual(values=c(16, 17, 15, 5), 
                     name="M.v. density\ntreatment",
                     breaks=c("1", "2", "4", "5"),
                     labels=c("1", "2", "4","5")) +
  geom_point(size=3)
#fig4a

## For nitrifd
sub2<-subset(sub1, soilmeas == 'nitrifd')
fig4b <- ggplot(sub2, aes(x=biomval, y=sF, shape=as.factor(mvtrt))) + 
  mytheme + theme(legend.position = c(0.75, 0.75), legend.justification = c(1, 0), legend.background=element_rect(colour='black')) +
  ggtitle('b') +
  ylab("Nitrification (ugN/G*d)") + 
  xlab("Dry aboveground plant biomass (g)") + 
  facet_grid(comptrt ~ biommeas , scales='free') +
  scale_shape_manual(values=c(16, 17, 15, 5), 
                     name="M.v. density\ntreatment",
                     breaks=c("1", "2", "4", "5"),
                     labels=c("1", "2", "4","5")) +
  geom_point(size=3)
#fig4b

## For soilmoi
sub2<-subset(sub1, soilmeas == 'soilmoi')
fig4c <- ggplot(sub2, aes(x=biomval, y=sF, shape=as.factor(mvtrt))) + 
  mytheme + theme(legend.position = c(0.75, 0.75), legend.justification = c(1, 0), legend.background=element_rect(colour='black')) +
  ggtitle('c') +
  ylab("Soil moisture (%)") + 
  xlab("Dry aboveground plant biomass (g)") + 
  facet_grid(comptrt ~ biommeas , scales='free') +
  scale_shape_manual(values=c(16, 17, 15, 5), 
                     name="M.v. density\ntreatment",
                     breaks=c("1", "2", "4", "5"),
                     labels=c("1", "2", "4","5")) +
  geom_point(size=3)
#fig4c


## All panels

#FUNCTION TO ASSIGN A PLOT'S LEGEND AS A GROB OBJECT
g_legend<-function(p){
  tmp <- ggplotGrob(p)
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

#pick the main legend for all panels
legend <- g_legend(fig4a)

#arrange the grob objects
fig4<-arrangeGrob(fig4a + theme(legend.position="none"),
                fig4b + theme(legend.position="none"),
                fig4c + theme(legend.position="none"), 
                legend,
                nrow=2, ncol=2)
#fig4
ggsave(filename="fig4.pdf", plot=fig4, width = 12, height = 12, units = 'in') #save the plot and define its size





