#e4_makeFig4.R
#Make Figure 4: Mixture plant biomass vs soil measure (sF)

### Load libraries ###
library(ggplot2)
library(reshape2)
library(grid)
library(gridExtra)


### My plot theme ###
source('e4CodePackage_100614/mytheme.R')


### Prep dataframe ###
source('e4CodePackage_100614/e4_prepdfFig4.R')
#str(data4)




### Order the factors a logical way and give them pretty labels ###

data4 <- transform(data4, soilmeas = factor(soilmeas, levels=c("nhdi", "nodi", "totdi", "ammonifd", "nitrifd","minzd", "soilmoi")))
data4 <- transform(data4, comptrt = factor(comptrt, levels=c("N","P","S"), labels=c("Neighbor \n= None","Neighbor \n= Panicum","Neighbor \n= Sorghum")))
data4 <- transform(data4, biommeas = factor(biommeas, levels=c("mivi","comptrt","total"), labels=c("Biomass = M.v.","Biomass = Neighbor","Biomass = Total")))




### Subset data - I'm only looking at mivi biomass and total biomass for this figure ###

sub1<-subset(data4, biommeas == "Biomass = M.v." | biommeas == "Biomass = Total")




### PANEL A. Nitrate ###

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




### PANEL B. Nitrification ###

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




### PANEL C. Soil moisture ###

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




### ALL PANELs ###

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





