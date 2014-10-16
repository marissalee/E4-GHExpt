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
source('e4CodePackage_100614/e4_prepdfFig5.R')
#str(data4)
data5<-data4 #just switch names


### Order the factors a logical way and give them pretty labels ###

data5 <- transform(data5, soilmeas = factor(soilmeas, levels=c("nhdi", "nodi", "totdi", "ammonifd", "nitrifd","minzd", "soilmoi")))
data5 <- transform(data5, comptrt = factor(comptrt, levels=c("N","P","S")))
data5 <- transform(data5, biommeas = factor(biommeas, levels=c("mivi","comptrt","total")))


### Make prettier labels for the  soil measurements ###

soilmeas_names <- list(
  'nhdi'="Ammonium\n(ugN/G)",
  'nodi'="Nitrate\n(ugN/G)",
  'totdi'="Total Inorganic N\n(ugN/G)",
  'ammonifd'="Ammonification\n(ugN/G*d)",
  'nitrifd'="Nitrification\n(ugN/G*d)",
  'minzd'="Mineralization\n(ugN/G*d)",
  'soilmoi'="Soil Moisture\n(%)"
)

soilmeas_labeller <- function(variable,value){
  return(soilmeas_names[value])
}




### Figure 5a. Mivi vs soilmeasures, shapes are comptrt ###

### Subset data - I'm only looking at mivi biomass this panel ###
sub1<-subset(data5, biommeas == "mivi")

fig5a <- ggplot(sub1, aes(x=biomval, y=sF, shape=comptrt, colour=comptrt)) + 
  mytheme +
  ylab("Soil measurement") + 
  xlab("Dry aboveground\nMicrostegium biomass (g)") + 
  facet_grid(soilmeas ~ ., scales='free', labeller=soilmeas_labeller)  +
  geom_point(size=3) +
  scale_colour_manual(values=c("grey", "black", "black"), 
                      name="Neighbor\ntreatment",
                      breaks=c("N", "P", "S"),
                      labels=c("No Neighbor", "Panicum", "Sorghum")) +
  scale_shape_manual(values=c(16, 17, 0), 
                     name="Neighbor\ntreatment",
                     breaks=c("N", "P", "S"),
                     labels=c("No Neighbor", "Panicum", "Sorghum"))
#fig5a




### Figure 5b. Mivi vs soilmeasures, shapes are comptrt ###

### Subset data - I'm only looking at mivi biomass this panel ###
sub1<-subset(data5, biommeas == "total")

fig5b <- ggplot(sub1, aes(x=biomval, y=sF, shape=comptrt, colour=comptrt)) + 
  mytheme +
  ylab("Soil measurement") + 
  xlab("Dry aboveground\ntotal plant biomass (g)") + 
  facet_grid(soilmeas ~ ., scales='free', labeller=soilmeas_labeller)  +
  geom_point(size=3) +
  scale_colour_manual(values=c("grey", "black", "black"), 
                      name="Neighbor\ntreatment",
                      breaks=c("N", "P", "S"),
                      labels=c("No Neighbor", "Panicum", "Sorghum")) +
  scale_shape_manual(values=c(16, 17, 0), 
                     name="Neighbor\ntreatment",
                     breaks=c("N", "P", "S"),
                     labels=c("No Neighbor", "Panicum", "Sorghum"))
#fig5b




### ALL PANELS ###

#FUNCTION TO ASSIGN A PLOT'S LEGEND AS A GROB OBJECT
g_legend<-function(p){
  tmp <- ggplotGrob(p)
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

#pick the main legend for all panels
legend <- g_legend(fig5a)
lwidth <- sum(legend$width)

#arrange the grob objects
ag<-arrangeGrob(fig5a + theme(legend.position="none"),
                fig5b + theme(legend.position="none"),
                nrow=1)
fig5<-arrangeGrob(ag, 
                  legend, 
                  widths=unit.c(unit(1, "npc") - lwidth, lwidth), 
                  nrow=1)
#fig5
ggsave(filename="fig5.pdf", plot=fig5, width = 10, height = 12, units = 'in') #save the plot and define its size








