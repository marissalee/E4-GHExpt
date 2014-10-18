#e4_makeFig2.R
#Make Figure 2: Mv density treatment vs plant biomass


### Load libraries ###
library(ggplot2)
library(reshape2)
library(grid)
library(gridExtra)


### My plot theme ###
source('e4CodePackage_100614/mytheme.R')


### Prep dataframe ###
source('e4CodePackage_100614/e4_prepdfFig2.R')
#str(data2)




### PANEL A: Plot Mv treatment vs biomass measures ###

#subset data
sub<-subset(data2, biommeas == "relmivi")

#set up plot
fig2a <- ggplot(sub, aes(x=as.integer(as.character(mvtrt)), y=biomval, colour=comptrt, shape=comptrt)) + 
  mytheme +
  ylab("Microstegium relative\n abundance (%)") + 
  xlab("Microstegium\ndensity treatment") + 
  ggtitle('a') +
  stat_summary(aes(shape=comptrt, colour=comptrt),fun.y = mean, geom='point', size=2) +
  stat_summary(aes(colour=comptrt), fun.data = mean_cl_normal, geom = 'errorbar', mult = 1, width=0.2, size=0.5) +
  stat_summary(aes(colour=comptrt), fun.y = mean, geom='line', size=0.5) +
  scale_colour_manual(values=c("grey", "black", "black"), 
                      name="Neighbor\ntreatment",
                      breaks=c("N", "P", "S"),
                      labels=c("No Neighbor", "Panicum", "Sorghum")) +
  scale_shape_manual(values=c(16, 17, 15), 
                     name="Neighbor\ntreatment",
                     breaks=c("N", "P", "S"),
                     labels=c("No Neighbor", "Panicum", "Sorghum")) +
  coord_cartesian(ylim=c(-5,105)) + scale_y_continuous(breaks=seq(0, 100, 20)) +
  coord_cartesian(xlim=c(-0.5,6.5)) + scale_x_continuous(breaks=seq(0, 6, 1))
fig2a
#fig2a + geom_jitter(position = position_jitter(w = 0.1, h = 0)) #show the raw data points

#ggsave(filename="fig2a.pdf", width = 6, height = 4, units = 'in') #save the plot and define its size




### PANEL B: Plot Mv treatment vs species biomass ###

#subset data
sub2<-subset(data2, biommeas != "relmivi")
sub3<-subset(sub2, biommeas != "total")

#set up plot
fig2b <- ggplot(sub3, aes(x=as.integer(as.character(mvtrt)), y=biomval, colour=comptrt, shape=comptrt, linetype=biommeas)) +
  mytheme +
  ylab("Species' dry above-\nground plant biomass (g)") + 
  xlab("Microstegium\ndensity treatment") +
  ggtitle('b') +
  stat_summary(mapping=aes(shape=comptrt, colour=comptrt), fun.y = mean, geom='point', size=2) +
  stat_summary(mapping=aes(colour=comptrt, linetype=biommeas), fun.y = mean, geom='line', size=0.5) + 
  stat_summary(fun.data = mean_cl_normal, geom = 'errorbar', mult = 1, width=0.2, size=0.5) +
  scale_colour_manual(values=c("grey", "black", "black"), 
                      name="Neighbor\ntreatment",
                      breaks=c("N", "P", "S"),
                      labels=c("No Neighbor", "Panicum", "Sorghum")) +
  scale_shape_manual(values=c(16, 17, 15), 
                     name="Neighbor\ntreatment",
                     breaks=c("N", "P", "S"),
                     labels=c("No Neighbor", "Panicum", "Sorghum")) +
  scale_linetype_manual(values = c("solid","dotdash"),
                        name="Species",
                        breaks=c("mivi","compabund"),
                        labels=c("Microstegium", "Neighbor sp.")) +
  coord_cartesian(ylim=c(-5,105)) + scale_y_continuous(breaks=seq(0, 100, 20)) +
  coord_cartesian(xlim=c(-0.5,6.5)) + scale_x_continuous(breaks=seq(0, 6, 1))
fig2b
#fig2b + geom_jitter(position = position_jitter(w = 0.1, h = 0))
#ggsave(filename="fig2b.pdf", width = 6, height = 4, units = 'in') #save the plot and define its size




### PANEL C: Plot Mv treatment vs total biomass ###

#subset data
sub4<-subset(data2, biommeas == "total")

#set up plot
fig2c <- ggplot(sub4, aes(x=as.integer(as.character(mvtrt)), y=biomval, colour=comptrt, shape=comptrt)) +
  mytheme +
  ylab("Total dry above-\nground plant biomass (g)") + 
  xlab("Microstegium\ndensity treatment") +
  ggtitle('c') +
  stat_summary(mapping=aes(shape=comptrt, colour=comptrt), fun.y = mean, geom='point', size=2) +
  stat_summary(mapping=aes(colour=comptrt), fun.y = mean, geom='line', size=0.5) + 
  stat_summary(fun.data = mean_cl_normal, geom = 'errorbar', mult = 1, width=0.2, size=0.5) +
  scale_colour_manual(values=c("grey", "black", "black"), 
                      name="Neighbor\ntreatment",
                      breaks=c("N", "P", "S"),
                      labels=c("No Neighbor", "Panicum", "Sorghum")) +
  scale_shape_manual(values=c(16, 17, 15), 
                     name="Neighbor\ntreatment",
                     breaks=c("N", "P", "S"),
                     labels=c("No Neighbor", "Panicum", "Sorghum")) +
  coord_cartesian(ylim=c(-5,105)) + scale_y_continuous(breaks=seq(0, 100, 20)) +
  coord_cartesian(xlim=c(-0.5,6.5)) + scale_x_continuous(breaks=seq(0, 6, 1))
fig2c
#fig2c + geom_jitter(position = position_jitter(w = 0.1, h = 0))
#ggsave(filename="fig2c.pdf", width = 6, height = 4, units = 'in') #save the plot and define its size




### Figure2, ALL PANELS ###

#check out the individual panels
# fig2a
# fig2b #use this legend for all 3 panels
# fig2c

#FUNCTION TO ASSIGN A PLOT'S LEGEND AS A GROB OBJECT
g_legend<-function(p){
  tmp <- ggplotGrob(p)
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

#pick the main legend for all panels
legend <- g_legend(fig2b)
lwidth <- sum(legend$width)

#arrange the grob objects
ag<-arrangeGrob(fig2a + theme(legend.position="none"),
                fig2b + theme(legend.position="none"),
                fig2c + theme(legend.position="none"))
fig2<-arrangeGrob(ag, 
                    legend, 
                    widths=unit.c(unit(1, "npc") - lwidth, lwidth), 
                    nrow=1)
fig2
ggsave(filename="fig2.pdf", plot=fig2, width = 6, height = 10, units = 'in') #save the plot and define its size




