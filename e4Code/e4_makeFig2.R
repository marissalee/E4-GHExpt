#e4_makeFig2.R
#Make Figure 2: Mv density treatment vs plant biomass

##################################
### Load libraries ###
library(ggplot2)
library(reshape2)
library(grid)
library(gridExtra)

##################################
### My plot theme ###
source('e4Code/mytheme.R')

##################################
### Prep dataframe ###
source('e4Code/e4_prepdfFig2.R')
#str(data2)

##################################
### Functions ###
#none


##################################
### Figure 2A: Plot Mv treatment vs species biomass ###
sub2<-subset(data2, biommeas != "relmivi") #subset data
sub3<-subset(sub2, biommeas != "total") #subset data

## Base plot
fig2a <- ggplot(sub3, aes(x=as.integer(as.character(mvtrt)), y=biomval, colour=comptrt, shape=comptrt, linetype=biommeas)) +
  mytheme +
  ylab("Species' dry above-\nground plant biomass (g)") + 
  xlab("Microstegium\ndensity treatment") +
  ggtitle('a') +
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
fig2a






##################################
### Figure 2B: Plot Mv treatment vs total biomass ###
sub4<-subset(data2, biommeas == "total") #subset data

## Base plot
fig2b <- ggplot(sub4, aes(x=as.integer(as.character(mvtrt)), y=biomval, colour=comptrt, shape=comptrt)) +
  mytheme +
  ylab("Total dry above-\nground plant biomass (g)") + 
  xlab("Microstegium\ndensity treatment") +
  ggtitle('b') +
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
fig2b



##################################
### Polished Plots ###

## Put Fig2 panels together 
legend <- g_legend(fig2a)
lwidth <- sum(legend$width)
ag<-arrangeGrob(fig2a + theme(legend.position="none"),
                fig2b + theme(legend.position="none"))
fig2<-arrangeGrob(ag, 
                    legend, 
                    widths=unit.c(unit(1, "npc") - lwidth, lwidth), 
                    nrow=1)
fig2

## Export Fig2
ggsave(filename="e4Output_figures/fig2.pdf", plot=fig2, width = 4, height = 5, units = 'in') #save the plot and define its size





### Extra ###################################################

# ### Figure 2B: Plot Mv treatment vs biomass measures ###
# sub<-subset(data2, biommeas == "relmivi") #subset data
# 
# ## Base plot
# fig2b <- ggplot(sub, aes(x=as.integer(as.character(mvtrt)), y=biomval, colour=comptrt, shape=comptrt)) + 
#   mytheme +
#   ylab("Microstegium relative\n abundance (%)") + 
#   xlab("Microstegium\ndensity treatment") + 
#   ggtitle('b') +
#   stat_summary(aes(shape=comptrt, colour=comptrt),fun.y = mean, geom='point', size=2) +
#   stat_summary(aes(colour=comptrt), fun.data = mean_cl_normal, geom = 'errorbar', mult = 1, width=0.2, size=0.5) +
#   stat_summary(aes(colour=comptrt), fun.y = mean, geom='line', size=0.5) +
#   scale_colour_manual(values=c("grey", "black", "black"), 
#                       name="Neighbor\ntreatment",
#                       breaks=c("N", "P", "S"),
#                       labels=c("No Neighbor", "Panicum", "Sorghum")) +
#   scale_shape_manual(values=c(16, 17, 15), 
#                      name="Neighbor\ntreatment",
#                      breaks=c("N", "P", "S"),
#                      labels=c("No Neighbor", "Panicum", "Sorghum")) +
#   coord_cartesian(ylim=c(-5,105)) + scale_y_continuous(breaks=seq(0, 100, 20)) +
#   coord_cartesian(xlim=c(-0.5,6.5)) + scale_x_continuous(breaks=seq(0, 6, 1))
# fig2b
# 
# 
