#e4_makeFig3.R
#Make Figure 3: Monoculture type vs soil measure value


### Load libraries ###
library(ggplot2)
library(reshape2)
library(grid)
library(gridExtra)


### My plot theme ###
source('e4CodePackage_100614/mytheme.R')


### Prep dataframe ###
source('e4CodePackage_100614/e4_prepdfFig3.R')
#str(data3)




### Order the factors a logical way ###

data3$soilmeas <- factor(data3$soilmeas, levels = c('nhdi', 'nodi', 'totdi', 'ammonifd', 'nitrifd','minzd','soilmoi'))
data3$type <- factor(data3$type, levels = c('Empty', 'Mivi', 'Pavi', 'Sobi', 'CompEmpty', 'CompSobi', 'CompPavi'))




### PANEL A. Plot monoculture type vs soil value in facets ###

sub1<-subset(data3, type == 'Mivi' | type == 'Empty' | type == 'Pavi' | type == 'Sobi')
fig3a <- ggplot(sub1, aes(x=type, y=soilval)) + 
  mytheme +
  ggtitle('a') +
  ylab('Soil measurement') +
  xlab('Monoculture type') +
  facet_grid( soilmeas ~. , scales='free') +
  stat_summary(fun.y = mean, geom='point', size=3) +
  stat_summary(fun.data = mean_cl_normal, geom = 'errorbar', mult = 1, width=0.2)
#fig3a




### PANEL B. Plot M.v. biomass vs soil value in facets for comptrt=N ###

sub1<-subset(data3, type == 'Mivi' | type == 'Empty' | type == 'CompEmpty')

fig3b <- ggplot(sub1, aes(x=mivi, y=soilval, shape=as.factor(mvtrt))) + 
  mytheme +
  ggtitle('b') +
  ylab('Soil measurement') +
  xlab('M.v. biomass (g)') +
  facet_grid( soilmeas ~. , scales='free') +
  scale_shape_manual(values=c(18, 16, 17, 15, 5, 6), 
                     name="M.v. density\ntreatment",
                     breaks=c("0", "1", "2", "4", "5","6"),
                     labels=c("0", "1", "2", "4", "5","6")) +
  geom_point(aes(shape=as.factor(mvtrt)), size=3)
#fig3b




### All PANELS ###

#FUNCTION TO ASSIGN A PLOT'S LEGEND AS A GROB OBJECT
g_legend<-function(p){
  tmp <- ggplotGrob(p)
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

#pick the main legend for all panels
legend <- g_legend(fig3b)
lwidth <- sum(legend$width)

#arrange the grob objects
tmp<-arrangeGrob(fig3a + theme(legend.position="none"),
                  fig3b + theme(legend.position="none"),
                  nrow=1)
fig3<-arrangeGrob(tmp, 
                  legend, 
                  widths=unit.c(unit(1, "npc") - lwidth, lwidth), 
                  nrow=1)
#fig3
ggsave(filename="fig3.pdf", plot=fig3, width = 10, height = 12, units = 'in') #save the plot and define its size














