#e4_makeFig3.R
#Make Figure 3: Monoculture type vs soil measure value

## Load libraries
library(ggplot2)
library(reshape2)
library(grid)
library(gridExtra)

## My plot theme
source('e4CodePackage_100614/mytheme.R')

## Remove unnecessary cols  
#str(data)
removecols<-c('bk','julydate',
              'pavi','sobi','weed','relmivi','compabund',
              'notes')
indx<-colnames(data) %in% removecols
data.r<-data[,!indx]

colnames(data.r)
ru.data<- data.frame(potid=data.r$potid,
                     type=data.r$type,
                     comptrt=data.r$comptrt,
                     mvtrt=data.r$mvtrt,
                     mivi=data.r$mivi,
                     total=data.r$total,
                     nhdi=data.r$nhdi,
                     nodi=data.r$nodi,
                     totdi=data.r$totdi,
                     ammonifd=data.r$ammonifd,
                     nitrifd=data.r$nitrifd,
                     minzd=data.r$minzd,
                     soilmoi=data.r$soilmoi)
data3.1 <- ru.data
#View(data3.1)


## Reshape so that plant biomass values are all in one column (biomval), with an identifier column to identify what type of biomass that value represents (biommeas) and add a column that weights the soilvals by total plant biomass
data3.2 <- melt(data3.1, measure.vars=c('nhdi','nodi','totdi','ammonifd','nitrifd','minzd','soilmoi'), id.vars=c('potid','type','comptrt','mvtrt','mivi','total'))
#View(data3.2)
#colnames(data3.2)
colnames(data3.2)[7]<-'soilmeas'
colnames(data3.2)[8]<-'soilval'
#colnames(data3.2)

# ## Weigh soilvals by total plant biomass, replace Empty pots with NA here
# data3.2$soilval.totbiom <- data3.2$soilval / data3.2$total
# data3.2[data3.2$type=='Empty','soilval.totbiom'] <- NA
# #View(data3.2)


## Subset data
data3.2$soilmeas <- factor(data3.2$soilmeas, levels = c('nhdi', 'nodi', 'totdi', 'ammonifd', 'nitrifd','minzd','soilmoi'))
data3.2$type <- factor(data3.2$type, levels = c('Empty', 'Mivi', 'Pavi', 'Sobi', 'CompEmpty', 'CompSobi', 'CompPavi'))

## A. Plot monoculture type vs soil value in facets
sub1<-subset(data3.2, type == 'Mivi' | type == 'Empty' | type == 'Pavi' | type == 'Sobi')
fig3a <- ggplot(sub1, aes(x=type, y=soilval)) + 
  mytheme +
  ggtitle('a') +
  ylab('Soil measurement') +
  xlab('Monoculture type') +
  facet_grid( soilmeas ~. , scales='free') +
  stat_summary(fun.y = mean, geom='point', size=3) +
  stat_summary(fun.data = mean_cl_normal, geom = 'errorbar', mult = 1, width=0.2)
#fig3a


## B. Plot M.v. biomass vs soil value in facets for comptrt=N
sub1<-subset(data3.2, type == 'Mivi' | type == 'Empty' | type == 'CompEmpty')

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

# All panels

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

ggsave(filename="fig3.pdf", plot=fig3, width = 6, height = 12, units = 'in') #save the plot and define its size














