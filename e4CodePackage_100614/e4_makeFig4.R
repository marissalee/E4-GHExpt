#e4_makeFig4.R
#Make Figure 4. Mivi vs soilval for comptrt == N

### Load libraries ###
library(ggplot2)
library(reshape2)
library(grid)
library(gridExtra)


### My plot theme ###
source('e4CodePackage_100614/mytheme.R')


### Prep dataframe ###
source('e4CodePackage_100614/e4_prepdfFig3n4.R')
#str(data3)



### Order the factors a logical way ###

data3$soilmeas <- factor(data3$soilmeas, levels = c('nhdi', 'nodi', 'totdi', 'ammonifd', 'nitrifd','minzd','soilmoi'))
data3$type <- factor(data3$type, levels = c('Empty', 'Mivi', 'Pavi', 'Sobi', 'CompEmpty', 'CompSobi', 'CompPavi'))



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




### FIGURE 4. Plot M.v. biomass vs soil value in facets for comptrt=N ###

sub1<-subset(data3, type == 'Mivi' | type == 'Empty' | type == 'CompEmpty')

fig4 <- ggplot(sub1, aes(x=mivi, y=soilval, shape=as.factor(mvtrt))) + 
  mytheme +
  ylab('Soil measurement') +
  xlab('Dry aboveground\nMicrostegium biomass (g)') +
  facet_grid( soilmeas ~. , scales='free', labeller=soilmeas_labeller) +
  scale_shape_manual(values=c(18, 16, 17, 15, 5, 6), 
                     name="M.v. density\ntreatment",
                     breaks=c("0", "1", "2", "4", "5","6"),
                     labels=c("0", "1", "2", "4", "5","6")) +
  geom_point(aes(shape=as.factor(mvtrt)), size=3)
#fig4
ggsave(filename="fig4.pdf", plot=fig4, width = 6, height = 12, units = 'in') #save the plot and define its size



