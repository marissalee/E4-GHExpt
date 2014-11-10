#How do I add posthoc labels to facets?

#You'll need 'data_addToFacets.txt' to run this
#Note to myself...I pulled from the following files: e4_makeFig3.R, e4_cleanCode.R, e4_prepdfFig3n4.R


### Load data ###
data3 <- read.table('data_addToFacets.txt',header=T, sep="\t", stringsAsFactors=T)


### Load libraries ###
library(ggplot2)
library(grid)


### My plot theme ###
mytheme <- theme_bw(base_size = 10, base_family = "Helvetica") +
  theme(panel.border = element_rect(colour = "black", size=1),      #put a black box around the plotting area
        axis.line = element_line(colour = "black"),                 #axis lines are in black
        panel.grid.major = element_blank(),                         #turn off the gridlines
        panel.grid.minor = element_blank()) +
  theme(strip.text.x = element_text(face='italic', hjust=0.05),         #turn off the x axis facet labels
        strip.text.y = element_text(face='italic', hjust=0.05)) +      #make y axis facet labels be italic and top justified
  theme(legend.key = element_blank(),                               #turn off box around legend
        plot.title=element_text(hjust=-0.1, vjust=2, face='bold')) +#style and position of the panel label
  theme(axis.title.x=element_text(vjust=0),                        #make more room for the x and y axis labels
        axis.title.y=element_text(vjust=0),
        plot.margin=unit(c(0.1,0.1,0.1,0.1),"inches"))


### Order the factors a logical way and make prettier labels for soil measurements###
data3$soilmeas <- factor(data3$soilmeas, levels = c('nhdi', 'nodi', 'totdi', 'ammonifd', 'nitrifd','minzd','soilmoi'))
data3$type <- factor(data3$type, levels = c('Empty', 'Mivi', 'Pavi', 'Sobi', 'CompEmpty', 'CompSobi', 'CompPavi'))
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


### Prep dataframe ###
sub1<-subset(data3, type == 'Mivi' | type == 'Empty' | type == 'Pavi' | type == 'Sobi')


### Plot plant type vs soil value in facets ###
fig3a <- ggplot(sub1, aes(x=type, y=soilval)) + 
  mytheme +
  ggtitle('a') +
  ylab('Soil measurement') +
  xlab('Monoculture type') +
  facet_grid( soilmeas ~. , scales='free', labeller=soilmeas_labeller) +
  stat_summary(fun.y = mean, geom='point', size=2) +
  stat_summary(fun.data = mean_cl_normal, geom = 'errorbar', mult = 1, width=0.2, size=0.5)
fig3a
#ammend dataframe to include posthoc letters .. I don't know how to do this on a faceted plot yet, but see how I did it on 1 plot, below...


### Plot plant type vs total biomass in 1 plot ###
fig3b <- ggplot(sub1, aes(x=type, y=total)) + 
  mytheme +
  ggtitle('b') +
  ylab('Total dry above-\nground biomass (g)') +
  xlab('Monoculture type') +
  stat_summary(fun.y = mean, geom='point', size=2) +
  stat_summary(fun.data = mean_cl_normal, geom = 'errorbar', mult = 1, width=0.2, size=0.5)
fig3b

#ammend dataframe to include posthoc letters
labels <- c('a','b','b','c')
addlabels <- function(x) {
  labpos <- mean(x) + 8
  names(labpos) <- c("y") #this is what ggplot is expecting
  return (labpos)
}
fig3b.wletters <- fig3b + stat_summary(fun.data = 'addlabels', geom='text', label=labels)
fig3b.wletters




