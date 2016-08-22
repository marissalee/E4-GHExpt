#How do I conditionally add regression lines to facets?

#You'll need 'data_addToFacets.txt' to run this
#Note to myself...I pulled from the following files: e4_makeFig4.R, e4_cleanCode.R, e4_prepdfFig3n4.R


### Load data ###
data3 <- read.table('data_addToFacets.txt',header=T, sep="\t", stringsAsFactors=T)
fetab <- read.table('fig4_lme_fe.txt', header=T, sep="\t", stringsAsFactors=T)


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
sub1<-subset(data3, type == 'Mivi' | type == 'Empty' | type == 'CompEmpty')

### Plot model predict lines ###

# Run the model
### Fit LME model ###
# resp <- mivi + I(mivi^2) + (1|bk)
library(lme4)
library(lmerTest)
SOILMEAS<-unique(sub1$soilmeas)#loop through each soil measure
s <- 0
store <-numeric(0)
for (s in 1:length(SOILMEAS)){ 
  df <- subset(sub1, soilmeas == SOILMEAS[s]) #subset data by soil measure
  model <- lmer(soilval ~ mivi + I(mivi^2) + (1|bk), data = df)  #run the full model with lmerTest active
  keep<-predict(model, re.form=NA)
  store<-c(store,keep)
}
store
length(store)
dim(sub1)
sub1$model.predict <- store


# Plot
fig4 <- ggplot(sub1, aes(x=mivi, y=soilval)) + 
  mytheme +
  ylab('Soil measurement') +
  xlab('Dry aboveground\nMicrostegium biomass (g)') +
  facet_grid( soilmeas ~. , scales='free', labeller=soilmeas_labeller) +
  geom_point(size=1.5) +
  geom_line(data=sub1, aes(x=mivi, y=model.predict))

fig4






