#e4_makeFig3.R
#Make Figure 3: Monoculture type vs soil measure value

##################################
### Load libraries ###
#none

##################################
### Prep dataframe ###
source('e4Code/e4_prepdfFig3n4.R')
#str(data3)

##################################
### Functions ###
#FXN: GENERATES A SEQUENCE BY A MULTIPLE OF YOUR CHOICE FOR A RANGE OF VALUES
GenerateAts<-function(lim.temp, multiple){
  yat.temp<-seq.int(lim.temp[1],lim.temp[2], by=multiple)
  return(yat.temp)
}

##################################
### Set up Figure Panels ###

#PANEL ORDER
# 2 columns and 4 rows of panels where both columns have just 1 xaxis and the 2 xaxes are the same
measorder<-c('soilmoi','total',
             'nhdi','nodi','totdi',
             'ammonifd','nitrifd','minzd') # Build the panels so that measurements follow in this order
mat<-matrix(c(1,3,4,5,9,2,6,7,8,9),
            5, #nrow
            2, #ncol
            byrow = FALSE) #order that the panels are added
letters<-c("A","B","C","D","E","F","G","H")

#FIGURE DIMENSIONS
w1<-7.62 #3 inches
w<-w1*2
w
h1<-6.985 # 2.75 inches
h<-h1*4 
h

#PANEL SETUP
l1<-layout(mat,
           widths=c(lcm(w1),lcm(w1)), #based on ncol #
           heights=c(lcm(h1), lcm(h1), lcm(h1), lcm(h1+2.5), lcm(1)) #based on nrow #
           ) 
layout.show(l1) #number of panels

#UNIVERSAL X-AXIS
xname.line1<-"Monoculture type"
xlims<-c(0,4.5)
xat1<-1:4
sum.fig3.soil$nhdi$groupcol #can't do 'as.numeric' on these because they are all different odd levels, so just creating my own x data
x<-1:4
xnames <- c("Empty", expression(italic(Microstegium)), 
            expression(italic(Panicum)), expression(italic(Sorghum)))

##################################
### Figure 3: Monoculture type vs soil measures and total biomass ###

#SET UP DATAFRAMES
sub1<-subset(data3, type == 'Mivi' | type == 'Empty' | type == 'Pavi' | type == 'Sobi')
df <- sub1[ , c("type","soilval")] #this doesn't include total biom, but I think it will be ok
#xy
nh<-sum.fig3.soil$nhdi
no<-sum.fig3.soil$nodi
tot<-sum.fig3.soil$totdi
ammon<-sum.fig3.soil$ammonifd
nitrif<-sum.fig3.soil$nitrifd
minz<-sum.fig3.soil$minzd
soilmoi<-sum.fig3.soil$soilmoi
total<-sum.fig3.total
df.list<-list(soilmoi, total, nh, no, tot, ammon, nitrif, minz)

#ATTRIBUTES OF GROUPS
#none

#SET UP Y AXES
#ynames
ynames <- c("Soil Moisture\n(%)", "Total biomass (g)",
           "Ammonium\n(ugN/G)","Nitrate\n(ugN/G)","Total Inorganic N\n(ugN/G)",
           "Ammonification\n(ugN/G*d)","Nitrification\n(ugN/G*d)","Mineralization\n(ugN/G*d)") #pretty ylab names
#ylims
ylim.soilmoi <- c(50,100)
ylim.total <- c(0,100)
ylim.nh <- c(0,18)
ylim.no <- c(0,120)
ylim.tot <- c(0,120)
ylim.ammon <- c(-1.5,1.7)
ylim.nitrif <- c(-3,3.5)
ylim.minz <- c(-3,3.5)
ylimlist<-list(ylim.soilmoi, ylim.total,
            ylim.nh, ylim.no, ylim.tot,
            ylim.ammon, ylim.nitrif, ylim.minz)
#yat
multiplelist<-list(10, #soilmoi
                   10, #total
                   2,  #nh
                   10,  #no
                   10,  #tot
                   1,  #ammonif
                   1,  #nitrif
                   1)  #minz
yatlist<-list()
i<-0
for (i in 1:length(ylimlist)){
  yatlist[[i]]<-GenerateAts(ylimlist[[i]], multiplelist[[i]])
}

#SET UP LOOP, WILL LOOP THROUGH EACH MEASURE TO ADD EACH PANEL
i<-0
for (i in 1:length(measorder)){
  
  if(i!=5 & i!=8){
    #SET UP THE PLOT
    par(mar=c(0,5,1,1)) #c(bottom, left, top, right) 
    plot(soilval~as.integer(type), df, axes = FALSE, type = "n", 
         xlab='', ylab='',
         xlim=xlims,
         ylim=ylimlist[[i]])
    box()
    #panel label
    mtext(letters[i], side=3, adj=0.05, padj=3, outer = F, cex = 1, font=2)
    #x-axis
    axis(1, at = xat1, labels=FALSE, tick=TRUE)
    # mtext(xname, side = 1, outer = F, cex = 1, adj= 0.55, line=2.2)
    #y-axis
    axis(2, at = yatlist[[i]])
    mtext(ynames[i], side = 2, outer = F, cex = 1, line=2)
    
    #ADD DATA
    dfsub<-df.list[[i]]
    #add means
    points(y=dfsub$mean,x=x, pch=16)
    #add se bars
    xse<-x
    y0se<-dfsub$mean + dfsub$se
    y1se<-dfsub$mean - dfsub$se
    arrows(xse, y0se, xse, y1se, angle=90, code=3, length=0)
  }
  
  if(i==5 | i==8){
    par(mar=c(6,5,1,1)) #c(bottom, left, top, right) 
    plot(soilval~as.integer(type), df, axes = FALSE, type = "n", 
         xlab='', ylab='',
         xlim=xlims,
         ylim=ylimlist[[i]])
    box()
    #panel label
    mtext(letters[i], side=3, adj=0.05, padj=3, outer = F, cex = 1, font=2)
    #x-axis
    axis(1, at = xat1, labels=xnames, tick=TRUE, las=2)
    # mtext(xname, side = 1, outer = F, cex = 1, adj= 0.55, line=2.2)
    #y-axis
    axis(2, at = yatlist[[i]])
    mtext(ynames[i], side = 2, outer = F, cex = 1, line=2)
    
    #ADD DATA
    dfsub<-df.list[[i]]
    #add means
    points(y=dfsub$mean,x=x, pch=16)
    #add se bars
    xse<-x
    y0se<-dfsub$mean + dfsub$se
    y1se<-dfsub$mean - dfsub$se
    arrows(xse, y0se, xse, y1se, angle=90, code=3, length=0)
  }
}

#panel 9
par(mar=c(0,0,0,0))
plot(soilval~as.integer(type), df, axes = FALSE, type = "n", 
     xlab='', ylab='',
     xlim=xlims,
     ylim=ylimlist[[i]])
mtext(xname.line1, side=3, line=-2, adj=0.55)


#ggsave(filename="e4Output_figures/fig3.pdf", plot=fig3, width = 3.5, height = 8.5, units = 'in') #save the plot and define its size




