#e4_makeFig3_basePlots.R
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
##################################
#FXN: GENERATES A SEQUENCE BY A MULTIPLE OF YOUR CHOICE FOR A RANGE OF VALUES
GenerateAts<-function(lim.temp, multiple){
  yat.temp<-seq.int(lim.temp[1],lim.temp[2], by=multiple)
  return(yat.temp)
}

##################################
##################################
### Set up Figure Panels ###
tiff(filename='e4Output_figures/fig3.tiff', 
    width=7.5, # 7.5inches
    height=8, # 8inches 
    units="in", res=400, compression='lzw')
#quartz()
#dev.size(units = "cm")
#dev.off()
##################################
#PANEL ORDER
# 2 columns and 4 rows of panels where both columns have just 1 xaxis and the 2 xaxes are the same
measorder<-c('soilmoi','total',
             'nhdi','nodi','totdi',
             'ammonifd','nitrifd','minzd') # Build the panels so that measurements follow in this order
nrows<-4
ncols<-3
mat<-matrix(c(1,2,0,
              3,4,5,
              6,7,8,
              9,9,9),
            nrows, #nrow
            ncols, #ncol
            byrow = TRUE) #order that the panels are added
letters<-c("A","B","C","D","E","F","G","H")

#FIGURE DIMENSIONS
w.norm<-5.08 #2 inches
w<-w.norm*ncols
w # 6inches

h.norm<-5.08 # 2 inches
h4<-1.27 #0.5inch
h<-h.norm*nrows + h4
h # 8.5inches

#PANEL SETUP
l1<-layout(mat,
           widths=c(lcm(w.norm),lcm(w.norm), lcm(w.norm)), #based on ncol #
           heights=c(lcm(h.norm), lcm(h.norm), lcm(h.norm), lcm(h4)) #based on nrow #
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

##################################
#SET UP DATAFRAMES
##################################
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

##################################
#TUKEY LETTERS
##################################
#based on 'edited e4figures_rev.pptx in e4_repository/oldVersions/R_figures/edited
tuk.soilmoi<-c('a','a','a','b')
tuk.total<-c('a','b','b','c') 
tuk.nh<-c('a','ab','ab','b')
tuk.no<-c('a','b','bc','c')
tuk.tot<-c('a','b','bc','c')
tuk.ammon<-c('','','','')
tuk.nitrif<-c('','','','')
tuk.minz<-c('','','','')
tuks<-list(tuk.soilmoi,tuk.total,
     tuk.nh,tuk.no,tuk.tot,
     tuk.ammon,tuk.nitrif,tuk.minz)
tukbuff<-c(5, #soilmoi
           8, #total
           rep(10, 3), # n pools
           rep(1, 3)) # n fluxes

##################################
#SET UP Y AXES
##################################
#ynames
row1yname.sm<-"Soil moisture (%)"
row1yname.tot<-"Total biomass (g)"
row2yname1<-"Inorganic N"
row2yname2<-expression(paste("(",mu,"gN/G)"))
row3yname1<-"Net N mineralization"
row3yname2<-expression(paste("(",mu,"gN/G*d)"))
shortynames<-c('Soil moisture','Total biomass',
               'Ammonium','Nitrate','Total inorg. N',
               'Ammonification','Nitrification','Mineralization')
cex.shortyn<-0.9

#ylims
ylim.soilmoi <- c(50,100)
ylim.total <- c(0,100)
ylim.nh <- c(0,120)
ylim.no <- c(0,120)
ylim.tot <- c(0,120)
ylim.ammon <- c(-3,4.5)
ylim.nitrif <- c(-3,4.5)
ylim.minz <- c(-3,4.5)
ylimlist<-list(ylim.soilmoi, ylim.total,
            ylim.nh, ylim.no, ylim.tot,
            ylim.ammon, ylim.nitrif, ylim.minz)

#yat
multiplelist<-list(20, #soilmoi
                   20, #total
                   20,  #nh
                   20,  #no
                   20,  #tot
                   2,  #ammonif
                   2,  #nitrif
                   2)  #minz
yatlist<-list()
i<-0
for (i in 1:length(ylimlist)){
  yatlist[[i]]<-GenerateAts(ylimlist[[i]], multiplelist[[i]])
}

##################################
#SET UP LOOP, WILL LOOP THROUGH EACH MEASURE TO ADD EACH PANEL
par(mar=c(0,0,0,0)) #c(bottom, left, top, right) 
i<-0
for (i in 1:length(measorder)){
  
    #SETUP PLOT
    plot(soilval~as.integer(type), df, axes = FALSE, type = "n", 
         xlab='', ylab='',
         xlim=xlims,
         ylim=ylimlist[[i]])
    box()
    
    #PANEL LABELS
    mtext(letters[i], side=3, adj=0.05, padj=1.8, outer = F, cex = 1, font=2)
    if(i!=1 & i!=2){
      mtext(shortynames[i], side=3, adj=0.95, line=-1.5, cex=cex.shortyn) 
    }
    
    #X AXIS
    if(i==1 | i==2 | i==3 | i==4 | i==5){
      axis(1, at = xat1, labels=FALSE, tick=FALSE, las=2)
    }
    if(i==6 | i==7 | i==8){
      axis(1, at = xat1, labels=xnames, tick=TRUE, las=2)
    }
    
    #Y AXIS
    if(i==1){
      axis(2, at = yatlist[[i]])
      mtext(row1yname.sm, side = 2, outer = F, cex = 1, line=2)
    }
    if(i==2){
      axis(4, at = yatlist[[i]])
      mtext(row1yname.tot, side = 4, outer = F, cex = 1, line=2.5)
    }
    if(i==3){
      axis(2, at = yatlist[[i]])
      mtext(row2yname1, side = 2, outer = F, cex = 1, line=3.5) 
      mtext(row2yname2, side = 2, outer = F, cex = 1, line=2) 
    }
    if(i==6){
      axis(2, at = yatlist[[i]])
      mtext(row3yname1, side = 2, outer = F, cex = 1, line=3.5) 
      mtext(row3yname2, side = 2, outer = F, cex = 1, line=2) 
    }
    if(i==4 | i==5 |i==7 | i==8){
      axis(2, at = yatlist[[i]], labels=FALSE, tick=FALSE)
    }
    
    #ADD DATA
    dfsub<-df.list[[i]]
    #add means
    points(y=dfsub$mean,x=x, pch=16)
    #add se bars
    xse<-x
    y0se<-dfsub$mean + dfsub$se
    y1se<-dfsub$mean - dfsub$se
    arrows(xse, y0se, xse, y1se, angle=90, code=3, length=0)
    
    #ADD TUKEY LETTERS
    text(x=x, y=dfsub$mean + dfsub$se + tukbuff[i],
         labels=tuks[[i]])
    
}

#PANEL FOR THE X AXIS TITLE
par(mar=c(0,0,0,0))
plot(soilval~as.integer(type), df, axes = FALSE, type = "n", 
     xlab='', ylab='',
     xlim=xlims,
     ylim=ylimlist[[i]])
mtext(xname.line1, side=1, line=4.5, adj=0.55)


##################################
## FINISH
dev.off()
