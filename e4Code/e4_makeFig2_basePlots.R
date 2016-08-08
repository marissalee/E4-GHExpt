#e4_makeFig2_basePlots.R
#Make Figure 2: Mv density treatment vs plant biomass

##################################
### Load libraries ###
#none

##################################
### Prep dataframe ###
source('e4Code/e4_prepdfFig2.R')
#str(data2)

##################################
### Functions ###
#none

##################################
##################################
### Set up Figure Panels ###
tiff(filename='e4Output_figures/fig2.tiff', 
    width=6, #6 inches
    height=7, #7 inches
    units="in", res=400, compression='lzw')
#quartz()
#dev.size(units = "cm")
#dev.off()
##################################
#PANEL ORDER
# Two panels stacked on top of one another, plus a long panel on the right side to hold the legend
mat<-matrix(c(1,2,3,3), #number of panels
            2, #nrow
            2, #ncol
            byrow = FALSE) #order that the panels are added

#FIGURE DIMENSIONS
w.norm<-6.35 #2 inches
w2<-3.18 #1.5 inches
w<-w.norm+w2
w # 4 inches

h.norm<-5.08 #2 inches
h<-h.norm*2
h # 5 inches

#PANEL SETUP
l1<-layout(mat,
       widths=c(lcm(w.norm),lcm(w2)), #3:1 ratio based on ncol #
       heights=c(lcm(h.norm),lcm(h.norm))) #1:1 ratio based on nrow #
layout.show(l1) #number of panels

#UNIVERSAL X-AXIS
xname.line1<-expression(italic(Microstegium))
xname.line2<-"\ndensity treatment"
xlims<-c(0,8.5)
xat1<-1:6

##################################
##################################
### Figure 2A: Plot Mv treatment vs species biomass ###

##################################
#SET UP DATAFRAMES
##################################
df <- data2[ , c("mvtrt","biomval")] #same for both panels
#xy
mivi.N<-sum.fig2a.mivi$N
mivi.P<-sum.fig2a.mivi$P
mivi.S<-sum.fig2a.mivi$S
comp.P<-sum.fig2a.comp$P
comp.S<-sum.fig2a.comp$S
df.list<-list(mivi.N, mivi.P, mivi.S, comp.P, comp.S)

##################################
#ATTRIBUTES OF GROUPS
##################################
pchs<-c(16,17,0,17,0)
ltys<-c(1,1,1,2,2)
colors<-c("darkgray","black","black","black","black")

##################################
#SET UP Y-AXIS
##################################
yname<-"Species' dry above-\nground plant biomass (g)"
ylims<-c(0,100)
yat1<-seq.int(0,100, by=10)

##################################
#SET UP PLOT
par(mar=c(0,0,0,0)) #c(bottom, left, top, right) 
plot(biomval~as.integer(mvtrt), df, axes = FALSE, type = "n", 
     xlab='', ylab='',
     xlim=xlims,
     ylim=ylims)
box()

#PANEL LABELS
mtext(expression(bold(A)), side=3, adj=0.05, padj=1.8, outer = F, cex = 1)

#X AXIS
axis(1, at = xat1, labels=FALSE, tick=FALSE)
# mtext(xname, side = 1, outer = F, cex = 1, adj= 0.55, line=2.2)

#Y AXIS
axis(2, at = yat1)
mtext(yname, side = 2, outer = F, cex = 0.9, line=2)

#ADD DATA
i<-0
for (i in 1:length(df.list)){
  #subset group
  dfsub<-df.list[[i]]
  
  #add means
  points(y=dfsub$mean,x=dfsub$groupcol, pch=pchs[i], col=colors[i])
  
  #add line segments
  lines(y=dfsub$mean,x=dfsub$groupcol, lty=ltys[i], col=colors[i]) 
  
  #add se bars
  xse<-as.numeric(dfsub$groupcol)
  y0se<-dfsub$mean + dfsub$se
  y1se<-dfsub$mean - dfsub$se
  arrows(xse, y0se, xse, y1se, angle=90, code=3, length=0, col=colors[i])
}

#ADD LEGEND
legend('topright', 
       title=expression(bold(Species)),
       c(expression(italic(Microstegium)),
         "Neighbor"),
       lty=c(1,2,2), 
       cex=.8, bty="n", inset=0.01)


##################################
##################################
### Figure 2B: Plot Mv treatment vs total biomass ###

##################################
#SET UP DATAFRAMES
##################################
df <- data2[ , c("mvtrt","biomval")] #same for both panels
#xy
total.N<-sum.fig2b$N
total.P<-sum.fig2b$P
total.S<-sum.fig2b$S
df.list<-list(total.N, total.P, total.S)

##################################
#ATTRIBUTES OF GROUPS
##################################
pchs<-c(16,17,0)
ltys<-c(1,1,1)
colors<-c("darkgray","black","black")

##################################
#SET UP Y AXIS
##################################
yname<-"Total dry above-\nground plant biomass (g)"
ylims<-c(0,100)
yat1<-seq.int(0,100, by=10)

##################################
#SETUP PLOT
par(mar=c(0,0,0,0)) #c(bottom, left, top, right) 
plot(biomval~as.integer(mvtrt), df, axes = FALSE, type = "n", 
     xlab='', ylab='',
     xlim=xlims,
     ylim=ylims)
box()

#PANEL LABELS
mtext(expression(bold(B)), side=3, adj=0.05, padj=1.8, outer = F, cex = 1)

#X AXIS
axis(1, at = xat1)
mtext(xname.line1, side = 1, outer = F, cex = 1, adj= 0.55, line=2.5)
mtext(xname.line2, side = 1, outer = F, cex = 1, adj= 0.55, line=3.5)

#Y AXIS
axis(2, at = yat1)
mtext(yname, side = 2, outer = F, cex = .9, line=2)

#ADD DATA
i<-0
for (i in 1:length(df.list)){
  #subset group
  dfsub<-df.list[[i]]
  
  #add means
  points(y=dfsub$mean,x=dfsub$groupcol, pch=pchs[i], col=colors[i])
  
  #add line segments
  lines(y=dfsub$mean,x=dfsub$groupcol, lty=ltys[i], col=colors[i]) 
  
  #add se bars
  xse<-as.numeric(dfsub$groupcol)
  y0se<-dfsub$mean + dfsub$se
  y1se<-dfsub$mean - dfsub$se
  arrows(xse, y0se, xse, y1se, angle=90, code=3, length=0, col=colors[i])
}

##################################
### ADD UNIVERSAL LEGEND ###
par(mar=c(0,0,0,0)) #c(bottom, left, top, right) 
plot(biomval~as.integer(mvtrt), df, axes = FALSE, type = "n", 
     xlab='', ylab='',
     xlim=xlims,
     ylim=ylims)
legend('top', 
       title=expression(
         paste(
           bold(Neighbor), ' ',
           bold(treatment)
           )),
       c('No Neighbor',
         expression(italic(Panicum)),
         expression(italic(Sorghum))), 
       col=c(8,1,1), 
       pch=c(16,17,0), 
       cex=.9, bty="n", inset=.1)


##################################
## FINISH 
dev.off()



