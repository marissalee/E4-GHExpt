#e4_makeFig2.R
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
### Set up Figure Panels ###

#PANEL ORDER
# Two panels stacked on top of one another, plus a long panel on the right side to hold the legend
mat<-matrix(c(1,2,3,3), #number of panels
            2, #nrow
            2, #ncol
            byrow = FALSE) #order that the panels are added

#FIGURE DIMENSIONS
w1<-11.43 #4.5 inches
w2<-3.81 #1.5 inches
w<-w1+w2
w
h<-20.32 # 8 inches
h1<-9.2 #approx 4 inches; needs to be smaller than 10.16cm to account for space that the x axis takes up in panel B
h2<-h-h1 #approx 4 inches

#PANEL SETUP
l1<-layout(mat,
       widths=c(lcm(w1),lcm(w2)), #3:1 ratio based on ncol #
       heights=c(lcm(h1),lcm(h2))) #1:1 ratio based on nrow #
layout.show(l1) #number of panels

#UNIVERSAL X-AXIS
xname.line1<-expression(italic(Microstegium))
xname.line2<-"\ndensity treatment"
xlims<-c(0,7.5)
xat1<-1:6

##################################
### Figure 2A: Plot Mv treatment vs species biomass ###
par(mar=c(0,5,1,0)) #c(bottom, left, top, right) 

#SET UP DATAFRAMES
df <- data2[ , c("mvtrt","biomval")] #same for both panels
#xy
mivi.N<-sum.fig2a.mivi$N
mivi.P<-sum.fig2a.mivi$P
mivi.S<-sum.fig2a.mivi$S
comp.P<-sum.fig2a.comp$P
comp.S<-sum.fig2a.comp$S
df.list<-list(mivi.N, mivi.P, mivi.S, comp.P, comp.S)

#ATTRIBUTES OF GROUPS
pchs<-c(16,17,15,17,15)
ltys<-c(1,1,1,2,2)
colors<-c("darkgray","black","black","black","black")

#SET UP Y-AXIS
yname<-"Species' dry above-\nground plant biomass (g)"
ylims<-c(0,100)
yat1<-seq.int(0,100, by=10)

#SET UP PLOT
plot(biomval~as.integer(mvtrt), df, axes = FALSE, type = "n", 
     xlab='', ylab='',
     xlim=xlims,
     ylim=ylims)
box()
#panel label
mtext(expression(bold(A)), side=3, adj=0.05, padj=3, outer = F, cex = 1)
#x-axis
axis(1, at = xat1, labels=FALSE, tick=TRUE)
# mtext(xname, side = 1, outer = F, cex = 1, adj= 0.55, line=2.2)
#y-axis
axis(2, at = yat1)
mtext(yname, side = 2, outer = F, cex = 1, line=2)

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
legend('topright', 
       title=expression(bold(Species)),
       c(expression(italic(Microstegium)),
         "Neighbor"),
       lty=c(1,2,2), 
       cex=.8, bty="n", inset=0.01)


##################################
### Figure 2B: Plot Mv treatment vs total biomass ###
par(mar=c(4,5,1,0)) #c(bottom, left, top, right) 

#SET UP DATAFRAMES
df <- data2[ , c("mvtrt","biomval")] #same for both panels
#xy
total.N<-sum.fig2b$N
total.P<-sum.fig2b$P
total.S<-sum.fig2b$S
df.list<-list(total.N, total.P, total.S)

#ATTRIBUTES OF GROUPS
pchs<-c(16,17,15)
ltys<-c(1,1,1)
colors<-c("darkgray","black","black")

#SET UP Y AXIS
yname<-"Total dry above-\nground plant biomass (g)"
ylims<-c(0,100)
yat1<-seq.int(0,100, by=10)

#SETUP PLOT
plot(biomval~as.integer(mvtrt), df, axes = FALSE, type = "n", 
     xlab='', ylab='',
     xlim=xlims,
     ylim=ylims)
box()
#panel label
mtext(expression(bold(B)), side=3, adj=0.05, padj=3, outer = F, cex = 1)
#x-axis
axis(1, at = xat1)
mtext(xname.line1, side = 1, outer = F, cex = 1, adj= 0.55, line=2.2)
mtext(xname.line2, side = 1, outer = F, cex = 1, adj= 0.55, line=3.2)
#y-axis
axis(2, at = yat1)
mtext(yname, side = 2, outer = F, cex = 1, line=2)

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
### Add Legends ###
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
       pch=c(16,17,15), 
       cex=.8, bty="n", inset=.1)




## Export Fig2
#ggsave(filename="e4Output_figures/fig2.pdf", plot=fig2, width = 4, height = 5, units = 'in') #save the plot and define its size




