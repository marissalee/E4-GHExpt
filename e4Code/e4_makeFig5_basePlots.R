#e4_makeFig5_basePlots.R
#Make Figure 5: Mixture plant biomass vs soil measure (sF) - Mivi biomass

##################################
### Load libraries ###
#none

##################################
### Prep dataframe ###
source('e4Code/e4_prepdfFig5.R')
#str(data4)
data5<-data4 #just switch names

##################################
### Functions ###
##################################
#FXN: CONSOLIDATE PRED, ATTACH TO DATAFRAME
PredAttach<-function(sub,lme.fit){
  soilnames<-names(lme.fit)
  i<-0
  preddftabs<-list()
  for (i in 1:length(lme.fit)){preddftabs[[as.character(soilnames[i])]]<-lme.fit[[i]]$preddf}
  newsub<-ldply(preddftabs, data.frame)
  
  return(newsub)
}

#FXN: PULL OUT PVALS FOR EACH FIXED EFFECT
PullPvals<-function(lmeobj){
  #Make pval labels
  thing<-SoilUnlist(lmeobj)$anovatab
  terms<-row.names(thing[[1]])
  SOILMEAS<-names(thing)
  s <- 0
  store<-numeric(0)
  #storage containers
  for (s in 1:length(SOILMEAS)){
    pvals<-thing[[SOILMEAS[s]]]$'Pr..F.'
    store<-rbind(store, pvals)
  }
  colnames(store)<-terms
  result<-data.frame(soilmeas=SOILMEAS, store)
  
  i<-0
  result2<-result
  for (i in 1:length(terms)){
    ns <-which(result2[,i+1] >= 0.1)
    s.<-which(result2[,i+1] < 0.1 & result2[,i+1] > 0.05)
    st<-which(result2[,i+1] <= 0.05 & result2[,i+1] > 0.01)
    stt<-which(result2[,i+1] <= 0.01)
    
    result2[ns, i+1] <- 'ns'
    result2[s., i+1] <- '.'
    result2[st, i+1] <- '*'
    result2[stt, i+1] <- '**'
  }
  
  return(result2)
}

#FXN: NAME PVALS BASED ON SIGNIFICANCE LEVEL
NamPval<-function(result,terms){
  SOILMEAS<-result$soilmeas
  i <- 0
  store<-numeric(0)
  result2<-result
  #storage containers
  for (i in 1:length(SOILMEAS)){
    vec<-result2[result2$soilmeas==SOILMEAS[i],-1]
    labelvec<-paste(terms, vec)
    store<-rbind(store, labelvec)
  }
  store1<-data.frame(soilmeas=SOILMEAS,store)
  store1
  return(store1)
}

#FXN: GENERATES A SEQUENCE BY A MULTIPLE OF YOUR CHOICE FOR A RANGE OF VALUES
GenerateAts<-function(lim.temp, multiple){
  yat.temp<-seq.int(lim.temp[1],lim.temp[2], by=multiple)
  return(yat.temp)
}

##################################
##################################
### Set up Figure Panels ###
png(filename='e4Output_figures/fig5.png', 
    width=19.05, # 7.5inches
    height=20.32, # 8inches 
    units="cm", res=300)
#quartz()
#dev.size(units = "cm")
#dev.off()
##################################
#PANEL ORDER
# 3 columns and 4 rows of panels where both columns have just 1 xaxis and the 2 xaxes are the same
measorder<-c('soilmoi',
             'nhdi','nodi','totdi',
             'ammonifd','nitrifd','minzd') # Build the panels so that measurements follow in this order
mat<-matrix(c(1,0,0,
              2,3,4,
              5,6,7,
              8,8,8),
            4, #nrow
            3, #ncol
            byrow = TRUE) #order that the panels are added
letters<-c("A","B","C","D","E","F","G")

#FIGURE DIMENSIONS
w.norm<-5.08 # 2 inches
w<-w.norm*3
w # 6 inches
h.norm<-5.08 # 2 inches
h4<-1.27 #0.5inch
h<-h.norm*nrows + h4
h # 8.5 inches

#PANEL SETUP
l1<-layout(mat,
           widths=c(lcm(w.norm),lcm(w.norm), lcm(w.norm)), #based on ncol #
           heights=c(lcm(h.norm), lcm(h.norm), lcm(h.norm), 
                     lcm(h4)) #based on nrow #
) 
layout.show(l1) #number of panels

#UNIVERSAL X-AXIS
xlims<-c(0,50)
xat1<-GenerateAts(xlims, 10)

##################################
### Figure 5. Mivi vs soilmeasures, shapes are comptrt ###

##################################
#SET UP DATAFRAMES
##################################
sub1<-subset(data5, biommeas == "mivi") #subset data
sub2<-PredAttach(sub1, lme.fig5.mivi) #update dataframe with predicted values

##################################
#ATTRIBUTES OF GROUPS
##################################
NEIGHTRT<-unique(sub2$comptrt)
pchs<-c(16,17,15)
ltys<-c(1,2,1)
colors<-c("darkgray","black","black")

##################################
#SET UP Y AXES
##################################
#ynames
row1yname<-"Soil moisture (%)"
row2yname1<-"Inorganic N"
row2yname2<-expression(paste("(",mu,"gN/G)"))
row3yname1<-"Net N mineralization"
row3yname2<-expression(paste("(",mu,"gN/G*d)"))
shortynames<-c('Soil moisture',
               'Ammonium','Nitrate','Total inorg. N',
               'Ammonification','Nitrification','Mineralization')
cex.shortyn<-0.9

#ylims
ylim.soilmoi <- c(55,100)
ylim.nh <- c(0,140)
ylim.no <- c(0,140)
ylim.tot <- c(0,140)
ylim.ammon <- c(-10,20)
ylim.nitrif <- c(-10,20)
ylim.minz <- c(-10,20)
ylimlist<-list(ylim.soilmoi,
               ylim.nh, ylim.no, ylim.tot,
               ylim.ammon, ylim.nitrif, ylim.minz)

#yats
multiplelist<-list(10, #soilmoi
                   10,  #nh
                   10,  #no
                   10,  #tot
                   2,  #ammonif
                   2,  #nitrif
                   2)  #minz
yatlist<-list()
i<-0
for (i in 1:length(ylimlist)){
  yatlist[[i]]<-GenerateAts(ylimlist[[i]], multiplelist[[i]])
}

##################################
#LME MODEL FIXED EFFECT P VALS
##################################
cex.terms<-0.8
terms<-c('mv','mv2','neightrt','mv:neightrt')
reorder<-c(2,3,4,5,6,7,1)
mtxline<-c(-1.5,-2.5,-3.5,-4.5,-5.5) #for positioning mtext labels in the upper right corner of panels
result<-PullPvals(lme.fig5.mivi) #ignore warning message, pull pvals
df.aux<-NamPval(result, terms)
colnames(df.aux)[2:5]<-c('line1','line2','line3','line4')
df1.aux<-data.frame(reorder, df.aux)
df2.aux<-orderBy(~reorder, df1.aux)

##################################
## Base plot
#SET UP LOOP, WILL LOOP THROUGH EACH MEASURE TO ADD EACH PANEL
i<-0
for (i in 1:length(measorder)){
  par(mar=c(0,0,0,0)) #c(bottom, left, top, right) # side plot
  
  #SUBSET DATA BASED ON SOILMEAS
  df<-sub2[sub2$soilmeas == measorder[i],]
  df1<-orderBy(~biomval, data=df) # need to order by x vals so that regession lines work
  
  #SETUP PLOT
  plot(sF~biomval, df1, axes = FALSE, type = "n", 
       xlab='', ylab='',
       xlim=xlims,
       ylim=ylimlist[[i]])
  box()
  
  #PANEL LABELS
  mtext(letters[i], side=3, adj=0.05, padj=1.8, outer = F, cex = 1, font=2)
  mtext(df2.aux[i,'line1'], side=3, adj=0.95, line=mtxline[2],cex=cex.terms) #mv
  mtext(df2.aux[i,'line2'], side=3, adj=0.95, line=mtxline[3],cex=cex.terms) #mv2
  mtext(df2.aux[i,'line3'], side=3, adj=0.95, line=mtxline[4],cex=cex.terms) #neightrt
  mtext(df2.aux[i,'line4'], side=3, adj=0.95, line=mtxline[5],cex=cex.terms) #mv:neightrt
  if(i!=1){
    mtext(shortynames[i], side=3, adj=0.95, line=mtxline[1], cex=cex.shortyn) # name
  }
  
  #X AXIS
  if(i==1 | i==2 | i==3 | i==4){
    axis(1, at = xat1, labels=FALSE, tick=FALSE)
  }
  if(i==5 | i==6 | i==7){
    axis(1, at = xat1, labels=TRUE, tick=TRUE)
  }
  
  #Y AXIS
  if(i==1){
    axis(2, at = yatlist[[i]], labels=TRUE, tick=TRUE)
    mtext(row1yname, side = 2, outer = F, cex = 1, line=2)
  }
  if(i==2){
    axis(2, at = yatlist[[i]], labels=TRUE, tick=TRUE)
    mtext(row2yname1, side = 2, outer = F, cex = 1, line=3.5)
    mtext(row2yname2, side = 2, outer = F, cex = 1, line=2)
  }
  if(i==5){
    axis(2, at = yatlist[[i]], labels=TRUE, tick=TRUE)
    mtext(row3yname1, side = 2, outer = F, cex = 1, line=3.5)
    mtext(row3yname2, side = 2, outer = F, cex = 1, line=2)
  }
  if(i==3 | i==4 | i==6 | i==7){
    axis(2, at = yatlist[[i]], labels=FALSE, tick=FALSE)
  }
  
  #ADD DATA - loop through data subsetted by neightrt
  k<-0
  for(k in 1:length(NEIGHTRT)){
    df1.sub<-df1[df1$comptrt == NEIGHTRT[k],] #subset data by neightrt
    points(y=df1.sub$sF,x=df1.sub$biomval, pch=pchs[k], col=colors[k]) #add means
    lines(y=df1.sub$pred, x=df1.sub$biomval, col=colors[k], lty=ltys[k]) #add lme regression line
  }
  
}

#PANEL FOR THE X AXIS TITLE
xname.line1<-expression(paste("Dry aboveground ", italic(Microstegium), " biomass (g)"))
par(mar=c(0,0,0,0))
plot(sF~biomval, df, axes = FALSE, type = "n", 
     xlab='', ylab='',
     xlim=xlims,
     ylim=ylimlist[[i]])
mtext(xname.line1, side=1, line=1, adj=0.55)




##################################
## FINISH
dev.off()



