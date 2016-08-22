#e4_fitmodCode.R
#Set up model fxns, Set up fxn to summarize fitted models, Fit the models, 
#Make summary table, Make coef tables

#+ Set up model fxns  
# # 1. sF = (mivi * beta)
# # MODEL 1
# ModFxn1 <- function(df){h<- lm(sF ~ mivi, data=df)}

# 2. sF = (mivi * beta) + (compabund * beta2) + ((mivi*compabund) * beta3)
# MODEL 2
ModFxn2 <- function(df){h<- lm(sF ~ mivi * compabund, data=df)}

# MODEL 3
# s1s0 = (total * beta)
ModFxn3 <- function(df){h<- lm(sF ~ total, data=df)}


#+ Set up generic fxn to pull out info from each fitted model 
# FUNCTION TO PULL OUT SUMMARY STATS, CREATED WTIH ModFxn(data)
ModInfo<-function(results){
  r2 <- summary(results)$r.squared
  degf <- summary(results)$df
  fstat <- summary(results)$fstatistic
  pval <- summary(results)$coefficients[,"Pr(>|t|)"] # pvals for each term
  est <- coef(results) # coef for each term
  se <- sqrt(diag(vcov(results))) # standard error
  modinf<-list(r2=r2,degf=degf,fstat=fstat,pval=pval,est=est,se=se)
}


#+ Fit the models  
#initialize counters
SOILMEAS <- unique(datas2$scol)
i <- 0

COMPTRT <- unique(datas2$comptrt)
t <- 0

#initialize lists to store results 
#mod1list<-list()
mod2list<-list()
mod3list<-list()

for (i in 1:length(SOILMEAS)){
  for (t in 1:length(COMPTRT)){
    #name this iteration of the loop
    iter <- paste(SOILMEAS[i], COMPTRT[t], sep='_')
    
    #subset the data
    tmpdat<-datas2[datas2$scol==SOILMEAS[i] & datas2$comptrt==COMPTRT[t],]
    
    #run models
    #mod1<-ModInfo(ModFxn1(tmpdat))
    mod2<-ModInfo(ModFxn2(tmpdat))
    mod3<-ModInfo(ModFxn3(tmpdat))
    
    #store the results from each model in a list
    #mod1list[[as.character(iter)]] <- mod1
    mod2list[[as.character(iter)]] <- mod2
    mod3list[[as.character(iter)]] <- mod3
    
  }
}

str(mod2list)


#+ Organize fitted model results into tables  
# Make this list into a dataframe with each soil treatment/comptrt as a row and each model info as a column

# FUNCTION TO DETERMINE LIST DIMENSIONS, LIST CREATED WTIH ModInfo(ModFxn(data))
ListDims <- function(alist){
  
  # determine the dimensions of this list
  # Level1
  lev1len<-length(alist)
  
  # Level 2: This level has unique compartments for each modinf: r2, degf, fstat, pval, est, se
  # Level 3: This level has unique compartments for each element within a modinf category
  u<-0
  lev2len.store<-numeric(0)
  lev3len.store<-numeric(0)
  for (u in 1:length(alist)){
    # level2
    lev2len<-length(alist[[u]]) # this should be 6
    lev2len.store<-c(lev2len.store, lev2len)
    
    #level3
    sublist<-alist[[u]]
    for (v in 1:length(sublist)){
      lev3len<-length(sublist[[v]])
      lev3len.store<-c(lev3len.store, lev3len)
    }
  }
  
  # If level 2 dimensions are correct, then make a table that holds level 3 dimensions
  if(sum(lev2len.store != 6) == 0){ # there should only be 6's here based on the ModInfo fxn...
    lev1lev2<-matrix(lev3len.store, nrow=lev1len, byrow=T) #matrix where rows = level 1, columns = level 2, elements = level 3
  } else{
    print('for some reason, there are not 6 items in each component of level 1... you need to check this out')
  }
  
  return(lev1lev2)
}

# FUNCTION TO FIX LIST DIMENSIONS
FixListDims <- function(alist, listdimtab){
  lev1len <- length(alist)
  rowMatch <- apply(listdimtab, 2, function(x) min(x) == max(x))
  
  # If they don't match, fix the modlist
  if(sum(rowMatch) != dim(listdimtab)[2]){
    notMatched <- which(rowMatch==FALSE) # pick out the col nums that are not matched
    needtobe <- apply(listdimtab[,notMatched], 2, max) # what all the row elements need to be in each unMatched col
    
    # Loop through each problem area
    # Initialize
    l1<-0
    nm<-0
    for (l1 in 1:lev1len){
      for (nm in 1:length(notMatched)){
        currlen <- length(alist[[l1]][[notMatched[nm]]]) # what is the current length of the problem area?
        difflen <- needtobe[nm] - currlen # what is the difference between the current length and the length it needs to be?
        alist[[l1]][[notMatched[nm]]] <- c(alist[[l1]][[notMatched[nm]]], rep(NA,difflen)) # add the difference in length using NAs
        newlen <- length(alist[[l1]][[notMatched[nm]]]) # calculate the new length to check yoself
        #print(paste(currlen,difflen,newlen)) #checkyoself
      }
    }
  }
  return(alist)
}

# FUNCTION TO ADD COLUMN NAMES TO TABLE, BASED ON LEVELS 2 AND 3 IN THE LIST
MakeColNames <- function(alist, listdimtab, tab){
  #category of column names from the model info, but some categories have more than one value
  colcat<-names(alist[[1]]) 
  
  #evaluate the number of values per category
  digPerColcat <- apply(listdimtab, 2, max)  
  
  #repeat category names based on the num of values per category
  colcat1<-rep(colcat, digPerColcat) 
  
  #re-number so that values follow a sequence within the same category, e.g. degf1, degf2
  o <- 0
  store <- numeric(0)
  for (o in 1:length(digPerColcat)){
    tmp <- seq(1:digPerColcat[o])
    store <- c(store,tmp)
  }
  
  #paste together the category name and the category sequence number that was created in the loop
  colcat2<-paste(colcat1,store, sep='') 
  
  return(colcat2)
}

# Now, loop through each modellist, and each level within that list to make a dataframe summarizing the fitted models
# Initialize counter
MODLIST<-list(mod2list=mod2list,mod3list=mod3list)
y<-0

# Initialize storage list
newmodlist<-list()

for (y in 1:length(MODLIST)){
  # Identify current modlist
  modlist <- MODLIST[[y]]
  
  # Determine the dimensions of this list
  lev1lev2 <- ListDims(modlist)
  
  # Check to see if the level2 * level3 lengths match across rows so that the list can be unlisted. If they don't match, fix the modlist
  modlist <- FixListDims(modlist, lev1lev2)
  
  # Unlist modlist where rows = level1 and cols = level2 * level3
  table<-matrix(data=unlist(modlist), nrow=length(modlist), byrow=T)
  
  # Add row names to the final table
  row.names(table)<-names(modlist) 
  
  # Make column names and add them to the final table
  columnams <- MakeColNames(modlist, lev1lev2, table)
  colnames(table)<-columnams #assign these column names
  
  #store final table in new list
  iter<-names(MODLIST[y])
  newmodlist[[as.character(iter)]]<-table
}

str(newmodlist)


#+ View the fitted model results
#modl1<-data.frame(newmodlist[[1]])
modl2<-data.frame(newmodlist[[2]])
modl3<-data.frame(newmodlist[[3]])

# FUNCTION TO SUMMARIZE SIGNIFICANT BETAS
PvalSumm <- function(modl, alpha){
  # Identify the pval cols in modl
  pcolnum1<-grep(pattern='pval', x=colnames(modl))
  pcolnum<-pcolnum1[-1] # get rid of the intercept beta
  
  sigresults<-list()
  i <- 0
  for (i in 1:length(pcolnum)){
    sigrows<-modl[which(modl[,pcolnum[i]] < alpha),] # pval for beta associated with mivi
    sig<-row.names(sigrows)
    result<-paste(sig, round(sigrows[,pcolnum[i]], digits=4))
    sigresults[[i]]<-result
  }
  
  # Re-name the columns
  names(sigresults) <- colnames(modl)[pcolnum]
  
  return(sigresults)
}



# FUNCTION TO CREATE COEF MATRICES
MakeCoefMat <- function(modl){
  
  # Pull out the treatment ids that match these coefs
  id <- row.names(modl)
  id.mat <- matrix(unlist(strsplit(id,'_')), ncol=2, byrow=T)
  soilmeas <- id.mat[,1]
  comptrt <- id.mat[,2]
  
  # Figure out how many betas are in the model
  colnums<-grep(pattern='est', x=colnames(modl))
  
  # Gather the coefs
  coefdf<-numeric(0)
  i <- 0
  for (i in 1:length(colnums)){
    coefcol<-modl[,colnums[i]]
    coefdf<-cbind(coefdf, coefcol)
  }
  
  # Start the new dataframe
  coefs<-data.frame(soilmeas=soilmeas, comptrt=comptrt)
  
  # Plug in the coefs
  i <- 0
  for (i in 1:length(colnums)){
    coefs[,i+2] <- coefdf[,i]
  }
  
  # Re-name the columns
  colnames(coefs) <- c('soilmeas', 'comptrt', colnames(modl)[colnums])
  
  return(coefs)
}

