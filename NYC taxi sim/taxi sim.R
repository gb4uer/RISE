setwd('C:/Users/Gordon/Box Sync/AV')
load('tx_clean.RData') # includes trip data, relocation matrices, and charger distribution list
# trip data names: day of month, pickup time as minute of day, pickup point, dropoff point, duration (min), distance (miles), max distance from dropoff to charge, range needed to serve trip and get to charger, ID
# ggTime: list of 24 matrices, each with time (minutes) from each pickup (rows) to dropoff (columns)
# ggDist: one matrix, same as above but with distance (miles)
# chgDtbList: list of different charger distributions. names are in format [number of chargers]x[number of locations]. each vector represents the point locations of each charger in the distribution.
# chgDistList: list with distance from each point to the nearest charger in each distribution


uniMin <- function(mx,chgbool=NULL) {
  ch <- NULL
  mx[chgbool,] <- mx[chgbool,] + 100
  
  for(col in 1:ncol(mx)) {
    ch <- c(ch, whichMinNA(mx[,col]))
    mx[ch[length(ch)],] <- NA
  }
  return(ch)
}


whichMinNA <- function(v) {
  w <- which.min(v)
  if(length(w)==0) {
    return(NA)
  } else {
    return(w)
  }
}

trpAssign <- function(trp,bat) {
  tx <- NULL
  if(nrow(trp)>0)  tx <- with(trp,data.frame(pt=pt_end,
                                             t_count=time,
                                             chg_bool=2,
                                             t_chg=0,
                                             range=pmax(bat - dist, maxChgReloc),
                                             d_pass=dist,
                                             t_pass=time,
                                             d_trprel=0,
                                             t_trprel=0,
                                             d_chgrel=0,
                                             t_chgrel=0,
                                             ID=seq.int(nrow(trp)),
                                             tripID=ID,
                                             t_wait=0))
  return(tx)
}

taxiF <- function(t,tx,chargers,chgAT,bat,chgSpd=0.5,waitMax=10) {
  # Debugging inputs:
  # t <- 1;tx <- fleet[[1]]; chargers <- chg_avail[[1]][,1]; chgAT <- chg_avail[[1]][,2]; chgSpd=0.5;bat=100;reloc=T

  h <- floor(t/60) %% 24
  trp <- trip_data[trip_data$t_start==t,]
  tx <- tx[order(tx$range,decreasing = T),] # give priority to vehicles with more range
  tx2 <- NULL
  tx$t_count <- tx$t_count - 1 #update time counter
  tx$chg_bool[tx$t_count==0] <- 0 #available to charge
  avail <- tx$t_count<=10
  txA <- tx[avail,]
  
  #Routing taxis in each minute-----------------------------------------
  if(nrow(txA)>0) {
    
    nTx <- sum(avail)
    nTrp <- nrow(trp)
    ptT <- txA$pt
    ptP <- trp$pt_start
    
    # Creat matrices for times and distances between taxis and passengers
    tMx <- ggTime[[h+1]][ptT,ptP] 
    dMx <- ggDist[ptT,ptP] 
    
    if(is.null(dim(tMx))) {
      tMx <- matrix(tMx,ncol=nTrp)
      dMx <- matrix(dMx,ncol=nTrp)
    } 
    
    #Determine amount of range that should be subtracted due to non-charging
    fullChg <- txA$chg_bool
    tChg <- matrix(txA$t_chg,nrow(txA),nrow(trp))
    fullChg[fullChg>0] <- 0
    
    falseChg <- (tMx - waitMax)*chgSpd + fullChg - chgSpd
    falseChg[falseChg<0] <- 0
    falseChg[falseChg>tChg] <- tChg[falseChg>tChg]
    
    
    #Identify reserve required for each trip
    
    bat.lim <- txA$range - matrix(trp$chgDist,nrow=nTx,ncol=nTrp,byrow=T) - falseChg
    
    # Take out unfeasible combinations
    tMx[tMx + txA$t_count>waitMax] <- NA
    tMx[dMx>bat.lim] <- NA
    
    naMx <- !is.na(tMx)
    
    if(T %in% naMx) {
      
      feasTrp <- which(colSums(naMx)!=0)
      feasTx <- which(rowSums(naMx)!=0)
      
      tMx <- tMx[feasTx,feasTrp] 
      if(is.null(dim(tMx))) tMx <- matrix(tMx,ncol=nTrp)
      
      #Remove duplicate assignments
      chgbool <- txA$chg_bool[feasTx]==1
      ch0 <- uniMin(tMx,chgbool)
      
      feasTrp <- feasTrp[!is.na(ch0)]
      dMx <- dMx[feasTx,feasTrp]
      falseChg <- falseChg[feasTx,feasTrp]
      
      tMx <- tMx[,!is.na(ch0)]
      ch0 <- ch0[!is.na(ch0)]
      
      if(is.null(dim(tMx))) {
        reloc.T <- tMx[ch0]
        reloc.D <- dMx[ch0]
        falseChg <- falseChg[ch0]
      } else {
        reloc.T <- diag(tMx[ch0,])
        reloc.D <- diag(dMx[ch0,])
        falseChg <- diag(falseChg[ch0,])
      }
      
      
      #Add new taxis to fulfill unmet demand
      trpN <- trp[-feasTrp,] # untaken trips designated to new taxis
      
      if(nrow(trpN)>0) {
        tx2 <- trpAssign(trpN,bat)
        trp <- trp[feasTrp,]
      }
      
      chTx <- feasTx[ch0]
      
      #Update fleet with new assignments
      tx$d_pass[avail][chTx] <- tx$d_pass[avail][chTx] + trp$dist
      tx$t_pass[avail][chTx] <- tx$t_pass[avail][chTx] + trp$time
      tx$d_trprel[avail][chTx] <- tx$d_trprel[avail][chTx] + reloc.D
      tx$t_trprel[avail][chTx] <- tx$t_trprel[avail][chTx] + reloc.T
      
      #Update range
      tx$range[avail][chTx] <- tx$range[avail][chTx] -
        reloc.D -
        trp$dist -
        falseChg
      
      # Update chargers used by relocating taxis
      charging <- rep(F,length(ch0))
      charging[tx$chg_bool[avail][chTx]==1] <- T
      chargers <- c(chargers, tx$pt[avail][chTx[charging]])
      chgAT <- c(chgAT,falseChg[charging]/chgSpd)
      tx$t_chg[avail][chTx] <- 0 # Reset amount of time spent charging for active taxis
      tx$chg_bool[avail][chTx] <- 2 # Reset charging counter for active taxis
      
      tx$tripID[avail][chTx] <- trp$ID
      tx$pt[avail][chTx] <- trp$pt_end
      tx$t_count[avail][chTx][ tx$t_count[avail][chTx]<0 ] <- 0
      t_wait <- pmin(reloc.T,waitMax)
      tx$t_count[avail][chTx] <- tx$t_count[avail][chTx] + t_wait + trp$time
      tx$t_wait[avail][chTx] <- tx$t_wait[avail][chTx] + t_wait
      
    } 
    
  } else {
    tx2 <- trpAssign(trp,bat)
  }
  
  selC <- tx$chg_bool==1 # select charging taxis
  selBat <- selC & tx$range>=bat # select charging taxis with full battery
  tx$t_chg[selC] <- tx$t_chg[selC] +  chgSpd # Increase counter for amount of charge received
  tx$chg_bool[tx$chg_bool<0] <- tx$chg_bool[tx$chg_bool<0] -  chgSpd # Decrease counter for those with full battery
  
  tx$range[selC] <- tx$range[selC] +  chgSpd # After re-assigning taxis, add chgSpdmi to those that are charging
  
  # Free up chargers when batteries fully charged
  chgAT <- chgAT + 1 # increase chargers' available time
  
  fullChg <- bat - tx$range[selBat]
  tx$chg_bool[selBat] <- fullChg
  tx$t_chg[selBat] <- tx$t_chg[selBat] + fullChg
  
  chargers <- c(chargers, tx$pt[selBat])
  chgAT <- c(chgAT,-1 * fullChg/chgSpd)
  tx$range[selBat] <- bat
  
  
  # CHARGING RELOCATION----------------
  tx <- tx[order(tx$range),]
  notcharging <- tx$chg_bool==0
  txA <- tx[notcharging,] # Assign non-charging idle taxis to txA
  
  if(nrow(txA)>0 & length(chargers)>0) {
    
    # Make matrix with distances and times to each available charger
    
    tMx <- ggTime[[h+1]][txA$pt,chargers] 
    dMx <- ggDist[txA$pt,chargers] 
    if(is.null(dim(tMx))) {
      tMx <- matrix(tMx,ncol=length(chargers))
      dMx <- matrix(dMx,ncol=length(chargers))
    } 
    
    # same size matrices for amount of time chargers have been available and max charge that could be gained, respectively
    chgTime <- matrix(chgAT,nrow(txA), length(chargers),byrow = T)
    
    maxCharge <- -1 * txA$t_count - tMx # max time spent charging by newcomer
    maxCharge[maxCharge>chgTime] <- chgTime[maxCharge>chgTime] # bounded by time charger has been available
    
    maxCharge <- maxCharge*chgSpd - dMx # amount of charge gained
    
    maxCharge[maxCharge < chgSpd | dMx > txA$range] <- NA # is it worth the trip? can i make it?
    
    naMx <- !is.na(maxCharge)
    

    if(T %in% naMx) {
      
      feasChg <- which(colSums(naMx)!=0)
      feasTx <- which(rowSums(naMx)!=0)
      
      maxCharge <- maxCharge[feasTx,feasChg] 
      if(is.null(dim(maxCharge))) maxCharge <- matrix(maxCharge,ncol=length(feasChg))
      
      #Remove duplicate assignments
      ch0 <- uniMin(-1*maxCharge)
      
      feasChg <- feasChg[!is.na(ch0)]
      dMx <- dMx[feasTx,feasChg]
      tMx <- tMx[feasTx,feasChg]
      
      maxCharge <- maxCharge[,!is.na(ch0)]
      ch0 <- ch0[!is.na(ch0)]
      
      if(is.null(dim(tMx))) {
        reloc.T <- tMx[ch0]
        reloc.D <- dMx[ch0]
        mC <- maxCharge[ch0]
      } else {
        reloc.T <- diag(tMx[ch0,])
        reloc.D <- diag(dMx[ch0,])
        mC <- diag(maxCharge[ch0,])
      }
      
      
      chTx <- feasTx[ch0]
      
      # Relocate to chargers where viable, add range owed
      tx$d_chgrel[notcharging][chTx] <- txA$d_chgrel[chTx]+reloc.D   
      tx$t_chgrel[notcharging][chTx] <- txA$t_chgrel[chTx]+reloc.T
      
      tx$range[notcharging][chTx] <- txA$range[chTx] + mC
      tx$pt[notcharging][chTx] <- chargers[feasChg]
      tx$t_count[notcharging][chTx] <- txA$t_count[chTx] + reloc.T
      tx$t_chg[notcharging][chTx] <- txA$t_chg[chTx] + (mC + reloc.D)
      tx$chg_bool[notcharging][chTx] <- 1
      
      # Mark chargers as occupied

      chargers <- chargers[-feasChg] #untaken chargers remain available
      chgAT <- chgAT[-feasChg] #untaken chargers remain available
      
      
      
    }
  }
  
  tx <- rbind(tx,tx2)
  return(list(tx,chargers,chgAT))
}


taxiSim <- function(bat, chgdtb=length(chgDtbList), power=7,kpm=0.25,simsave=F,file=NULL,time_start=0,time_stop=1440,start=1,end=1) {
  # bat = 200; chgdtb = 1; chgSpd = 0.75; start=1; file=NULL; simsave=F; time_stop=1440; end=1
  
  chargers <- chgDtbList[[chgdtb]]
  chgAT <- rep(100,length(chargers))
  
  trp0 <- trip_data[trip_data$t_start==0,]
  tx <- trpAssign(trp0,bat)
  chgtot=sum(tx$chg_bool==1)
  t <- 1
  sim <- NULL
  soc_0 <- 1
  
  if(simsave) {
    fleet <- list(tx)
    chg_avail <- list(cbind(chargers,chgAT))
  }
  
  runtime <- 0
  chgSpd = power/ kpm /60
  while(t<=time_stop & nrow(tx)<10000) {
    
    if(t%%60==0) {
      
      sim <- rbind(sim,data.frame(tel=round(as.numeric(difftime(Sys.time(),t0,units='mins')),2),
                                  hr=floor(t/60),
                                  bat=bat,
                                  chgdtb=names(chgDtbList)[chgdtb],
                                  power=power,
                                  ntx=nrow(tx),
                                  ntrp=sum(trip_data$hour==floor(t/60)),
                                  avg_nchg=round(chgtot/60),
                                  avg_range=round(mean(tx$range),2),
                                  d_trprel=round(mean(tx$d_trprel),2),
                                  t_trprel=round(mean(tx$t_trprel),2),
                                  d_chgrel=round(mean(tx$d_chgrel),2),
                                  t_chgrel=round(mean(tx$t_chgrel),2),
                                  wait=round(sum(tx$t_wait)/sum(trip_data$t_start<=t),2)))
      chgtot=0
      
      print(sim[nrow(sim),])
      if(!is.null(file)) write.csv(sim,paste0(paste(file,bat,chgdtb,power,sep='_'),'.csv'))
      
    }
    
    result <- taxiF(t,tx,chargers,chgAT,bat,chgSpd = chgSpd)
    tx <- result[[1]]
    chargers <- result[[2]]
    chgAT <- result[[3]]
    chgtot <- chgtot+sum(tx$chg_bool==1)
    
    if(simsave) {
      fleet <- c(fleet, list(tx))
      chg_avail <- c(chg_avail,list(cbind(chargers,chgAT)))
      assign('fleet',fleet,envir = .GlobalEnv)
      assign('chg_avail',chg_avail,envir = .GlobalEnv)
    }
    
    if(t==1440 & soc_0 - mean(tx$range)/bat > 0.05) {
      t <- 1
      soc_0 <- mean(tx$range)/bat
      runtime <- runtime + 24
    }
    
    t <- t+1
    
  }
  
  res <- data.frame(runtime = round(t/60) + runtime,
                    bat = bat,
                    chgdtb = names(chgDtbList)[chgdtb],
                    power = power,
                    ntrp = nrow(trip_data),
                    dist_tot = sum(trip_data$dist), 
                    peak = max(summary.factor(trip_data$t_start)),
                    ntx = nrow(tx),
                    d_trprel = sum(tx$d_trprel),
                    t_trprel = sum(tx$t_trprel),
                    d_chgrel = sum(tx$d_chgrel),
                    t_chgrel = sum(tx$t_chgrel),
                    d_pass = sum(tx$d_pass),
                    t_pass = sum(tx$t_pass))
  return(res)
}

t0 <- Sys.time()

trip_data$maxChgReloc <- chgDistList[[1]][trip_data$pt_end]
trip_data$chgDist <- trip_data$maxChgReloc + trip_data$dist
trip_data$ID <- 1:nrow(trip_data)
simRes <- taxiSim(bat=500,chgdtb=1,power=50,file='test1',simsave = T)
rm(list=c('fleet','chg_avail'))

gc()

res_all <- NULL

for(bat in 1:15*10) {
  for(chgdtb in 1:length(chgDtbList)) {
    trip_data$maxChgReloc <- chgDistList[[chgdtb]][trip_data$pt_end]
    trip_data$chgDist <- trip_data$maxChgReloc + trip_data$dist
    
    for(power in c(7,11,22,50)) {
      t0 <- Sys.time()
      
      res_all <- rbind(res_all,taxiSim(bat,chgdtb,power,file='nyc'))
      write.csv(res_all,'res_all.csv')
      
    }
    
  }
}

