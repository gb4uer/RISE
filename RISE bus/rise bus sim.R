setwd('C:/Users/Gordon/Documents/RISE bus')
load('bangalore_init.RData') # includes trip data, relocation matrices, and charger distribution points
library(dplyr)
library(SearchTrees)

whichMinNA <- function(v) {
  w <- which.min(v)
  if(length(w)==0) {
    return(NA)
  } else {
    return(w)
  }
}


modpos <- function(x,n) {
  mod <- x%%n
  if(mod==0) {
    return(n)
  } else {
    return(mod)
  }
}


vehAssign <- function(mx,chgbool=NULL,sequential=T) {
  # Debugging: mx= tMx + 200*as.numeric(dMx > bat.lim); chgbool=chgbool
  ch <- rep(NA,ncol(mx))
  mx[chgbool,] <- mx[chgbool,] + 100

  if(sequential) {
    for(i in 1:ncol(mx)) {
      sel <- whichMinNA(mx[,i])
      ch[i] <- sel
      mx[sel,] <- NA
    }
  } else {
    while(sum(!is.na(mx))) {
      sel <- whichMinNA(mx)
      ch[ceiling(sel/nrow(mx))] <- modpos(sel,nrow(mx))
      mx[modpos(sel,nrow(mx)),] <- NA
      mx[,ceiling(sel/nrow(mx))] <- NA
    }
  }
  
  return(ch)
}


vehCreate <- function(trp,bat0,maxid=0) {
  fleet <- NULL
  if(nrow(trp)>0)  fleet <- with(trp,data.frame(pt=end_pt,
                                             t_count=dur,
                                             chg_bool=2,
                                             t_chg=0,
                                             bat = bat0,
                                             range=pmax(bat0 - dist, maxChgReloc),
                                             d_pass=dist,
                                             t_pass=dur,
                                             d_trprel=0,
                                             t_trprel=0,
                                             d_chgrel=0,
                                             t_chgrel=0,
                                             ID=seq.int(nrow(trp)) + maxid,
                                             tripID=ID,
                                             t_wait=0,
                                             ntrp=0))
  return(fleet)
}


fleetRoute <- function(t,fleet,chargers,chgAT,chgutil,bat0,chgSpd=0.5,waitMax=10,drel_max=40,chgthreshold=1) {
  # Debugging inputs: t= time+1;bat0=600;chgSpd=60;waitMax=10;drel_max=1000;chgthreshold=1

  h <- floor(t/60) %% 24
  trp <- subset(trip_data,start_min==t)
  fleet <- fleet[order(fleet$range,decreasing = T),] # give priority to vehicles with more range
  veh2 <- NULL
  fleet$t_count <- fleet$t_count - 1 #update time counter
  fleet$chg_bool[fleet$t_count==0] <- 0 #available to charge
  avail <- fleet$t_count<=10
  vehA <- subset(fleet,avail)
  
  #Routing fleets in each minute-----------------------------------------
  if(nrow(vehA)>0 & nrow(trp)>0) {
    
    nveh <- sum(avail)
    nTrp <- nrow(trp)
    ptT <- vehA$pt
    ptP <- trp$start_pt
    
    # Creat matrices for times and distances between fleets and passengers
    tMx <- time_mx_list[[h+1]][ptT,ptP] 
    dMx <- dist_mx[ptT,ptP] 
    
    if(is.null(dim(tMx))) {
      tMx <- matrix(tMx,ncol=nTrp)
      dMx <- matrix(dMx,ncol=nTrp)
    } 
    
    #Determine amount of range that should be subtracted due to non-charging
    fullChg <- vehA$chg_bool
    tChg <- matrix(vehA$t_chg,nrow(vehA),nrow(trp))
    fullChg[fullChg>0] <- 0
    
    falseChg <- (tMx - waitMax)*chgSpd + fullChg - chgSpd
    falseChg[falseChg<0] <- 0
    falseChg[falseChg>tChg] <- tChg[falseChg>tChg]
    
    
    #Identify reserve required for each trip
    
    bat.lim <- vehA$range - matrix(trp$chgDist,nrow=nveh,ncol=nTrp,byrow=T) - falseChg
    
    # Take out unfeasible combinations
    tMx[tMx + vehA$t_count>waitMax | dMx > drel_max] <- NA
    # tMx[dMx>bat.lim] <- NA
    
    naMx <- !is.na(tMx)
    
    if(T %in% naMx) {
      
      feasTrp <- which(colSums(naMx)!=0)
      feasveh <- which(rowSums(naMx)!=0)
      
      tMx <- tMx[feasveh,feasTrp] 
      bat.lim <- bat.lim[feasveh,feasTrp]
      dMx <- dMx[feasveh,feasTrp]
      
      if(is.null(dim(tMx))) {
        tMx <- matrix(tMx,ncol=length(feasTrp))
        dMx <- matrix(dMx,ncol=length(feasTrp))
        bat.lim <- matrix(bat.lim,ncol=length(feasTrp))
      }
      
      #Remove duplicate assignments
      chgbool <- vehA$chg_bool[feasveh]==1
      ch0 <- vehAssign(tMx + 200*as.numeric(dMx > bat.lim),chgbool)
      
      feasTrp <- feasTrp[!is.na(ch0)]
      falseChg <- falseChg[feasveh,feasTrp]
      
      tMx <- tMx[,!is.na(ch0)]
      dMx <- dMx[,!is.na(ch0)]
      
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
      
      
      #Add new fleets to fulfill unmet demand
      trpN <- trp[-feasTrp,] # untaken trips designated to new fleets
      
      if(nrow(trpN)>0) {
        veh2 <- vehCreate(trpN,bat0,maxid=nrow(fleet))
        trp <- trp[feasTrp,]
      }
      
      chveh <- feasveh[ch0]
      
      #Update fleet with new assignments
      fleet$d_pass[avail][chveh] <- fleet$d_pass[avail][chveh] + trp$dist
      fleet$t_pass[avail][chveh] <- fleet$t_pass[avail][chveh] + trp$dur
      fleet$d_trprel[avail][chveh] <- fleet$d_trprel[avail][chveh] + reloc.D
      fleet$t_trprel[avail][chveh] <- fleet$t_trprel[avail][chveh] + reloc.T
      
      #Update range
      fleet$range[avail][chveh] <- fleet$range[avail][chveh] -
        reloc.D -
        trp$dist -
        falseChg
      
      # Update chargers used by relocating fleets
      if(sum(fleet$chg_bool[avail][chveh]==1)) {
        charging <- rep(F,length(ch0))
        charging[fleet$chg_bool[avail][chveh]==1] <- T
        
        # Update charger utilization
        ncharging_bypt <- aggregate(ID~pt,
                                    subset(fleet,chg_bool==1 & pt %in% fleet$pt[avail][chveh[charging]]),length)
        chg_tochange <- data.frame(pt=fleet$pt[avail][chveh[charging]])
        chg_tochange$nchg <- factor(chg_tochange$pt,levels=ncharging_bypt$pt)
        levels(chg_tochange$nchg) <- ncharging_bypt$ID
        chg_tochange$nchg <- as.numeric(as.character(chg_tochange$nchg))
        chg_tochange$ID <- paste(chg_tochange$pt,chg_tochange$nchg)
        while(sum(duplicated(chg_tochange$ID))) {
          chg_tochange$nchg[duplicated(chg_tochange$ID)] <- chg_tochange$nchg[duplicated(chg_tochange$ID)] - 1
          chg_tochange$ID <- paste(chg_tochange$pt,chg_tochange$nchg)
        }
        chg_tochange$ID <- factor(chg_tochange$ID,levels=chgutil$ID)
        levels(chg_tochange$ID) <- 1:nrow(chgutil)
        chg_tochange$ID <- as.numeric(as.character(chg_tochange$ID))
        chgutil$util[chg_tochange$ID] <- chgutil$util[chg_tochange$ID] + 
          fleet$t_chg[avail][chveh[charging]]/chgSpd - falseChg[charging]/chgSpd
        
        chargers <- c(chargers, fleet$pt[avail][chveh[charging]])
        chgAT <- c(chgAT,falseChg[charging]/chgSpd)
        
      }
      
      fleet$t_chg[avail][chveh] <- 0 # Reset amount of time spent charging for active fleets
      fleet$chg_bool[avail][chveh] <- 2 # Reset charging counter for active fleets
      
      fleet$tripID[avail][chveh] <- trp$ID
      fleet$pt[avail][chveh] <- trp$end_pt
      
      t_wait <- pmax(fleet$t_count[avail][chveh] + reloc.T,0)
      fleet$t_count[avail][chveh] <- t_wait + trp$dur
      fleet$t_wait[avail][chveh] <- fleet$t_wait[avail][chveh] + t_wait
      fleet$ntrp[avail][chveh] <- fleet$ntrp[avail][chveh] + 1
      
      # Update bat
      while(sum(fleet$range<0)) {
        fleet$bat[fleet$range<0] <- fleet$bat[fleet$range<0] + 50
        fleet$range[fleet$range<0] <- fleet$range[fleet$range<0] + 50
      }
      
    } 
    
  } else {
    veh2 <- vehCreate(trp,bat0,maxid = nrow(fleet))
  }
  
  # Vehicle charging----------
  selC <- fleet$chg_bool==1 # select charging vehicles
  selBat <- selC & fleet$range>=fleet$bat # select charging vehicles with full battery
  fleet$t_chg[selC] <- fleet$t_chg[selC] +  chgSpd # Increase counter for amount of charge received
  fleet$chg_bool[fleet$chg_bool<=0 & fleet$range>=fleet$bat] <- 
    fleet$chg_bool[fleet$chg_bool<=0 & fleet$range>=fleet$bat] -  chgSpd # Decrease counter for those with full battery
  
  fleet$range[selC] <- fleet$range[selC] +  chgSpd # After re-assigning fleets, add chgSpdmi to those that are charging
  
  # Free up chargers when batteries fully charged
  chgAT <- chgAT + 1 # increase chargers' available time
  
  if(sum(selBat)) {
    fullChg <- fleet$bat[selBat] - fleet$range[selBat]
    fleet$chg_bool[selBat] <- fullChg
    fleet$t_chg[selBat] <- fleet$t_chg[selBat] + fullChg
    # Update charger utilization
    ncharging_bypt <- aggregate(ID~pt,
                                subset(fleet,selBat),length)
    chg_tochange <- data.frame(pt=fleet$pt[selBat])
    chg_tochange$nchg <- factor(chg_tochange$pt,levels=ncharging_bypt$pt)
    levels(chg_tochange$nchg) <- ncharging_bypt$ID
    chg_tochange$nchg <- as.numeric(as.character(chg_tochange$nchg))
    chg_tochange$ID <- paste(chg_tochange$pt,chg_tochange$nchg)
    while(sum(duplicated(chg_tochange$ID))) {
      chg_tochange$nchg[duplicated(chg_tochange$ID)] <- chg_tochange$nchg[duplicated(chg_tochange$ID)] - 1
      chg_tochange$ID <- paste(chg_tochange$pt,chg_tochange$nchg)
    }
    chg_tochange$ID <- factor(chg_tochange$ID,levels=chgutil$ID)
    levels(chg_tochange$ID) <- 1:nrow(chgutil)
    chg_tochange$ID <- as.numeric(as.character(chg_tochange$ID))
    chgutil$util[chg_tochange$ID] <- chgutil$util[chg_tochange$ID] + 
      fleet$t_chg[selBat]/chgSpd
    
    chargers <- c(chargers, fleet$pt[selBat])
    chgAT <- c(chgAT,-1 * fullChg/chgSpd)
    fleet$range[selBat] <- fleet$bat[selBat]
  }
  

  
  
  # CHARGING RELOCATION----------------
  fleet <- fleet[order(fleet$range),]
  notcharging <- fleet$chg_bool==0
  vehA <- subset(fleet,notcharging & range/bat < chgthreshold) # Assign non-charging idle fleets to vehA
  
  if(nrow(vehA)>0 & length(chargers)>0) {
    
    # Make matrix with distances and times to each available charger
    
    tMx <- time_mx_list[[h+1]][vehA$pt,chargers] + 2
    dMx <- dist_mx[vehA$pt,chargers] 
    if(is.null(dim(tMx))) {
      tMx <- matrix(tMx,ncol=length(chargers))
      dMx <- matrix(dMx,ncol=length(chargers))
    } 
    
    # Bound by time charger has been available
    chgTime <- matrix(chgAT,nrow(vehA), length(chargers),byrow = T)
    
    maxCharge <- -1 * vehA$t_count - tMx # max time spent charging by newcomer
    maxCharge[maxCharge>chgTime] <- chgTime[maxCharge>chgTime] # bounded by time charger has been available
    
    # Bound by maximum battery range
    maxBat <- matrix((vehA$bat - vehA$range),nrow(vehA), length(chargers))
    maxCharge[maxCharge>maxBat] <- maxBat[maxCharge>maxBat]
    
    maxCharge <- maxCharge*chgSpd - dMx # amount of charge gained
    
    maxCharge[maxCharge < chgSpd*tMx | dMx > vehA$range] <- NA # is it worth the trip? can i make it?
    
    naMx <- !is.na(maxCharge)
    

    if(T %in% naMx) {
      
      feasChg <- which(colSums(naMx)!=0)
      feasveh <- which(rowSums(naMx)!=0)
      
      maxCharge <- maxCharge[feasveh,feasChg] 
      if(is.null(dim(maxCharge))) maxCharge <- matrix(maxCharge,ncol=length(feasChg))
      
      #Remove duplicate assignments
      ch0 <- vehAssign(-1*maxCharge)
      
      feasChg <- feasChg[!is.na(ch0)]
      dMx <- dMx[feasveh,feasChg]
      tMx <- tMx[feasveh,feasChg]
      
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
      
      
      chveh <- feasveh[ch0]
      
      # Relocate to chargers where viable, add range owed
      fleet$d_chgrel[notcharging][chveh] <- vehA$d_chgrel[chveh]+reloc.D   
      fleet$t_chgrel[notcharging][chveh] <- vehA$t_chgrel[chveh]+reloc.T
      
      fleet$range[notcharging][chveh] <- vehA$range[chveh] + mC
      fleet$pt[notcharging][chveh] <- chargers[feasChg]
      fleet$t_count[notcharging][chveh] <- vehA$t_count[chveh] + reloc.T
      fleet$t_chg[notcharging][chveh] <- vehA$t_chg[chveh] + (mC + reloc.D)
      fleet$chg_bool[notcharging][chveh] <- 1
      
      # Mark chargers as occupied

      chargers <- chargers[-feasChg] #untaken chargers remain available
      chgAT <- chgAT[-feasChg] #untaken chargers remain available
      
      
      
    }
  }
  
  fleet <- rbind(fleet,veh2)
  return(list(fleet,chargers,chgAT,chgutil))
}


fleetSim <- function(bat0=50,nchg=500,nloc=0,power=100,eff=1.5,waitMax=10,drel_max=1000,simsave=F,file=NULL,time_start=0,time_stop=1439,start=1,end=1,chgthreshold=1,display=F) {
  # Initiate------
  t0 <- Sys.time()
  
  bat1 <- bat0
  day <- dow[1]
  assign('day',day,envir = .GlobalEnv)
  
  sim <- NULL
  
  chgcoor <- kmeans(chgcoor_all,nchg)$centers
  
  if(nloc>0) {
    chgloc <- kmeans(chgcoor,nloc)$centers
    locations <- knnLookup(pts_tree,newdat=chgloc,k=1)
    chargers0 <- locations[knnLookup(createTree(chgloc),newdat=chgcoor,k=1)]
    
  } else {
    chargers0 <- knnLookup(pts_tree,newdat=chgcoor,k=1)
  }
  
  chargers <- chargers0

  mindist_tochg <- apply(as.matrix(dist_mx[,unique(chargers)],ncol=length(unique(chargers))),1,min)
  trip_data_all$maxChgReloc <- mindist_tochg[trip_data_all$end_pt]
  trip_data_all$chgDist <- trip_data_all$maxChgReloc + trip_data_all$dist
  assign('trip_data_all',trip_data_all,envir = .GlobalEnv)
  
  trip_data <- subset(trip_data_all,trip_data_all[[day]]==1)
  assign('trip_data',trip_data,envir = .GlobalEnv)
  
  chgutil <- data.frame(pt=chargers[order(chargers)],port=1:length(chargers),util=0)
  for(i in which(!duplicated(chgutil$pt))) {
    chgutil$port[i:nrow(chgutil)] <- chgutil$port[i:nrow(chgutil)] - chgutil$port[i] + 1
  }
  chgutil$ID <- paste(chgutil$pt,chgutil$port)
  
  chgAT <- rep(100,length(chargers0))
  
  fleet <- vehCreate(trp = data.frame(end_pt=sample(chargers0,size0,replace=T),
                                         dist=0,
                                         dur=0,
                                         ID=NA,
                                         maxChgReloc= 0),
                        bat0=bat0,
                        maxid=0)
  
  t <- 0
  sim <- NULL
  soc_0 <- 1
  
  if(simsave) {
    fleet_list <- list(fleet)
    chg_list <- list(data.frame(chargers,chgAT))
  }
  
  run <- 1
  chgSpd = round(power/ eff /60,1)
  
  # Run simulation--------
  while(t<=time_stop & nrow(fleet)<10000 & mean(fleet$bat)<bat0*10) {
    
    if(t%%60==0) {
      
      sim <- rbind(sim,data.frame(tel=round(as.numeric(difftime(Sys.time(),t0,units='mins')),2),
                                  run,
                                  hr=floor(t/60),
                                  day,
                                  bat0,
                                  nchg=length(chargers0),
                                  nloc=length(unique(chargers0)),
                                  power,
                                  chgthreshold,
                                  size0,
                                  nveh=nrow(fleet),
                                  avg_bat=mean(fleet$bat),
                                  ntrp=sum(fleet$ntrp),
                                  avg_chgutil=mean(chgutil$util)/(t+(which(dow==day)-1)*1440),
                                  avg_soc=round(mean(fleet$range/fleet$bat),2),
                                  d_trprel=sum(fleet$d_trprel),
                                  t_trprel=sum(fleet$t_trprel),
                                  d_chgrel=sum(fleet$d_chgrel),
                                  t_chgrel=sum(fleet$t_chgrel),
                                  d_pass=sum(fleet$d_pass),
                                  t_pass=sum(fleet$t_pass),
                                  avg_wait=round(sum(fleet$t_wait)/sum(fleet$ntrp),2)))
      
      print(sim[nrow(sim),])
      if(!is.null(file)) write.csv(sim,paste(file,bat0,nchg,nloc,power,chgthreshold,'hrprof.csv',sep='_'))
      assign('sim_hrprof',sim,envir = .GlobalEnv)
      
    }
    
    result <- fleetRoute(t,fleet,chargers,chgAT,chgutil,bat0,chgSpd,waitMax,drel_max,chgthreshold)
    fleet <- result[[1]]
    chargers <- result[[2]]
    chgAT <- result[[3]]
    chgutil <- result[[4]]
    
    trip_data_edit[[day]][fleet$tripID[!is.na(fleet$tripID)]] <- fleet$ID[!is.na(fleet$tripID)]
    assign('trip_data_edit',trip_data_edit,envir = .GlobalEnv)
    
    if(simsave) {
      fleet_list <- c(fleet, list(fleet))
      chg_list <- c(chg_list,list(data.frame(chargers,chgAT)))
      assign('fleet_list',fleet_list,envir = .GlobalEnv)
      assign('chg_list',chg_list,envir = .GlobalEnv)
    } 
  
    assign('time',t,envir = .GlobalEnv)
    assign('fleet',fleet,envir = .GlobalEnv)
    assign('chargers',chargers,envir = .GlobalEnv)
    assign('chgAT',chgAT,envir = .GlobalEnv)
    assign('chgutil',chgutil,envir = .GlobalEnv)
    
    # End of day-----
    if(t==1439 & day!='sunday') {
      t <- -1
      day <- dow[which(dow==day)+1]
      assign('day',day,envir = .GlobalEnv)
      
      
      trip_data <- subset(trip_data_all,trip_data_all[[day]]==1)
      assign('trip_data',trip_data,envir = .GlobalEnv)
      
    } else if(t==1439 & day=='sunday' & (soc_0 - mean(fleet$range/fleet$bat) > 0.05 | bat1 < mean(fleet$bat)*.95)) {
      t <- -1
      day <- dow[1]
      assign('day',day,envir = .GlobalEnv)
      
      soc_0 <- mean(fleet$range/fleet$bat)
      bat1 <- mean(fleet$bat)
      run <- run + 1
      
      trip_data <- subset(trip_data_all,trip_data_all[[day]]==1)
      assign('trip_data',trip_data,envir = .GlobalEnv)
      
      for(var in c('t_pass','d_pass','t_trprel','d_trprel','t_chgrel','d_chgrel','t_wait','ntrp')) fleet[[var]] <- 0
      chgutil$util <- 0
    }
    
    t <- t+1
    
    if(display) print(t)
    
  }
  
  return(sim[nrow(sim),])
}

# Setup inputs-----
dow <- c('monday','tuesday','wednesday','thursday','friday','saturday','sunday')
dist_byday <- NULL
for(d in dow) {
  dist_byday[[d]] <- sum(trip_data$dist*trip_data[[d]])
}
dist_byday <- data.frame(dist_byday)
nchg0 <- round(max(dist_byday)*1.2/200*2/24,-1)
pts_tree <- createTree(pts_uni)
eff <- 1.5

trip_data$dist <- round(trip_data$dist,1)
trip_data_all <- trip_data
trip_data_edit <- trip_data


size0 <- 0
for(d in dow) {
  trips <- subset(trip_data_all,trip_data_all[[d]]==1)
  activity <- cumsum(summary(factor(trips$start_min,levels=0:1440),maxsum=1e5)) -
    cumsum(summary(factor(trips$end_min,levels=0:1440),maxsum=1e5))
  size <- max(activity)
  if(size>size0) size0 <- size
}

# Execute simulation---------
file1 <- 'results/bangalore_ev'
set.seed(111)
# fleetSim(bat0=600,nchg=50,nloc=1,power=5000,file=file1,simsave = F,drel_max = 1000,chgthreshold=0.2)
res_all <- fleetSim(bat0=600,nchg=50,nloc=1,power=5000,file=file1,simsave = F,drel_max = 1000,chgthreshold=0.2)

for(nloc in c(0,10,1)) {
  for(power in c(50,200,500)) {
    for(chgut in c(0.5,0.8)) {
      nchg <- round(max(dist_byday)*1.2/power*eff/24/chgut,-1)
      
      for(chgthreshold in c(0.2,1)) {
        
        if(power>200) {
          
          set.seed(111)
          
          res_all <- rbind(res_all,fleetSim(bat0=50,nchg,nloc,power,file=file1,drel_max=1000,chgthreshold = chgthreshold))
          write.csv(res_all,'results/res_all.csv')
          saveRDS(list(fleet=fleet,trips=trip_data_edit,chgutil=chgutil),paste(file1,50,nchg,nloc,power,chgthreshold,'data.RDS',sep='_'))
        }
        
      }
    }
  }
}

res_all
library(ggplot2)
thresh_choice <- aggregate(avg_bat~power+nchg+nloc,res_all[-1,],min)
ggplot(subset(res_all,avg_bat %in% thresh_choice$avg_bat)) +
  geom_bar(aes(x=paste(power,nchg,nloc,sep='\n'),y=avg_bat,fill=factor(power),alpha=avg_bat<480),stat='identity') +
  geom_line(aes(x=as.numeric(factor(paste(power,nchg,nloc,sep='\n'))),y=nveh)) +
  geom_point(aes(x=as.numeric(factor(paste(power,nchg,nloc,sep='\n'))),y=nveh,alpha=avg_bat<480)) +
  geom_text(aes(x=10,y=490,label='Fleet size')) +
  scale_alpha_discrete(guide=F) +
  theme_bw() +
  labs(x='Scenario (charging power (kW), # chargers, # locations)',y='Battery range') +
  scale_fill_discrete(guide=F)
ggsave('actransit_simres_plot.jpeg')
