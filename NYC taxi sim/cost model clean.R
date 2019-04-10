setwd('C:/Users/Gordon/Box Sync/AV')
library(dplyr)
library(ggplot2)

txData <- readRDS('yt_1wk_select.RDS')
txData <- txData[txData$trip_distance>0,]
dist_pass <- sum(txData$trip_distance[txData$pickup_day<13])
epa <- read.csv('epacycle_nyc.csv',header = T)
speeds <- epa$speed_mph
epa_dist <- sum(speeds)/3600
epa_time <- nrow(epa)-1
accel <- (speeds[3:length(speeds)] - speeds[1:(length(speeds)-2)])/2
accel <- c((speeds[2]-speeds[1])/2,accel,(speeds[length(speeds)]-speeds[length(speeds)-1])/2)

ntxFx <- function(finalFleet){
  ntx_hat <- NULL
  for(b in min(finalFleet$bat):max(finalFleet$bat)) {
    batfleet <- subset(finalFleet,bat==floor(b/10)*10 | bat==ceiling(b/10)*10)
    for(chg in levels(factor(batfleet$nchg))) {
      chgfleet = subset(batfleet,nchg==chg)
      for(spd in levels(factor(chgfleet$chgSpd))) {
        spdfleet = subset(chgfleet,chgSpd==spd)
        if(b>=min(spdfleet$bat) & b<=max(spdfleet$bat)) {
          if(b %in% spdfleet$bat) {
            ntx_hat <- rbind(ntx_hat,c(b,chg,as.numeric(chg)/4,spd,select(subset(spdfleet,bat==b)[1,],c(ntx,dist_tot,time_tot,days))) %>% as.numeric())
          } else {
            low <- spdfleet[spdfleet$bat<b,][which.min(abs(spdfleet[spdfleet$bat<b,]$bat-b)),]
            high <- spdfleet[spdfleet$bat>b,][which.min(abs(spdfleet[spdfleet$bat>b,]$bat-b)),]
            n1 <- low$ntx * abs(high$bat - b)/abs(high$bat - low$bat) + 
              high$ntx * abs(low$bat - b)/abs(high$bat - low$bat)
            d1 <- low$dist_tot * abs(high$bat - b)/abs(high$bat - low$bat) + 
              high$dist_tot * abs(low$bat - b)/abs(high$bat - low$bat)
            t1 <- low$time_tot * abs(high$bat - b)/abs(high$bat - low$bat) + 
              high$time_tot * abs(low$bat - b)/abs(high$bat - low$bat)
            dy <- low$days * abs(high$bat - b)/abs(high$bat - low$bat) + 
              high$days * abs(low$bat - b)/abs(high$bat - low$bat)
            
            ntx_hat <- rbind(ntx_hat,c(b,chg,as.numeric(chg)/4,spd,n1,d1,t1,dy) %>% as.numeric())
          }
        }
        
      }
    }
  }
  ntx_hat <- data.frame(ntx_hat)
  names(ntx_hat) <- c('bat','nchg','nodes','chgSpd','ntx','dist_tot','time_tot','days')
  return(ntx_hat)
}

bat_deg <- function(bat,dist_tot,ntx, chgSpd=0.5) { 
  a <- 8.61e-6
  b <- -5.13e-3
  c <- 7.63e-1
  d <- -6.7e-3
  e <- 2.35
  f <- 14876
  Ea <- 24.5
  R <- 8.314e-3
  temp <- 20+273.15
  kpm <- 0.25 + 0.00015*bat
  k1 <- 0.0008
  k2 <- (d*temp+e)
  I_chg <- chgSpd*60/bat
  power <- pmax(accel*.447*speeds*.447*(1100+7*bat*kpm)+0.5*0.3*1.225*0.66*(speeds*.447)^3,0)

  nrm <- epa_dist/bat/sum(power)
  c_rate <- power*nrm*3600
  Ah_cyc <- c_rate/3600*bat*kpm/4
  cyc_loss1 <- sum(k1*exp(k2*c_rate)*Ah_cyc)
  reps <- dist_tot/ntx/epa_dist
  Ah_day <- dist_tot/ntx*kpm/4 # Nissan Leaf: cell voltage ~4V
  cycle_loss <- (cyc_loss1*reps + k1*exp(k2*I_chg)*Ah_day)/2
  cal_loss <- f*exp(-Ea/(R*(273.15+15)))
  return(c(cycle_loss,cal_loss))
}

bat_durFx <- function(ntx,bat,nchg,nodes,dist_tot,time_tot,days,fastdeg=F, chgSpd=0.5) {
  deg <- bat_deg(bat,dist_tot/days,ntx,chgSpd = chgSpd)
  cycle_loss <- deg[1]
  cal_loss <- deg[2]
  df <- ntx_hat[ntx_hat$nchg==nchg & ntx_hat$chgSpd==chgSpd & ntx_hat$bat<=bat,]
  b1 <- df$bat[which.min(abs(df$ntx-ntx) %>% as.matrix())]
  if(length(b1)==0) return(c(0,NA))
  if(b1==bat)b1 <- bat - 1
  loss <- (1-b1/bat)*100
  t <- polyroot(c(-(loss^2),(2*loss*cycle_loss + cal_loss^2),-1*cycle_loss^2))
  bat_dur <- t/365.25
  
  if(b1<fastdeg*bat) {
    loss1 <- max((1-fastdeg)*100,0)
    t1 <- polyroot(c(-loss1^2,(2*loss1*cycle_loss + cal_loss^2),-1*cycle_loss^2))
    k <- loss-loss1+cal_loss*Re(t1[1])^0.5
    t2 <- polyroot(c(k^2-cal_loss^2*Re(t1[1]),-1*cal_loss^2,-2*k*cycle_loss,0,cycle_loss^2))
    bat_dur <- (t1[1] + t2[1])/365.25
    
  }
  
  
  
  return(c(Re(bat_dur[1]),b1))
}

ntx_cost <- function(ntx,bat,nchg,nodes,dist_tot,time_tot,days,fastdeg=F, chgSpd=0.5,park=T) {

  kpm <- 0.25 + 0.00015*bat
  bat_cost <- 200*bat*kpm*ntx
  bat_dur <- bat_durFx(ntx,bat,nchg,nodes,dist_tot,time_tot,days,fastdeg=fastdeg, chgSpd=chgSpd)
  bat_amort <- (bat_cost-100*kpm*ntx*bat_dur[2])*(0.05/(1-1.05^(-1*bat_dur[1])))
  bat_npv <- bat_amort*(1-1.05^(-20))/0.05
  
  ins_cost <- (600*ntx + 0.05*dist_tot/days*365.25)*(1-1.05^(-20))/0.05
  if(park) {
    park_cost <- 0.9*ntx*300*12*(1-1.05^(-20))/0.05
  } else {
    park_cost <- 0
  }

  veh_dur <- 300000/(dist_tot/days/ntx)/365.25 # Assume 300000mi lifetime
  veh_cost <- 30000*ntx
  veh_amort <- veh_cost*(0.05/(1-1.05^(-1*veh_dur)))
  veh_npv <- veh_amort*(1-1.05^(-20))/0.05 # Calculate NPV of fleet based on 10-year system lifetime
  dist_NPV <- dist_pass/7*365.25*(1-1.05^(-20))/.05
  overhead <- (15/6*ntx*365.25)*(1-1.05^(-20))/.05
  
  
  return(c(park_cost/dist_NPV, ins_cost/dist_NPV, overhead/dist_NPV + 0.04, veh_npv/dist_NPV, bat_npv/dist_NPV, bat_dur))
  
}

ntx_opt <- function(ntx,bat,nchg,nodes,dist_tot,time_tot,days,fastdeg=F,chgSpd=0.5,park=T) {
  r <- ntx_cost(ntx,bat,nchg,nodes,dist_tot,time_tot,days,fastdeg=fastdeg,chgSpd,park = park)
  sum(r[1:5])
} 

costFx <- function(ntx,bat,nchg,nodes,dist_tot,time_tot,days,file=NULL,fastdeg=F,chgSpd=0.5,park=T) {
  
  b=bat;nc=nchg;cs=chgSpd
  fleetsub = subset(ntx_hat,nchg==nc & chgSpd==cs & bat<b)
  if(nrow(fleetsub)==0) {
    return(c(bat,nchg,nodes,chgSpd,rep(99,12)))
  }
  
  ntx_costs = NULL
  for(i in fleetsub$ntx) {
    ntx_costs = c(ntx_costs,ntx_opt(ntx=i,bat=bat,dist_tot=dist_tot,time_tot=time_tot,
                      nchg=nchg,nodes=nodes,days=days,fastdeg=fastdeg,chgSpd=chgSpd,park=park))
  }
  
  ntx <- fleetsub$ntx[which.min(ntx_costs)]
  kpm <- 0.25 + 0.00015*bat
  
  chg_cost <- 10000*nchg*chgSpd + 250*nchg*chgSpd*(1-1.05^(-20))/.05 + 10000*nodes
  
  dist_cost <- dist_tot/days*365.25*(1-1.05^(-20))/.05*kpm*0.12
  dist_NPV <- dist_pass/7*365.25*(1-1.05^(-20))/.05
  
  cpm <- (chg_cost + dist_cost)/dist_NPV + ntx_opt(ntx,bat,nchg,nodes,dist_tot,time_tot,days,fastdeg=fastdeg,chgSpd=chgSpd,park=park)

  return(c(bat,nchg,nodes,chgSpd,ntx,cpm,ntx_cost(ntx,bat,nchg,nodes,dist_tot,time_tot,days,fastdeg = fastdeg,chgSpd=chgSpd,park=park),chg_cost/dist_NPV,dist_cost/dist_NPV,0.4+chgSpd*60/bat/5))
}

costFx_noDeg <- function(ntx,bat,nchg,nodes,dist_tot,time_tot,days,file=NULL,chgSpd) {
  kpm <- 0.25 + 0.00015*bat
  
  veh_dur <- 300000/(dist_tot/days/ntx)/365.25 # Assume 300000km lifetime
  veh_cost <- 30000*ntx
  veh_amort <- veh_cost*(0.05/(1-1.05^(-1*veh_dur)))
  veh_npv <- veh_amort*(1-1.05^(-20))/0.05 # Calculate NPV of fleet based on 10-year system lifetime
  
  bat_cost <- 200*bat*kpm*ntx
  bat_dur <- veh_dur
  bat_amort <- (bat_cost)*(0.05/(1-1.05^(-1*bat_dur)))
  bat_npv <- bat_amort*(1-1.05^(-20))/0.05
  
  ins_cost <- (600*ntx + 0.05*dist_tot/days*365.25)*(1-1.05^(-20))/0.05
  park_cost <- 0.9*ntx*300*12*(1-1.05^(-20))/0.05

  chg_cost <- 10000*nchg*chgSpd + 250*nchg*chgSpd*(1-1.05^(-20))/.05 + 10000*nodes
  
  overhead <- (15/6*ntx*365.25)*(1-1.05^(-20))/.05
  
  dist_cost <- dist_tot/days*365.25*(1-1.05^(-20))/.05*kpm*0.12
  dist_NPV <- dist_pass/7*365.25*(1-1.05^(-20))/.05
  
  costs <- c(park_cost/dist_NPV, ins_cost/dist_NPV, veh_npv/dist_NPV, bat_npv/dist_NPV, bat_dur,chg_cost/dist_NPV,dist_cost/dist_NPV,overhead/dist_NPV + 0.04)
  cpm <- sum(costs[-5])
  return(c(bat,nchg,nodes,chgSpd,ntx,cpm,costs))
}


# Calculate cost of service for all scenarios------------
fleets <- read.csv('simres_iterall.csv')
fleets$ntx[fleets$ntx>=10000] <- NA
fleets$nodes <- fleets$nchg/4
ntx_hat <- ntxFx(fleets)
 
ntx_hat <- subset(ntx_hat,!is.na(ntx))

fleetfull <- subset(fleets,!is.na(ntx))

resCost_cs <- NULL
for(i in 1:nrow(fleetfull)) {
  resCost_cs <- rbind(resCost_cs,with(fleetfull[i,],costFx(ntx,bat,nchg,nodes,dist_tot=dist_tot,time_tot=time_tot,days=days,file=NULL,fastdeg=0,chgSpd=chgSpd)))
}
resCost_cs <- data.frame(resCost_cs)
names(resCost_cs) <- c('bat','nchg','nodes','chgSpd','ntx_opt','cpm','park','ins','other','veh','bat_cost','bat_dur','soc_end','chg_cost','dist_cost','fastdeg')
resCost_cs[which.min(resCost_cs[,6]),]
resCost_cs$ntx <- fleetfull$ntx


# Visualize results by charging speed and battery range------------------
costbestbat <- NULL
s <- 0.5
for(s in levels(factor(fleets$bat))) {
  costbestbat <- rbind(costbestbat, resCost_cs[resCost_cs$bat==s & resCost_cs$bat%%10==0,][which.min(resCost_cs$cpm[resCost_cs$bat==s & resCost_cs$bat%%10==0]),])  
}

costbestspd <- NULL
for(b in levels(factor(fleets$bat))) {
  for(s in levels(factor(fleets$chgSpd))) {
    costbestspd <- rbind(costbestspd, resCost_cs[resCost_cs$chgSpd==s & resCost_cs$bat==b,][which.min(resCost_cs$cpm[resCost_cs$chgSpd==s & resCost_cs$bat==b]),])  
  }
}


costbestspd$chgSpd <- factor(costbestspd$chgSpd)
levels(costbestspd$chgSpd) <- c(7,11,22,50)
ggplot(subset(costbestspd,bat%%30==0)) + geom_tile(aes(x=factor(bat),y=chgSpd,fill=cpm)) +
  geom_text(aes(x=factor(bat),y=chgSpd,label=nchg)) +
  scale_fill_gradientn(colors=rainbow(50,end=0.7),name='Cost of service\n($/mi)') +
  labs(x='Battery range (mi)',y='Charging power (kW)') +
  theme_classic()
ggsave('cost_tile.jpeg')

dflong <- function(df,cols=1:ncol(df),ind=1:nrow(df)) {
  long <- NULL
  for(i in cols) {
    long <- rbind(long,cbind(ind,df[,i],names(df)[i]))
  }
  long <- data.frame(index=as.numeric(long[,1]),stat=as.numeric(long[,2]),name=as.character(long[,3]))
  return(long)
}
costbestbat <- costbestbat[-1,]
bestbat_long <- dflong(costbestbat,cols=c(7:11,14:15),ind=c(3:15)*10)
ggplot(bestbat_long) + geom_bar(aes(x=factor(index),y=stat,fill=name),stat='identity') + 
  labs(x='Battery range (mi)',y='Cost per mile ($)') + 
  scale_fill_discrete(name='Cost component',labels=c('Battery','Charging infrastructure','Electricity','Insurance','O&M','Parking','Vehicle purchase')) +
  theme_classic() + geom_text(data=costbestbat,aes(x=factor(bat),y=cpm+0.05,label=paste0(round(chgSpd*60*0.25),'\n',nchg)))+ 
  geom_rect(aes(xmin=which.min(costbestbat$cpm)-0.449,xmax=which.min(costbestbat$cpm)+0.449,
                ymin=0.001,ymax=min(costbestbat$cpm)-0.001),fill=NA,color='black',lwd=1) + ylim(c(0,.5))
ggsave('breakdown_iterbat.jpeg')


# Sensitivity analyses-------------
resCost_vardeg2 <- NULL
for(i in 1:nrow(fleetfull)) {
  resCost_vardeg2 <- rbind(resCost_vardeg2,with(fleetfull[i,],costFx(ntx,bat,nchg,nodes,dist_tot=dist_tot,time_tot=time_tot,days=days,file=NULL,fastdeg=0.6+chgSpd*60/bat/5,chgSpd=chgSpd)))
}
resCost_vardeg2 <- data.frame(resCost_vardeg2)
names(resCost_vardeg2) <- c('bat','nchg','nodes','chgSpd','ntx_opt','cpm','park','ins','other','veh','bat_cost','bat_dur','soc_end','chg_cost','dist_cost','fastdeg')
costbestspd_vardeg2 <- NULL
for(s in levels(factor(fleets$bat))) {
  costbestspd_vardeg2 <- rbind(costbestspd_vardeg2, resCost_vardeg2[resCost_vardeg2$bat==s,][which.min(resCost_vardeg2$cpm[resCost_vardeg2$bat==s]),])  
}
costbestspd_vardeg2
resCost_vardeg2[which.min(resCost_vardeg2[,6]),]

resCost_nodeg <- NULL
for(i in 1:nrow(fleets)) {
  #resCost <- rbind(resCost,with(ntx_hat[i,],costFx(ntx,bat,nchg,nodes,dist_tot=900000,time_tot=5400000,days=1,file=NULL,fastdeg=F,chgSpd=0.5)))
  resCost_nodeg <- rbind(resCost_nodeg,with(fleets[i,],costFx_noDeg(ntx,bat,nchg,nodes,dist_tot=dist_tot,time_tot=time_tot,days=days,file=NULL,chgSpd=chgSpd)))
}
resCost_nodeg <- data.frame(resCost_nodeg)
names(resCost_nodeg) <- c('bat','nchg','nodes','chgSpd','ntx_opt','cpm','park','ins','veh','bat_cost','bat_dur','chg_cost','dist_cost','other')
costbestspd_nodeg <- NULL
for(s in levels(factor(fleets$bat))) {
  costbestspd_nodeg <- rbind(costbestspd_nodeg, resCost_nodeg[resCost_nodeg$bat==s,][which.min(resCost_nodeg$cpm[resCost_nodeg$bat==s]),])  
}
costbestspd_nodeg
resCost_nodeg[which.min(resCost_nodeg[,6]),]

resCost_nopark <- NULL
for(i in 1:nrow(fleetfull)) {
  resCost_nopark <- rbind(resCost_nopark,with(fleetfull[i,],costFx(ntx,bat,nchg,nodes,dist_tot=dist_tot,time_tot=time_tot,days=days,file=NULL,fastdeg=0,park=F,chgSpd=chgSpd)))
}
resCost_nopark <- data.frame(resCost_nopark)
names(resCost_nopark) <- c('bat','nchg','nodes','chgSpd','ntx_opt','cpm','park','ins','other','veh','bat_cost','bat_dur','soc_end','chg_cost','dist_cost','fastdeg')
resCost_nopark[which.min(resCost_nopark[,6]),]

costbestspd_nopark <- NULL
for(s in levels(factor(fleets$bat))) {
  costbestspd_nopark <- rbind(costbestspd_nopark, resCost_nopark[resCost_nopark$bat==s,][which.min(resCost_nopark$cpm[resCost_nopark$bat==s]),])  
}
costbestbat$nopark = costbestbat$cpm - costbestbat$park
subset(resCost_nopark,bat==80)
