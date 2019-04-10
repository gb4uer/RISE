library(dplyr)
library(geosphere)
library(SearchTrees)
library(ggplot2)
rawC <- readRDS('../China EV - private/cityX_raw.RDS')
# Process raw data---------------
raw_data <- read.csv('thisistherawdata.csv')
saveRDS(raw_data,'cityX_raw.RDS')

# Do processing to get following columns: t_start (# min, integer 0-1439), duration (min, integer), distance (mi, float), lat_start, lng_start, lat_end, lng_end
raw_data <- subset(raw_data, duration>0 & distance>0 & distance/duration*60<100) # cleaning infeasible trips

# Bounding box that cuts out extreme 1%
minlat <- lats[order(lats)[round(length(lats)/100)]]
maxlat <- lats[order(lats,decreasing=T)[round(length(lats)/100)]]
minlng <- lngs[order(lngs)[round(length(lngs)/100)]]
maxlng <- lngs[order(lngs,decreasing=T)[round(length(lngs)/100)]]

raw_data <- subset(raw_data,lat_start > minlat & lng_start > minlng &
                 lat_start < maxlat & lng_start < maxlng &
                 lat_end > minlat & lng_end > minlng &
                 lat_end < maxlat & lng_end < maxlng)

lnglen = distm(c(maxlng,mean(minlat,maxlat)),c(minlng,mean(minlat,maxlat)))
latlen = distm(c(mean(minlng,maxlng),maxlat),c(mean(minlng,maxlng),minlat))

# Center points for grid cells with 500m sides
ctrlat <- minlat + seq(from=0,to=maxlat-minlat,length.out=round(latlen/500))
ctrlng <- minlng + seq(from=0,to=maxlng-minlng,length.out=round(lnglen/500))

gridcoor <- data.frame(lng = rep(ctrlng,length(ctrlat)), lat = as.vector(sapply(ctrlat,rep,times=length(ctrlng))))

gridTree <- createTree(gridcoor)
raw_data$pt_start <- knnLookup(gridTree,raw_data$lng_start,raw_data$lat_start,k=1)
raw_data$pt_end <- knnLookup(gridTree,raw_data$lng_end,raw_data$lat_end,k=1)

cntstart <- summary.factor(raw_data$pt_start,maxsum=9e9)
cntend <- summary.factor(raw_data$pt_end,maxsum=9e9)

gridcoor$cntstart <- 0
gridcoor$cntstart[as.numeric(names(cntstart))] <- cntstart
gridcoor$cntend <- 0
gridcoor$cntend[as.numeric(names(cntend))] <- cntend
sum(gridcoor$cntend>0 | gridcoor$cntstart>0) # how many non-zero grid cells


# Get 600 clusters for Google Maps download
ctr600 <- kmeans(cbind(c(raw_data$lat_start,raw_data$lat_end),c(raw_data$lng_start,raw_data$lng_end)),600)
center600 <- data.frame(lng=ctr600$centers[,2],lat=ctr600$centers[,1])
ctr100 <- kmeans(center600,100) # For congestion predictions
gridcoor$cntstart[gridcoor$cntstart==0] <- NA

ggplot(gridcoor) + geom_point(aes(x=lng,y=lat,color=cntstart)) +
  geom_point(data=center600,aes(x=lng,y=lat),color='yellow',alpha=0.2) +
  geom_point(data=data.frame(ctr100$centers),aes(x=lng,y=lat),color='green',alpha=0.2)

clusters <- list(all=gridcoor,kmeans600=ctr600,kmeans100=ctr100)
saveRDS(clusters,'cityX_clusters.RDS')

# Google Maps Download -------------------------
root <- "https://maps.googleapis.com/maps/api/distancematrix/json?key="
key <- 'XXXX' # put Google API key here
ggCoor <- ctr600$centers

time0 <- 1522522800 # Put desired download time here in epoch

ggDistMx <- matrix(NA,600,600)
ggTimeMx <- matrix(NA,600,600)
write.csv(ggCoor,'cityX_ggCoor.csv')
library(rjson)
for(i in 1:60) {
  # testing: i=1;j=1
  oind <- (i*10-9):(i*10)
  orig <- paste(ggCoor[oind,1],ggCoor[oind,2],sep=',',collapse = '|')
  for(j in 1:60) {
    print(c(i,j))
    dind <- (j*10-9):(j*10)
    dest <- paste(ggCoor[dind,1],ggCoor[dind,2],sep=',',collapse = '|')
    
    ggSch <- paste0(root,key,'&origins=',orig,'&destinations=',dest,'&departure_time=',time0)
    rd <- readLines(ggSch)
    rj <- fromJSON(paste(rd, collapse = ""))
    mistake <- sapply(rj$rows, function(l) !all(summary(l$elements)[,1]==4))
    
    if(sum(mistake)) {
      for(m1 in which(mistake)) {
        # i = 3
        mis <- which(summary(rj$rows[[m1]]$elements)[,1]!=4)
        for(m2 in mis) {
          # m = 10
          names(rj$rows[[m1]]$elements[[m2]])[names(rj$rows[[m1]]$elements[[m2]])=='duration'] <- 'duration_in_traffic'
        }
      }
      rt <- unlist(sapply(rj$rows,unlist))
      ggTimeMx[oind,dind] <- matrix(as.numeric(rt[names(rt)=='elements.duration_in_traffic.value'])/60,10,10,byrow = T)
      ggDistMx[oind,dind] <- matrix(as.numeric(rt[names(rt)=='elements.distance.value'])/1000,10,10,byrow = T)
    } else {
      rt <- sapply(rj$rows,unlist)
      ggTimeMx[oind,dind] <- matrix(as.numeric(rt[row.names(rt)=='elements.duration_in_traffic.value',])/60,10,10,byrow = T)
      ggDistMx[oind,dind] <- matrix(as.numeric(rt[row.names(rt)=='elements.distance.value',])/1000,10,10,byrow = T)
    }
    
    
    
    Sys.sleep(1.1)
  }
}

apply(ggTimeMx, 2, function(v)sum(!is.na(v)))
apply(ggDistMx, 2, function(v)sum(!is.na(v)))

sum(!is.na(ggTimeMx));i;j

saveRDS(ggTimeMx,'cityX_ggTimeMx.RDS')
saveRDS(ggDistMx,'cityX_ggDistMx.RDS')
saveRDS(raw_data,'cityX_edit.RDS')



# Congestion times -------------------
clusters <- readRDS('cityX_clusters.RDS')
ggCoor <- read.csv('cityX_ggCoor.csv')

summary(clusters$kmeans100)
ctr100 <- clusters$kmeans100$centers
timeMx100 <- NULL
time0 <- 1522512000 # midnight on desired day, in epoch time

library(rjson)
dest <- paste(ctr100[,2],ctr100[,1],sep=',',collapse = '|')
h=0
for(h in 0:23) {
  time1 <- time0 + h*3600
  timeMx100[[h+1]] <- matrix(NA,ncol=100,nrow=100)

  for(i in 1:100) {
    # i=1
    orig <- paste(ctr100[i,2],ctr100[i,1],sep=',',collapse = '|')
    print(c(h,i))
    
    ggSch <- paste0(root,key,'&origins=',orig,'&destinations=',dest,'&departure_time=',time1)
    rd <- readLines(ggSch)
    rj <- fromJSON(paste(rd, collapse = ""))
    mistake <- sapply(rj$rows[[1]]$elements, function(v) (!'duration_in_traffic' %in% names(v)))
    
    if(sum(mistake)) {
      for(m1 in which(mistake)) {
        if('duration' %in% names(rj$rows[[1]]$elements[[m1]])) {
          names(rj$rows[[1]]$elements[[m1]])[names(rj$rows[[1]]$elements[[m1]])=='duration'] <- 'duration_in_traffic' 
        } else {
          rj$rows[[1]]$elements[[m1]]$duration_in_traffic <- NA
        }
      }
      rt <- unlist(sapply(rj$rows,unlist))
      timeMx100[[h+1]][i,] <- as.numeric(rt[row.names(rt)=='elements.duration_in_traffic.value'])/60
    } else {
      rt <- sapply(rj$rows,unlist)
      timeMx100[[h+1]][i,] <- as.numeric(rt[row.names(rt)=='elements.duration_in_traffic.value',])/60
    }
    
    
    
    Sys.sleep(1.1)
  }
}
saveRDS(timeMx100,'cityX_congTimes.RDS')



# Interpolation--------------
ggTimeMx <- readRDS('cityX_ggTimeMx.RDS')
ggDistMx <- readRDS('cityX_ggDistMx.RDS')
apply(ggTimeMx, 2, function(v)sum(!is.na(v)))
clusters <- readRDS('cityX_clusters.RDS')
centers <- clusters[[1]]
centers <- subset(centers,!is.na(cnt))


ggCoor <- clusters[[2]]$centers[,2:1]
tree600 <- createTree(ggCoor)

interp2 <- knnLookup(tree600,newdat=centers[,1:2],k=3)
weights2 <- NULL
for(i in 1:nrow(interp2)) {
  #i=1
  pts <- ggCoor[interp2[i,],]
  dists <- apply(pts,1,function(v)abs(v[1]-centers[i,1]) + abs(v[2]-centers[i,2]))
  weights2 <- rbind(weights2,(1/dists)/sum((1/dists)))
}

distMx1 <- matrix(NA,nrow(centers),600)
timeMx1 <- matrix(NA,nrow(centers),600)
distwts <- NULL
i=1
for(i in 1:nrow(centers)) {
  distwts[[i]] <- as.vector(distm(centers[i,1:2],ggCoor))/t(distm(ggCoor[interp2[i,],],ggCoor))
  distwts[[i]][distwts[[i]]==Inf] <- 1
}

for(i in 1:nrow(distMx1)) {
  # i =1
  
  newdist <- ggDistMx[interp2[i,],] * t(distwts[[i]])
  newtime <- ggTimeMx[interp2[i,],] * t(distwts[[i]])
  
  distMx1[i,] <- t(weights2[i,]) %*% newdist
  timeMx1[i,] <- t(weights2[i,]) %*% newtime
}

distMx_full <- matrix(NA,nrow(centers),nrow(centers))
timeMx_full <- matrix(NA,nrow(centers),nrow(centers))

distwts <- NULL
for(i in 1:nrow(centers)) {
  distwts[[i]] <- as.vector(distm(centers[i,1:2],centers[,1:2]))/t(distm(ggCoor[interp2[i,],],centers[,1:2]))
  distwts[[i]][distwts[[i]]==Inf] <- 1
}

for(i in 1:ncol(distMx_full)) {
  # i =1
  
  newdist <- distMx1[,interp2[i,]] * distwts[[i]]
  newtime <- timeMx1[,interp2[i,]] * distwts[[i]]
  
  distMx_full[,i] <- newdist %*% weights2[i,]
  timeMx_full[,i] <- newtime %*% weights2[i,]
}

smp <- sample(length(distMx_full),1000)
ggtest <- NULL

for(i in 1:1000) {
  # i=300
  print(i)
  oind = smp[i] %% nrow(centers)
  dind = ceiling(smp[i]/nrow(centers))
  ggSch <- paste0(root,key,'&origins=',paste(centers[oind,2:1],collapse=','),
                  '&destinations=',paste(centers[dind,2:1],collapse=','),'&departure_time=',time0)
  rd <- readLines(ggSch)
  rj <- fromJSON(paste(rd, collapse = ""))
  mistake <- sapply(rj$rows, function(l) !all(summary(l$elements)[,1]==4))
  
  if(sum(mistake)) {
    rt <- unlist(sapply(rj$rows,unlist))
    if(grepl('ZERO',rt)) {
      ggtest <- rbind(ggtest,c(NA,NA))
    } else {
      ggtest <- rbind(ggtest,c(as.numeric(rt[row.names(rt)=='elements.duration.value'])/60,
                               as.numeric(rt[row.names(rt)=='elements.distance.value'])/1000) )
    }
  } else {
    rt <- sapply(rj$rows,unlist)
    ggtest <- rbind(ggtest,c(as.numeric(rt[row.names(rt)=='elements.duration_in_traffic.value'])/60,
                             as.numeric(rt[row.names(rt)=='elements.distance.value'])/1000))
  }
  
  
  Sys.sleep(0.1)
}


plot(ggtest[,2],distMx_full[smp],pch=20)
plot(ggtest[,1],timeMx_full[smp],pch=20)
summary(lm(ggtest[,2]~distMx_full[smp]))
summary(lm(ggtest[,1]~timeMx_full[smp]))

saveRDS(distMx_full,'cityX_distMx_full.RDS')
saveRDS(timeMx_full,'cityX_timeMx_full.RDS')

ggCongMx <- readRDS('cityX_congTimes.RDS')
ggDiffMx <- lapply(ggCongMx,function(mx)mx/ggCongMx[[4]])

ggCoor100 <- clusters[[3]]$centers
tree100 <- createTree(ggCoor100)
interp100 <- knnLookup(tree100,newdat = centers[,1:2],k=1)
saveRDS(interp100,'cityX_congInterp.RDS')
saveRDS(ggDiffMx,'cityX_diffMx.RDS')


# Charging distribution synthesis--------------
clusters <- readRDS('cityX_clusters.RDS')
trpData <- readRDS('cityX_edit.RDS')

grid <- clusters[[1]]
latlng_all <- data.frame(lat=c(trpData$lat_start,trpData$lat_end),lng=c(trpData$lng_start,trpData$lng_end))
grid_latlng <- grid[c(trpData$pt_start,trpData$pt_end),1:2]

gridtree <- createTree(grid[,1:2])
chgpts <- NULL
for(nchg in 1:8*500) {
  print(nchg)
  dtb_kmeans <- kmeans(latlng_all,nchg)
  chgpts <- c(chgpts,list(knnLookup(gridtree,dtb_kmeans$centers[,2],dtb_kmeans$centers[,1],k=1)))
}


chgdtb2 <- NULL
for(chg in 1:8) {
  for(loc in c(10,50,100,250,500,750)) {
    if(length(summary.factor(chgpts[[chg]],maxsum = 8000))>loc) {
      loc_kmeans <- kmeans(grid[chgpts[[chg]],1:2],loc)
      loctree <- createTree(loc_kmeans$centers)
      pts <- loc_kmeans$centers[knnLookup(loctree,grid[chgpts[[chg]],1],grid[chgpts[[chg]],2],k=1),]
      chgdtb2 <- c(chgdtb2,list(knnLookup(gridtree,pts[,1],pts[,2],k=1)))
      names(chgdtb2)[length(chgdtb2)] <- paste(length(chgpts[[chg]]),loc,sep='x')
    }
  }
  
  chgdtb2 <- c(chgdtb2,list(chgpts[[chg]]))
  names(chgdtb2)[length(chgdtb2)] <- paste(length(chgpts[[chg]]),
                                           length(summary.factor(chgpts[[chg]],maxsum = 8000)),sep='x')
  
  
}

sapply(chgdtb2,function(v) length(summary.factor(v,maxsum = 8000)))
saveRDS(chgdtb2,'cityX_chgdtb.RDS')
save.image('cityX_init.RData')