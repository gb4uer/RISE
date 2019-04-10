# Load data-------
library(dplyr);library(rgdal);library(ggplot2);library(raster);library(rjson);library(geosphere)
library(SearchTrees)
library(osrm)
library(h2o)

setwd('C:/Users/Gordon/Box Sync/RISE bus')

feed <- 'actransit'
url <- 'https://transitfeeds.com/p/ac-transit/121/20190326/download'
download.file(url, "feed.zip",mode="wb") 
unzip("feed.zip",exdir='feed')
data <- lapply(paste0('feed/',list.files("feed")),read.csv)
names(data) <- gsub('.txt','',list.files("feed"))
trip_ends <- subset(data$stop_times,c(!duplicated(data$stop_times$trip_id)[-1],T)|!duplicated(data$stop_times$trip_id))
trip_starts <- subset(trip_ends,!duplicated(trip_id))
trip_ends <- subset(trip_ends,c(!duplicated(trip_id)[-1],T))
trip_data <- data.frame(id=trip_ends$trip_id,
                        start_time=trip_starts$arrival_time,
                        end_time=trip_ends$departure_time,
                        start_stop=trip_starts$stop_id,
                        end_stop=trip_ends$stop_id,
                        n_stops=trip_ends$stop_sequence)


integrate <- function(new_df,old_df,old_var,new_key,old_key=new_key,num=T) {
  new_var <- factor(new_df[[new_key]],levels=old_df[[old_key]])
  levels(new_var) <- old_df[[old_var]]
  
  if(num) {
    new_var <- as.numeric(as.character(new_var))
  }
  
  return(new_var)
}


trip_data$start_lat <- integrate(trip_data,data$stops,'stop_lat','start_stop','stop_id')
trip_data$start_lon <- integrate(trip_data,data$stops,'stop_lon','start_stop','stop_id')
trip_data$end_lat <- integrate(trip_data,data$stops,'stop_lat','end_stop','stop_id')
trip_data$end_lon <- integrate(trip_data,data$stops,'stop_lon','end_stop','stop_id')

# Find distance of each route-------

haversine <- function(orgs, dsts) {
  orgs <- orgs*pi/180
  dsts <- dsts*pi/180
  
  a <- sin(0.5 * (dsts[,2] - orgs[,2]))
  b <- sin(0.5 * (dsts[,1] - orgs[,1]))
  12742 * asin(sqrt(a * a + cos(orgs[,2]) * cos(dsts[,2]) * b * b))
}


rt_dist <- NULL
uni_shps <- as.character(unique(data$shapes$shape_id))
for(i in 1:length(uni_shps)) {
  print(i)
  rt_dist$shp_id[i] <- uni_shps[i]
  
  shp_sub <- subset(data$shapes,shape_id==uni_shps[i])
  rt_dist$dist_km[i] <- sum(haversine(shp_sub[c('shape_pt_lon','shape_pt_lat')][-nrow(shp_sub),],shp_sub[c('shape_pt_lon','shape_pt_lat')][-1,])) %>% round(1)
}
rt_dist <- data.frame(rt_dist)
write.csv(rt_dist,'actransit_rtdist.csv',row.names = F)

rt_dist$trip_id <- integrate(rt_dist,subset(data$trips,!duplicated(shape_id)),old_var = 'trip_id',new_key = 'shp_id',old_key='shape_id')
trip_data$shape_id <- factor(trip_data$id,levels=data$trips$trip_id)
levels(trip_data$shape_id) <- data$trips$shape_id
trip_data$dist <- factor(trip_data$shape_id,levels=rt_dist$shp_id)
levels(trip_data$dist) <- rt_dist$dist_km
trip_data$dist <- as.numeric(as.character(trip_data$dist))

shape_starts <- subset(data$shapes,!duplicated(shape_id))
shape_ends <- subset(data$shapes,c(!duplicated(shape_id)[-1],T))

start_lat2 <- integrate(trip_data,shape_starts,'shape_pt_lat','shape_id')
start_lon2 <- integrate(trip_data,shape_starts,'shape_pt_lon','shape_id')
end_lat2 <- integrate(trip_data,shape_ends,'shape_pt_lat','shape_id')
end_lon2 <- integrate(trip_data,shape_ends,'shape_pt_lon','shape_id')

trip_data$start_lat[is.na(trip_data$start_lat)] <- start_lat2[is.na(trip_data$start_lat)] 
trip_data$start_lon[is.na(trip_data$start_lon)] <- start_lon2[is.na(trip_data$start_lon)] 
trip_data$end_lat[is.na(trip_data$end_lat)] <- end_lat2[is.na(trip_data$end_lat)] 
trip_data$end_lon[is.na(trip_data$end_lon)] <- end_lon2[is.na(trip_data$end_lon)] 

trip_data$rt <- integrate(trip_data,data$trips,'route_id','id','trip_id',num=F)
trip_data$start_min <- as.numeric(substring(trip_data$start_time,0,2))*60 + as.numeric(substring(trip_data$start_time,4,5))
trip_data$end_min <- as.numeric(substring(trip_data$end_time,0,2))*60 + as.numeric(substring(trip_data$end_time,4,5))
trip_data$start_min <- trip_data$start_min %% 1440
trip_data$end_min <- trip_data$end_min %% 1440

trip_data$dur <- trip_data$end_min - trip_data$start_min
trip_data$service_id <- integrate(trip_data,data$trips,'service_id','id','trip_id',num=F)
for(day in c('monday','tuesday','wednesday','thursday','friday','saturday','sunday')) {
  trip_data[[day]] <- integrate(trip_data,data$calendar,day,'service_id')
}
trip_data$service_id <-NULL

# Cluster into 500m grid cells------
pts <- rbind(as.matrix(trip_data[c('start_lon','start_lat')]),
             as.matrix(trip_data[c('end_lon','end_lat')]))
pts <- round(pts,3)
pts <- data.frame(pts)
pts <- subset(pts,!duplicated(start_lon) & !duplicated(start_lat))
dist_stn <- dist(x = pts)*pi*6371/2/90*1000
stn_clust1 <- hclust(dist_stn,method='complete')
clust_sub <- cutree(stn_clust1,h=500)
pts_uni <- subset(pts,!duplicated(clust_sub))
row.names(pts_uni) <- 1:nrow(pts_uni)

pts_tree <- createTree(pts_uni)

trip_data$start_pt <- knnLookup(pts_tree,newdat=trip_data[c('start_lon','start_lat')],k=1) %>% as.numeric
trip_data$end_pt <- knnLookup(pts_tree,newdat=trip_data[c('end_lon','end_lat')],k=1) %>% as.numeric


# Download OSRM--------
names(pts_uni) <- c('lng','lat')
grid_dur <- matrix(NA,nrow(pts_uni),nrow(pts_uni))
for(i in 1:ceiling(nrow(pts_uni)/100)) {
  # i=1;j=1
  src_ind <- pmin((i-1)*100+1:100,nrow(pts_uni))
  
  for(j in 1:ceiling(nrow(pts_uni)/100)) {
    print(c(i,j))
    
    dst_ind <- pmin((j-1)*100+1:100,nrow(pts_uni))
    
    os <- osrmTable(src= data.frame(start_id=src_ind,start_lng=pts_uni$lng[src_ind],start_lat=pts_uni$lat[src_ind]),
                    dst= data.frame(start_id=dst_ind,start_lng=pts_uni$lng[dst_ind],start_lat=pts_uni$lat[dst_ind]))
    grid_dur[src_ind,dst_ind] <- os$duration
    
    Sys.sleep(0.1)
    
  }
}



# Google Maps Download for sample-------------------------
set.seed(1)
smp_org <- sample(nrow(pts_uni),2000,replace=T)
smp_dst <- sapply(smp_org,function(x)sample((1:nrow(pts_uni))[-x],1))
smp_hr <- sample(0:23,2000,replace=T)

root <- "https://maps.googleapis.com/maps/api/distancematrix/json?key="
gg_key <- as.character(read.table('../gg_key.txt')[,1]) # put Google API key here

time0 <- 1554879600 # 12am April 10, 2019 Pacific Time

gg_smp <- NULL

for(i in 1:2000) {
  # testing: i=1;j=1
  if(i%%10==0) print(i)
  orig <- paste(pts_uni[smp_org[i],2],pts_uni[smp_org[i],1],sep=',')
  dest <- paste(pts_uni[smp_dst[i],2],pts_uni[smp_dst[i],1],sep=',')
  ggSch <- paste0(root,gg_key,'&origins=',orig,'&destinations=',dest,'&departure_time=',time0+smp_hr[i]*3600)
  
  rd <- readLines(ggSch)
  rj <- fromJSON(paste(rd, collapse = ""))
  gg_smp$dist[i] <- rj$rows[[1]]$elements[[1]]$distance$value/1000
  gg_smp$dur[i] <- rj$rows[[1]]$elements[[1]]$duration_in_traffic$value/60
}
gg_smp <- data.frame(gg_smp)


# Train model for time and distance forecasting------
travel_data <- data.frame(gg_time=gg_smp$dur,gg_dist=gg_smp$dist,dist_log=log(gg_smp$dist+0.001),
                          osrm=grid_dur[smp_org + (smp_dst-1)*nrow(grid_dur)],
                          start_lat=pts_uni$lat[smp_org],
                          start_lng=pts_uni$lng[smp_org],
                          end_lat=pts_uni$lat[smp_dst],
                          end_lng=pts_uni$lng[smp_dst],
                          hr=smp_hr)
travel_data$hav_dist <- diag(distm(travel_data[c('start_lng','start_lat')],travel_data[c('end_lng','end_lat')]))

trip_data$hr <- round((trip_data$start_min + trip_data$end_min)/120)
travel_data <- data.frame(time_log=log(travel_data$gg_time + 0.001),travel_data)
travel_data$osrm_log <- log(travel_data$osrm + 0.001)

lm_dist_log <- lm(formula(paste0('dist_log~',paste(names(travel_data)[-c(1:4,6:10)],collapse='+'))),travel_data)

start_pts = rep(1:nrow(pts_uni),nrow(pts_uni))
end_pts = rep(1:nrow(pts_uni),each=nrow(pts_uni))
grid_data <- data.frame(osrm=grid_dur[start_pts + (end_pts-1)*nrow(grid_dur)],
                        start_lat=pts_uni$lat[start_pts],
                        start_lng=pts_uni$lng[start_pts],
                        end_lat=pts_uni$lat[end_pts],
                        end_lng=pts_uni$lng[end_pts],
                        hav_dist=as.numeric(distm(pts_uni,pts_uni)),
                        osrm_log=log(grid_dur[start_pts + (end_pts-1)*nrow(grid_dur)]+0.001))

logcorr <- function(x) x-0.001

summary(lm(travel_data$gg_time~predict(lm_dist_log,travel_data) %>% exp %>% logcorr)) # 0.95 R2

dist_mx <- matrix(predict(lm_dist_log,grid_data) %>% exp %>% logcorr,nrow=nrow(pts_uni)) %>% round(1)


# Use gradient boosting to forecast times------

localH2O <- h2o.init(nthreads=-1)

features <- names(travel_data[,-(1:4)])
response <- 'gg_time'

travel_data.h2o <- as.h2o(travel_data[c(response,features)])

gbm.model_full <- h2o.gbm(y=response, x=features, training_frame = travel_data.h2o, nfolds=5, 
                          ntrees = 1e4, max_depth = 4, sample_rate=1, col_sample_rate=0.5, learn_rate = .01, seed = 1122,
                          stopping_rounds=1, stopping_tolerance=0.001,stopping_metric = 'MSE',score_tree_interval = 100)  

pred_full <- as.data.frame(h2o.predict(gbm.model_full,travel_data.h2o))[,1] %>% as.numeric

summary(lm(travel_data$gg_time~pred_full)) # 0.99 R2

model_path <- h2o.saveModel(object=gbm.model_full, path=getwd(), force=TRUE)
model_path <- gsub('.*\\\\','',model_path)

gbm.model_full_2 <- h2o.loadModel(model_path)

time_mx_list <- NULL
for(h in 1:24) {
  print(h)
  grid_data.h2o <- as.h2o(data.frame(grid_data,hr=h-1))
  time_mx_list[[h]] <- as.data.frame(h2o.predict(gbm.model_full_2,grid_data.h2o))[,1] %>% as.numeric %>% round %>% matrix(.,nrow=nrow(pts_uni))
}

lapply(time_mx_list,function(mx)mx[1:5,1:5])
lapply(time_mx_list,function(mx)summary(as.numeric(mx))) %>% do.call('rbind',.)

trip_data$days <- rowSums(trip_data[c('monday','tuesday','wednesday','thursday','friday','saturday','sunday')])
pts_all <- data.frame(lat=c(rep(trip_data$start_lat,times=trip_data$days),
                            rep(trip_data$end_lat,times=trip_data$days)),
                      lon=c(rep(trip_data$start_lon,times=trip_data$days),
                            rep(trip_data$end_lon,times=trip_data$days)))
pts_all$lat <- pts_all$lat + rnorm(nrow(pts_all),mean=0,sd=0.002)
pts_all$lon <- pts_all$lon + rnorm(nrow(pts_all),mean=0,sd=0.002)
chgcoor_all <- pts_all[,2:1]
trip_data <- trip_data[c('dist','start_min','end_min','dur','start_pt','end_pt','monday','tuesday','wednesday','thursday','friday','saturday','sunday')]
trip_data$ID <- 1:nrow(trip_data)
pts_tree <- createTree(pts_uni)

save(list=c('time_mx_list','dist_mx','trip_data','pts_uni','chgcoor_all'),file=paste0(feed,'_init.RData'))
