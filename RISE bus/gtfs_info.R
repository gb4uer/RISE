library(dplyr);library(rgdal);library(ggplot2);library(raster);library(rjson)

setwd('C:/Users/Gordon/Box Sync/TNC EV/Bangalore bus')
uber_movement <- read.csv('bangalore-wards-2018-4-OnlyWeekdays-HourlyAggregate.csv')
zones <- readOGR('bangalore_wards.json')

head(uber_movement)
summary(area(zones)/1e6)

shapefile_df <- fortify(zones)

# Now the shapefile can be plotted as either a geom_path or a geom_polygon.
# Paths handle clipping better. Polygons can be filled.
# You need the aesthetics long, lat, and group.
map <- ggplot() +
  geom_path(data = shapefile_df, 
            aes(x = long, y = lat, group = group),
            color = 'gray', size = .2)
map
transit_apikey <- '79fba491-e0dd-4dc3-bbd3-198545e02ad9'
# transitfeeds.com has API with all Google Transit Feed data
# 

loc_url <- paste0('https://api.transitfeeds.com/v1/getLocations?key=',transit_apikey)
rd <- readLines(loc_url)
rj <- fromJSON(paste(rd, collapse = ""))
rj <- rj$results$locations
names(rj[[1]])
loc_df <- lapply(rj,data.frame) %>% do.call('rbind',.) %>% data.frame
write.csv(loc_df,'gtfs_locations.csv',row.names = F)

p=1
feed_df <- NULL
for(p in 1:11) {
  feed_url <- paste0('https://api.transitfeeds.com/v1/getFeeds?page=',p,'&key=',transit_apikey,'&limit=10000')
  rd <- readLines(feed_url)
  rj <- fromJSON(paste(rd, collapse = ""))$results$feeds %>% lapply(.,function(l)data.frame(id=l$id,type=l$ty,name=l$t,loc_name=l$l$t,loc_id=l$l$`id`)) %>% do.call('rbind',.) %>% data.frame
  feed_df <- rbind(feed_df,rj)
}

i= which(feed_df$id=='ac-transit/121')
for(i in 986:nrow(feed_df)) {
  if(i%%10==0) print(i)
  url <- paste0('https://api.transitfeeds.com/v1/getFeedVersions?key=',transit_apikey,'&feed=',gsub('/','%2F',feed_df$id[i]),'&page=1&limit=1&err=1&warn=1')
  rd <- readLines(url)
  rj <- fromJSON(paste(rd, collapse = ""))$results
  
  if(rj$`total` > 0) {
    feed_df$download_url[i] <- rj$versions[[1]]$url
  }
}
feed_df$download_url[duplicated(feed_df$download_url)] <- NA
write.csv(feed_df,'gtfs_feed_desc.csv',row.names=F)