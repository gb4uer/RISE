import partridge as ptg
import pandas as pd
import shapely as shp
import shapely.ops
from functools import partial
import pyproj

wgs_to_aea = partial(
    pyproj.transform,
    pyproj.Proj(init='epsg:4326'), # source coordinate system
    pyproj.Proj(init='ESRI:102003')) # destination coordinate system
    #Converts to a US Contiguous Albert Equal Area projection. This fails on pyproj>2.0. 

def GetGeoDistanceFromLineString(line_string):
    line_string = shapely.ops.transform(wgs_to_aea, line_string)  # apply projection
    #Something for getting spherical distances from linestrings of lat-long coordinates goes here
    return line_string.length

feed_file = '/home/nclarke/winhome/repos/RISE/RISE Bus/ac_transit.zip'

# find the busiest date
gtfs = ptg.load_geo_feed(feed_file)
date, service_ids = ptg.read_busiest_date(feed_file) # this returns multiple days, does this cause overlap in trips?

filtered_trips = gtfs.trips[gtfs.trips.service_id.isin(service_ids)]
trip_stops = filtered_trips.merge(gtfs.stop_times, left_on='trip_id', right_on='trip_id')
trip_stops = trip_stops.sort_values(['trip_id','route_id','service_id', 'stop_sequence'])

trips = trip_stops.iloc[0,0:11].to_frame().transpose() # create a final "trips" df, using only needed cols. 
# Should probaly use col labels instead of index, as there are optional col names that could be included in other feeds. 
trips = trips.drop(0) # I couldnt figure out how to just take a slice of the header row w/out also taking at least the first row. Please advise.

ts_by_tid = trip_stops.groupby(by='trip_id') # group trips by trip_id and then find origin & destination stop_ids
for name, group in ts_by_tid:
  trips = trips.append(group[0:1])
  trips['origin_stop'] = group['stop_id'].iloc[0]
  trips['destination_stop'] = group['stop_id'].iloc[-1]
# there must be a better way than a loop for this?

gtfs.shapes['distances'] = gtfs.shapes['geometry'].map(GetGeoDistanceFromLineString)
pd.merge(trips, gtfs.shapes[['shape_id','distances']],left_on='shape_id', right_on='shape_id', how='inner')
# this merge does not work. halp!
trips.to_csv('/home/nclarke/winhome/repos/RISE/RISE Bus/trips.csv')