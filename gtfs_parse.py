#!/usr/bin/env python3

import partridge as ptg
import shapely as shp
import shapely.ops
from functools import partial
import pyproj

wgs_to_aea = partial(
    pyproj.transform,
    pyproj.Proj(init='epsg:4326'),   # source coordinate system
    pyproj.Proj(init='ESRI:102003')) # destination coordinate system
    #Converts to a US Contiguous Albert Equal Area projection

def GetGeoDistanceFromLineString(line_string):
    line_string = shapely.ops.transform(wgs_to_aea, line_string)  # apply projection
    #Something for getting spherical distances from linestrings of lat-long coordinates goes here
    return line_string.length

feed_file = '/home/rick/projects/RISE/data/gtfs_minneapolis.zip'

# find the busiest date
gtfs = ptg.load_geo_feed(feed_file)
date, service_ids = ptg.read_busiest_date(feed_file)

filtered_trips = gtfs.trips[gtfs.trips.service_id.isin(service_ids)]
trip_stops = filtered_trips.merge(gtfs.stop_times, left_on='trip_id', right_on='trip_id')

trip_stops = trip_stops.sort_values(['trip_id','route_id','service_id', 'stop_sequence'])

#TODO: Extract first and last stop from each trip_id group
trip_stops.groupby(by='trip_id', level=0, as_index=False).nth([0,-1])
#gives
#Trip FirstStopTime LastStopTime

#This
gtfs.shapes['distances'] = gtfs.shapes['geometry'].map(GetGeoDistanceFromLineString)
#gives
#shape_id Distance

#merge those tables on shape_id and drop extra columns

#Trip FirstStopTime LastStopTime Distance


gtfs.stop_times
