#!/usr/bin/env python3
from functools import partial
import numpy as np
import pandas as pd
import partridge as ptg
import pyproj
import shapely as shp
import shapely.ops
import sys

wgs_to_aea = partial(
    pyproj.transform,
    pyproj.Proj(init='epsg:4326'), # source coordinate system
    pyproj.Proj(init='esri:102003')) # destination coordinate system
    #Converts to a US Contiguous Albert Equal Area projection. This fails on pyproj>2.0. 

def GetGeoDistanceFromLineString(line_string):
    line_string = shapely.ops.transform(wgs_to_aea, line_string)  # apply projection
    #Something for getting spherical distances from linestrings of lat-long coordinates goes here
    return line_string.length

def MatchColumn(df, colname):
  """Turns start_x and end_x into x while ensuring they were the same"""
  assert (df['start_'+colname]==df['end_'+colname]).all()
  df = df.drop(columns=['end_'+colname])
  df = df.rename(index=str, columns={'start_'+colname: colname})
  return df



if len(sys.argv)!=3:
  print("Syntax: {0} <GTFS File> <Output>".format(sys.argv[0]))
  sys.exit(-1)

feed_file   = sys.argv[1]
output_file = sys.argv[2]

#######################
#Filter for service ids

# find the busiest date
gtfs = ptg.load_geo_feed(feed_file)
date, service_ids = ptg.read_busiest_date(feed_file)

print("Service id chosen = {0}".format(service_ids))

#Select the service ids from that date. Note that trip_ids are still unique
#because they include service_ids as a substring
trips = gtfs.trips[gtfs.trips.service_id.isin(service_ids)]

#Clean up stops data
gtfs.stops['lat'] = gtfs.stops.geometry.y
gtfs.stops['lng'] = gtfs.stops.geometry.x

#Combine trips with stop data
trips = trips.merge(gtfs.stop_times, left_on='trip_id', right_on='trip_id')

#Filter trips to only regularly scheduled stops
trips = trips[trips.pickup_type.astype(int)==0]
trips = trips[trips.drop_off_type.astype(int)==0]

trips = trips.sort_values(['trip_id','route_id','service_id','stop_sequence'])

#TODO: Maybe concatenate trip_id with a service_id and route_id and direction_id to ensure it is unique?

#Drop unneeded columns
trips = trips.drop(columns=[
  'wheelchair_accessible',
  'pickup_type',
  'drop_off_type', 
  'service_id',       #Trip ids include this as a substring
  'direction_id',
  'route_id',
  'stop_sequence'     #This is held by the sorted order
])

#TODO: block_id is supposed to indicate continuous travel by a *single vehicle*
#and, thus, might provide a good way of simplifying the problem

#We need to get information about when each trip begins and ends, so we sort and
#then group by trip. Each group is then ordered such that its first and last
#entries contain the needed information.
first_stops = trips.groupby(by='trip_id').first().reset_index()
last_stops  = trips.groupby(by='trip_id').last().reset_index()

first_stops = first_stops.merge(gtfs.stops[['stop_id','lat','lng']], how='left', on='stop_id')
last_stops  = last_stops.merge (gtfs.stops[['stop_id','lat','lng']], how='left', on='stop_id')

trips = first_stops.merge(last_stops, on="trip_id")

#Now, we need to clean up the merge

#Rename columns
to_rename = dict()
for c in trips.columns:
  if c.endswith("_x"):
    to_rename[c] = "start_"+c.replace("_x","")
  elif c.endswith("_y"):
    to_rename[c] = "end_"+c.replace("_y","")

trips = trips.rename(index=str,columns=to_rename)

#Ensure that start and end values are the same and reduce them to a single
#column
trips = MatchColumn(trips,'shape_id')
trips = MatchColumn(trips,'block_id')
trips = MatchColumn(trips,'trip_headsign')

#Merge in distances
gtfs.shapes['distance'] = gtfs.shapes['geometry'].map(GetGeoDistanceFromLineString)
trips = trips.merge(gtfs.shapes[['shape_id', 'distance']], how='left', on='shape_id')
trips = trips.drop(columns='shape_id')
trips['duration'] = trips['end_arrival_time']-trips['start_departure_time']

#Get wait time between trips
trips = trips.sort_values(["block_id", "start_arrival_time"])
next_trip = trips.shift(-1).copy()
trips['wait_time'] = next_trip['start_departure_time']-trips['end_arrival_time']
trips['wait_time'][trips['block_id']!=next_trip['block_id']]=np.nan

#Output
trips.drop(columns=['trip_id']).to_csv(output_file, index=False)
