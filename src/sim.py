#!/usr/bin/env python3
import math
import numpy as np
import pandas as pd
import salabim as sim
import sys

#  based on SimPy example model
class Bus(sim.Component):
    """
    TODO
    """
    def __init__(self, start_time, end_time, trip_power, charging_rate, *args, **kwargs):
        sim.Component.__init__(self, *args, **kwargs)
        self.start_time    = start_time
        self.end_time      = end_time
        self.trip_power    = trip_power
        self.charging_rate = charging_rate

    def process(self):
        """TODO
        """
        #Trip starts when this component is activated
        yield self.request(bus_depot)
        yield self.hold(till=self.end_time)
        yield self.request((energy, self.trip_power))
        yield self.hold(duration=self.trip_power/self.charging_rate*3600)
        self.release(bus_depot)



class Dispatcher:
    def __init__(self, input_file, battery_cap_kwh=200, kwh_per_km=1.2, charging_rate=150):
        self.gtfs            = pd.read_pickle(input_file, compression='infer')
        self.gtfs            = self.gtfs.sort_values(['block_id', 'start_arrival_time'])
        self.buses           = []
        self.battery_cap_kwh = battery_cap_kwh
        self.kwh_per_km      = kwh_per_km
        self.charging_rate   = charging_rate
        self.trip_seg_count  = 0
        for block_id, group in self.gtfs.groupby(['block_id']):
            self.schedule_block(block_id, group)

    def schedule_block(self, block_id, trips):
        trips = trips.copy()
        distance_to_trip    = 5 #km. TODO: make this the maximum of the distance to the start and end
        travel_time_to_trip = 0.2*3600 #seconds
        maxdist             = self.battery_cap_kwh/self.kwh_per_km-2*distance_to_trip
        trips['cumdist']  = trips.distance.cumsum()
        trips['trip_seg'] = (trips.cumdist/1000/maxdist).astype(np.int)
        # print(trips[['trip_headsign','start_arrival_time','distance','cumdist','trip_seg']])
        for trip_seg, seg_group in trips.groupby('trip_seg'):
            trip_power      = (seg_group.iloc[-1].cumdist-seg_group.iloc[0].cumdist+2*distance_to_trip)*self.kwh_per_km
            trip_start_time = seg_group.iloc[0].start_arrival_time-travel_time_to_trip
            trip_end_time   = seg_group.iloc[-1].end_arrival_time+travel_time_to_trip
            new_bus         = Bus(trip_start_time,trip_end_time, trip_power, self.charging_rate, at=trip_start_time)
            self.trip_seg_count += 1


if len(sys.argv)!=2:
  print("Syntax: {0} <Parsed GTFS File>".format(sys.argv[0]))
  sys.exit(-1)

input_file = sys.argv[1]

env        = sim.Environment(trace=True) #time_unit='seconds'
energy     = sim.Resource("energy", anonymous=True, capacity=999999)
bus_depot  = sim.Resource("buses", capacity=999999)
dispatcher = Dispatcher(input_file)

env.run()

bus_depot.claimed_quantity.print_histogram()
energy.claimed_quantity.print_histogram()
bus_depot.print_statistics()
energy.print_statistics()
bus_depot.print_info()
energy.print_info()
print('Trip seg count = {0}'.format(dispatcher.trip_seg_count))
