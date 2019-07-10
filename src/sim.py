#!/usr/bin/env python3
import pandas as pd
import salabim as sim

#  based on SimPy example model
class Bus(sim.Component):
    """
    A Bus does things
    
    """

    def __init__(self, block_id, start_time, charging_rate=150, battery_capacity=100, km_per_kwh=4):
        self.start_trip(block_id, time)
        self.battery_capacity = battery_capacity
        self.battery_level = battery_capacity
        self.km_per_kwh = km_per_kwh
        self.block_id = block_id
        self.charging_rate = charging_rate

    def process(self):
        """
        Calculates a trip
        """
        self.get_bus()
        while self.calculate_trip(start_time):
            self.battery_level -= self.trip_power
            yield self.hold(till=self.end_time)
            self.return_bus() #This can't block
            self.get_bus()
        self.release(bus_depot)

    def calculate_trip(self, start_time):
        """
        """
        self.start_time = start_time
        self.my_trips = gtfs[gtfs.block_id==self.block_id & gtfs.start_arrival_time>=start_time]
        if len(self.my_trips)==0:
            return False
        self.my_trips['cumdist'] = self.my_trips.distance.cumsum()
        maxdist = self.battery_level*self.km_per_kwh-8 #so we can get to and from the depot. arbitrary atm. 
        last_trip = self.my_trips[self.my_trips.cumdist<maxdist][-1]
        self.end_time = last_trip.end_arrival_time
        self.trip_power = maxdist/self.km_per_kwh
        return True

    def get_bus(self):
        """
        """
        yield self.request(bus_depot)
        self.battery_level = self.battery_capacity

    def return_bus(self):
        """
        """
        yield self.hold(duration=5) #Travel time to bus depot
        yield self.request((energy,self.trip_power)) #Charge up the bus
        charge_time = self.trip_power*self.charging_rate
        yield self.hold(duration=charge_time) #Charge up
        yield self.release(bus_depot) #Free the bus for use



class BusGenerator(sim.Component):
    """
    Generate new Bus.
    """
    # get a list of all the unique block ids
    # generate a bus for each block_id


charged_buses = 0

# Setup and start the simulation
env = sim.Environment(trace=True)
print("EV Bus Simulation")

# import pre-processed GTFS trips

if len(sys.argv)!=2:
  print("Syntax: {0} <Parsed GTFS File>".format(sys.argv[0]))
  sys.exit(-1)

input_file = sys.argv[1]
gtfs = pd.read_pickle(input_file, compression='infer')

#Global resources
energy = sim.Resourse("energy", anonymous=True)
bus_depot = sim.Resource("buses", anonymous=True)

CarGenerator()

env.run(SIM_TIME)


# fuel_pump.capacity.print_histogram()
#fuel_pump.claimed_quantity.print_histogram()
#fuel_pump.available_quantity.print_histogram()


#gas_station.requesters().length.print_histogram()
#gas_station.requesters().length_of_stay.print_histogram(30, 0, 10)
