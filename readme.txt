city_init.R: script for pre-processing taxi data, including downloading relocation data from Google Maps.

RISE bus: 
gtfs_info.R: script for scraping feed information (including download links) from transitfeeds.com
rise_bus_init.R: script for converting GTFS data into form usable by RISE and extrapolating relocation matrix from 2,000 data points downloaded from Google Maps API.
rise_bus_sim.R: simulation adjusted for use with bus systems.
actransit_init.RData: processed data from AC transit in Oakland to be used as model inputs.
feed.zip: GTFS feed for AC transit

NYC taxi sim:
cost model clean.R: model for estimating cost of simulation results
epacycle_nyc.csv: EPA driving cycle for NYC, used in cost model clean.R
taxi sim.R: RISE simulation script for taxi fleets
tx_clean.RData: inputs for Manhattan simulation
yt_1wk_select.RDS: cleaned trip data for 1 week of Yellow Cab trips in Manhattan