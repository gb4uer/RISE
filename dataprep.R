# Load libraries and set working directory
library(dplyr);library(rgdal);library(ggplot2);library(raster);library(rjson);library(geosphere);library(tidyverse);library(SearchTrees);library(osrm);library(h2o); library(stringr); library(readtext); library(gmapsdistance)

setwd("Bangalore data")

# Load in formfour data and add columns for each day of week 

formfour <- read_csv(file="11_10_form_four_Jan, 2019-withtimes.csv") %>% 
  mutate(Monday = case_when(
    str_detect(form_four_name, "All Days") ~ 1,
    str_detect(form_four_name, "Week Days") ~ 1,
    TRUE ~ 0)) %>% 
  mutate(Tuesday = Monday) %>% 
  mutate(Wednesday = Monday) %>% 
  mutate(Thursday = Monday) %>% 
  mutate(Friday = Monday) %>% 
  mutate(Saturday = case_when(
    str_detect(form_four_name, "All Days") ~ 1,
    str_detect(form_four_name, "Week Days") ~ 0,
    TRUE ~ 0)) %>% 
  mutate(Sunday = Saturday) %>% 
  mutate(Holiday = case_when(
    str_detect(form_four_name, "Holiday") ~ 1,
    TRUE ~ 0))

# Note: an alternative way to do the above, but it creates an additional column called 'i' 

#days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

#for(i in days){
#  if (i %in% days[1:5]) {
#    formfour <- formfour %>% 
#      mutate_(i = case_when(
#        str_detect("form_four_name", "All Days") ~ 1,
#        str_detect("form_four_name", "Week Days") ~ 1,
#        TRUE ~ 0))
#  }
#  else {
#    formfour <- formfour %>%   
#      mutate_(i = case_when(
#        str_detect("form_four_name", "All Days") ~ 1,
#        str_detect("form_four_name", "Week Days") ~ 0,
#        TRUE ~ 0))
#  }
#}

# Load in route data and get start and end legs of each route

routes <- read_csv(file="route_point_edit.csv") %>% 
  dplyr::select(-route_id, -bus_stop_id, -bus_stop_code, -bus_stop_group_id, -fare_stage, -sub_stage) %>% 
  rename(latitude = latitude_current, longitude = longitude_current) %>%
  group_by(route_number, route_direction) %>% 
  filter(route_order == 1 | route_order == max(route_order)) %>% 
  # Starting here, collapse matrix into one row per route_number
  mutate(startend = ifelse(route_order == 1, "start", "end")) %>%  
  gather(latitude, longitude, key = "lat/lon", value = "coordinate") %>% 
  unite("coord_type", startend, `lat/lon`, sep = "_") %>% 
  distinct() %>% 
  group_by(route_number, route_direction, bus_stop_name, coord_type) %>% 
  summarise(coordinate = mean(coordinate)) %>% 
  spread(coord_type, coordinate)

# Note to self: number of unique routes is 15421 based on this: 
# test <- routes$route_number %>% unique()
# length(test)

# Q: why doesn't this work? 
#routes_j2 <- routes_j %>% 
#  unite(route_nodir = route_number, route_direction, sep = "_", remove = FALSE)

###########################

#Choose a random subset of the data for simplicity

# routes_j <- routes %>% filter(route_number == "1" | route_number == "10-J" | route_number == "111"| route_number == "12" | route_number == "PSBS-JHC-SKK-KBS" | route_number == "333-Q" | route_number == "BGH-TNF-BMT24G" | route_number == "V-ITPL-HOD-SVM")
# formfour_j <- formfour %>% filter(route_number == "1" | route_number == "10-J" | route_number == "111"| route_number == "12" | route_number == "PSBS-JHC-SKK-KBS" | route_number == "333-Q" | route_number == "BGH-TNF-BMT24G" | route_number == "V-ITPL-HOD-SVM")

# Update routes data so that every row in routes has a start and end lat/lon and so that there’s only one matching row in routes for each row in form_four
# routes_j1 <- routes_j %>%
#   mutate(route_nodir = paste0(route_number, route_direction)) %>%
#   group_by(route_nodir) %>%
#   summarise_at(vars(end_latitude:start_longitude), na.omit)

# # Update form_four data to have the same route_nodir column as routes
# 
# formfour_j1 <- formfour_j %>%
#   mutate(route_nodir = paste0(route_number, route_direction))
# 
# # Run a test inner_join 
# 
# joined <- inner_join(formfour_j1, routes_j1, by = "route_nodir") %>%
#     dplyr::select(-schedule_id, -schedule_number, -form_four_id, -route_number_id, -trip_number, -route_name, -start_point, -end_point, -shift_type_id, -is_dread_trip, -org_name, -running_time, -break_time, -form_four_name)

###########################

# Try on full dataset 

# Update routes data so that every row in routes has a start and end lat/lon and so that there’s only one matching row in routes for each row in form_four
routes2 <- routes %>% 
  mutate(route_nodir = paste0(route_number, route_direction)) %>% 
  group_by(route_nodir) %>% 
  summarise_at(vars(end_latitude:start_longitude), max, na.rm = TRUE)
  # Note: summarise with na.omit (as in routes_j1) above did not work, so changed slightly
  # Open question -- not sure why observations in routes2 is not exactly half the number of     observations in routes (18998 vs. 37988)

# Update form_four data to have the same route_nodir column as routes

formfour2 <- formfour %>% 
  mutate(route_nodir = paste0(route_number, route_direction))

# Run inner_join 

joined2 <- inner_join(formfour2, routes2, by = "route_nodir") %>% 
  dplyr::select(-schedule_id, -schedule_number, -form_four_id, -route_number_id, -trip_number, -route_name, -start_point, -end_point, -shift_type_id, -is_dread_trip, -org_name, -running_time, -break_time, -form_four_name) 

trips_final <- joined2 %>% mutate(duration = (end_time - start_time)/60)

#write_csv(trips_final, path = "joined.csv")
