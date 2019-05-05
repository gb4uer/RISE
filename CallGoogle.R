# Call Google Distance API

text <- readtext("../googlekey.txt")
text[1,2]

set.api.key(text[1,2])

gettraveltime <- function(origin_lat, origin_lon, dest_lat, dest_lon) {
  origin_string <- paste0(toString(origin_lat), sep = "+", toString(origin_lon))
  dest_string <- paste0(toString(dest_lat), sep = "+", toString(dest_lon))
  google_results <- gmapsdistance(
    origin = origin_string, 
    destination = dest_string, 
    mode = "driving", 
    key = get.api.key()
  )
  travel_time = google_results$Time
  return(travel_time)
}

gettraveltime(38.1621328, 24.0029257, 37.9908372, 23.7383394)
