# Call Google Distance API

text <- readtext("../googlekey.txt")
text[1,2]

set.api.key(text[1,2])

gmapsdistance(origin = "38.1621328+24.0029257", destination = "37.9908372+23.7383394", mode = "driving", key = get.api.key())