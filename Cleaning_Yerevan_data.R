library(dplyr)
library(stringr)
library(geosphere)
Yerevan_points_database <- read.csv("Yerevan_points.csv")
data.frame(Yerevan_points_database)

#removing unnesaccary categories
Yerevan_filtered <- Yerevan_points_database %>% filter(!TYPE %in% c("fuel", "gate", "tower", "trafic_signals", "drinking_water", "car", "tyres",
                                                              "speed_camera", "car_repair", "yes", "bench", "hostel", "memorial", "fountain", "car_rental",
                                                            "motorway_junction", "car_wash",  "crossing", "traffic_signals", "bus_station", "motel",
                                                          "give_way",  "«?????? ?????» ?????????? ????", "«?? ??? ??? ????????» ???", "company", "guest_house", "interior_decoration",
                                                          "?????????? ???????????? ??. ?????? ?????????",  "????", "waste_disposal", "waste_basket",
                                                          "bollard", "doityourself", "lift_gate", "car_parts", "fire_station", "artwork", "hotel", "furniture", "toilets", "parking") )

# Delete unnecessary symbols from WKT column
Yerevan_filtered$WKT <- gsub("POINT ","", Yerevan_filtered$WKT)
Yerevan_filtered$WKT <- gsub("[()]", "", Yerevan_filtered$WKT)

#Defining categories

#bank/atm
Yerevan_filtered$TYPE <- ifelse(Yerevan_filtered$TYPE %in% c("atm", "bank"), "bank/atm", Yerevan_filtered$TYPE)

#Restaurant/cafe
Yerevan_filtered$TYPE <- ifelse(Yerevan_filtered$TYPE %in% c("restaurant", "cafe", "fast_food", "outdoor"), "restaurant/cafe", Yerevan_filtered$TYPE)

#Supermarket/shops(food)
Yerevan_filtered$TYPE <- ifelse(Yerevan_filtered$TYPE %in% c("greengrocer", "supermarket", "confectionery", "marketplace", "kiosk",
                                                              "bakery", "butcher", "convenience"), "supermarket/shop(food)", Yerevan_filtered$TYPE)

#mall/shops(non-food)
Yerevan_filtered$TYPE <- ifelse(Yerevan_filtered$TYPE %in% c("clothes", "florist", "computer", "houseware", "cosmetics", "books", "toys", "gift", "mall", "stationery",
                                                             "electronics", "mobile_phone", "hardware", "shoes", "department_store", "hifi", "bureau_de_change",
                                                             "accessories", "telecommunication"), "mall/shop(non-food)",Yerevan_filtered$TYPE)

#healthcare
Yerevan_filtered$TYPE <- ifelse(Yerevan_filtered$TYPE %in% c("pharmacy", "hospital", "doctors","dentist", "clinic", "optician"), "healthcare", Yerevan_filtered$TYPE)

#schools/kindergarten
Yerevan_filtered$TYPE <- ifelse(Yerevan_filtered$TYPE %in% c("school", "kindergarten", 
                                                             "childcare", "college"), "School/kindergarten", Yerevan_filtered$TYPE)

#museum/theater/cinema

Yerevan_filtered$TYPE <- ifelse(Yerevan_filtered$TYPE %in% c("cinema", "theatre", "museum"), "cinema/theatre/museum", Yerevan_filtered$TYPE)


#pub/bars
Yerevan_filtered$TYPE <- ifelse(Yerevan_filtered$TYPE %in% c("pub", "bar", "nightclub"), "pub/bar", Yerevan_filtered$TYPE)

#beauty salons
Yerevan_filtered$TYPE <- ifelse(Yerevan_filtered$TYPE %in% c("hairdresser", "beauty"), "beauty_salon", Yerevan_filtered$TYPE)

#sport/fitness
Yerevan_filtered$TYPE <- ifelse(Yerevan_filtered$TYPE %in% c("sports", "fitness_centre", "sports_centre"), "sport/fitness", Yerevan_filtered$TYPE)

# playground/attraction

Yerevan_filtered$TYPE <- ifelse(Yerevan_filtered$TYPE %in% c("playground", "attraction", "monument"), "playground/attraction",  Yerevan_filtered$TYPE)

#state institutions/NGO

Yerevan_filtered$TYPE <- ifelse(Yerevan_filtered$TYPE %in% c("embassy", "courthouse", "it", "library",
                                                             "government", "police", "insurance", "ngo", "travel_agency"), "state institutions/NGO", Yerevan_filtered$TYPE)


#Giving points to each category


Yerevan_filtered$point <- ifelse(Yerevan_filtered$TYPE %in% c("bank/atm", "supermarket/shop(food)", "healthcare"), 10,
                                 ifelse (Yerevan_filtered$TYPE %in% c("sport/fitness", "playground/attraction", "state institutions/NGO", "pub/bar", "beauty_salon", "bus_stop"), 4,
                                  ifelse (Yerevan_filtered$TYPE %in% c("restaurant/cafe", "university", "cinema/theatre/museum"), 6 ,
                                  ifelse (Yerevan_filtered$TYPE  == "mall/shop(non-food)", 14 , 
                                  ifelse (Yerevan_filtered$TYPE %in% c("place_of_worship", "post_office"), 3 , 
                                  ifelse (Yerevan_filtered$TYPE  == "School/kindergarten", 7 , 
                                  ifelse (Yerevan_filtered$TYPE  == "bookmaker", 1 , 0)))))))

type_counts <- table(Yerevan_filtered$TYPE)

# Get the types that occur less than or equal to 3 times
types_to_delete <- names(type_counts[type_counts <= 7])

# Filter the rows with those types
rows_to_delete <- Yerevan_filtered %>% filter(TYPE %in% types_to_delete)

# Remove the rows from the original dataset
Yerevan_filtered <- Yerevan_filtered %>% filter(!TYPE %in% types_to_delete)
(Yerevan_filtered_count <- Yerevan_filtered %>% group_by(TYPE, point)%>% count(TYPE)) %>% arrange(desc(n))
# # Coordinates of Point 1 (latitude and longitude in decimal degrees)
# lat1 <- 44.5033
# lon1 <- 40.1772
# 
# # Coordinates of Point 2 (latitude and longitude in decimal degrees)
# lat2 <- 45.2355
# lon2 <- 39.6916

# # Calculate the distance between the two points
# distance <- distGeo(c(lon1, lat1), c(lon2, lat2))
# 
# # Print the result
# print(distance)  # The distance will be in meters
# # Sample character value
# value <- Yerevan_filtered[1,1]
# 
# # Split the string by space
# numbers <- strsplit(value, " ")[[1]]
# 
# # Convert the numbers to numeric values and store them in variables
# number1 <- as.numeric(numbers[1])
# number2 <- as.numeric(numbers[2])
# 
# # Print the numbers
# print(number1)  # Output: 44.4721880662711
# print(number2)  # Output: 40.1176802643038



write.csv(Yerevan_filtered, "newcleandata.csv")