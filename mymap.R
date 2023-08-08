install.packages("rstudioapi")
library(ggplot2) 
library(ggmap)
library(plotly)
library(dplyr)
library(rstudioapi)
library(stringr)
library(leaflet)

citypoints <- read.csv('test.csv')
citypoints$WKT <- substr(citypoints$WKT, 8, length(citypoints$WKT))
citypoints$WKT <- str_trim(citypoints$WKT, "both")
citypoints$WKT <- gsub(")", "", citypoints$WKT)


citypoints$lon <- str_split(citypoints$WKT, " ")[[1]][1]
citypoints$lat <- str_split(citypoints$WKT, " ")[[1]][2]
for(i in 1:nrow(citypoints)) {
  citypoints[i, ncol(citypoints) - 1] <- str_split(citypoints$WKT, " ")[[i]][1]
  citypoints[i, ncol(citypoints)] <- str_split(citypoints$WKT, " ")[[i]][2] 
}


options(digits = 16)
citypoints$lon <- as.numeric(citypoints$lon)
citypoints$lat <- as.numeric(citypoints$lat)

banks <- citypoints$TYPE == "bank"
atms <- citypoints$TYPE == "atm"

citypoints[banks,"TYPE"] <- "Banks, ATMs"
citypoints[atms, "TYPE"] <- "Banks, ATMs"
citypoints <- citypoints[-which(citypoints$TYPE == "gate"),]
citypoints <- citypoints[-which(citypoints$TYPE == "tower"),]
citypoints <- citypoints[-which(citypoints$TYPE == "speed_camera"),]
citypoints <- citypoints[-which(citypoints$TYPE == "traffic_signals"),]
citypoints <- citypoints[-which(citypoints$TYPE == "fuel"),]
citypoints <- citypoints[-which(citypoints$TYPE == "fire_station"),]
citypoints <- citypoints[-which(citypoints$TYPE == "car"),]
citypoints <- citypoints[-which(citypoints$TYPE == "car_repair"),]
citypoints <- citypoints[-which(citypoints$TYPE == "car_wash"),]
citypoints <- citypoints[-which(citypoints$TYPE == "artwork"),]
citypoints <- citypoints[-which(citypoints$TYPE == "Harut"),]
citypoints <- citypoints[-which(citypoints$TYPE == "taxi"),]
citypoints <- citypoints[-which(citypoints$TYPE == "Vazgen"),]
citypoints <- citypoints[-which(citypoints$TYPE == "Tigran"),]
citypoints <- citypoints[-which(citypoints$TYPE == "Manvel Danielyan"),]
citypoints <- citypoints[-which(citypoints$TYPE == "street_lamp"),]
citypoints <- citypoints[-which(citypoints$TYPE == "Arsen Matevosyan"),]
citypoints <- citypoints[-which(citypoints$TYPE == "Vladimir Melikyan"),]
citypoints <- citypoints[-which(citypoints$TYPE == "Karandash"),]
citypoints <- citypoints[-which(citypoints$TYPE == "company"),]
citypoints <- citypoints[-which(citypoints$TYPE == "kerb"),]
citypoints <- citypoints[-which(citypoints$TYPE == "mobile_phone"),]
citypoints <- citypoints[-which(citypoints$TYPE == "bench"),]
citypoints <- citypoints[-which(citypoints$TYPE == "motorway_junction"),]
citypoints <- citypoints[-which(citypoints$TYPE == "crossing"),]

citypoints <- citypoints[order(citypoints$TYPE),]

has_questionmark <- str_detect(citypoints$TYPE, "\\?")

citypoints[has_questionmark, "TYPE"] <- "missing"
citypoints <- citypoints[-which(citypoints$TYPE == "missing"),]
citypoints <- citypoints[-which(citypoints$TYPE == "Audit,_Tax,_Advisory"),]
citypoints <- citypoints[-which(citypoints$TYPE == "archaeological_site"),]
citypoints <- citypoints[-which(citypoints$TYPE == "furniture"),]
citypoints <- citypoints[-which(citypoints$TYPE == "substation"),]
citypoints <- citypoints[-which(citypoints$TYPE == "adult_gaming_centre"),]
citypoints <- citypoints[-which(citypoints$TYPE == "advertising_agency"),]
citypoints <- citypoints[-which(citypoints$TYPE == "alcohol"),]
citypoints <- citypoints[-which(citypoints$TYPE == "arts_centre"),]
citypoints <- citypoints[-which(citypoints$TYPE == "Artur"),]

baby_goods <- citypoints$TYPE == "baby_goods"
accessories <- citypoints$TYPE == "accessories"
bakeries <- citypoints$TYPE == "bakery"
clothes <- citypoints$TYPE == "clothes"
beauties <- citypoints$TYPE == "beauty"

citypoints[baby_goods,"TYPE"] <- "Shops"
citypoints[accessories,"TYPE"] <- "Shops"
citypoints[bakeries,"TYPE"] <- "Shops"
citypoints[clothes,"TYPE"] <- "Shops"
citypoints[beauties,"TYPE"] <- "Barbershops, beauty salons"

citypoints <- citypoints[order(citypoints$TYPE),]

citypoints <- citypoints[-which(citypoints$TYPE == "picnic_site"),]
citypoints <- citypoints[-which(citypoints$TYPE == "pitch"),]

citypoints <- citypoints[-which(citypoints$TYPE == "Azat"),]
citypoints <- citypoints[-which(citypoints$TYPE == "administrative"),]
citypoints <- citypoints[-which(citypoints$TYPE == "architect"),]
citypoints <- citypoints[-which(citypoints$TYPE == "attraction"),]
citypoints <- citypoints[-which(citypoints$TYPE == "association"),]

has_LLC <- str_detect(citypoints$TYPE, "\\LLC")
citypoints[has_questionmark, "TYPE"] <- "Shops"
citypoints <- citypoints[order(citypoints$TYPE),]

citypoints <- citypoints[-which(citypoints$TYPE == "community_centre"),]
citypoints <- citypoints[-which(citypoints$TYPE == "telecommunication"),]
citypoints <- citypoints[-which(citypoints$TYPE == "studio"),]
citypoints <- citypoints[-which(citypoints$TYPE == "stationery"),]
citypoints <- citypoints[-which(citypoints$TYPE == "bar"),]
citypoints <- citypoints[-which(citypoints$TYPE == "bicycle"),]
citypoints <- citypoints[-which(citypoints$TYPE == "bicycle_rental"),]
citypoints <- citypoints[-which(citypoints$TYPE == "bicycle_parking"),]

citypoints[citypoints$TYPE == "cafe","TYPE"] <- "Restaurants, cafe, fast-food"

citypoints <- citypoints[-which(citypoints$TYPE == "block"),]
citypoints <- citypoints[-which(citypoints$TYPE == "bookmaker"),]
citypoints <- citypoints[-which(citypoints$TYPE == "bureau_de_change"),]
citypoints <- citypoints[-which(citypoints$TYPE == "butcher"),]
citypoints <- citypoints[-which(citypoints$TYPE == "camp_site"),]

useless1 <- c(
"car_parts",
"car_rental",
"caravan_site",
"casino",                         
"chain",
"chemist",                        
"childcare",
"chocolate",
"communication",
"computer",                       
"comunication",
"copyshop",
"cosmetics",
"courthouse",                     
"coworking_space",
"curtain",                        
"customers",
"Davo & Aro",
"deli",                        
"Dinner_sets",
"doityourself", 
"drinking_water",
"dry_cleaning",
"embassy",
"employment_agency",              
"entrance",
"estate_agent",                   
"farm",                                                  
"fence",
"financial",                      
"FindArmenia",
"fitness_centre",                 
"florist",
"fountain",                       
"Friendship",
"GAG AUTO SERVICE",               
"gift",
"give_way",                       
"golf_course",
"government",                     
"greengrocer",
"Gregsys",                        
"Grigori Petrosyan",
"hardware",
"Hayk Mehrabyan",
"hifi",                           
"information",
"insurance",
"interior_decoration",            
"it",
"Karlen",
"Keheyan Manuk",    
"Kevorkian berjiklian",
"kissing_gate",
"laundry",
"lawyer",                         
"library",
"lift_gate",                      
"memorial",                       
"Mia Casa LLC",                   
"mini_roundabout",
"MINOTEL",                        
"Mo-Su 10:30-20:00",
"monument",                       
"motorcycle",                     
"musical_instrument",             
"Natal tati",
"newsagent",                     
"ngo",
"nightclub",                      
"no",
"optician"     
)

for(useless in useless1) {
  citypoints <- citypoints[-which(citypoints$TYPE == useless),]
}

useless2 <- c(
"outdoor", "parking",                        
"pawnbroker",
"pet",                            
"photo",                          
"playground","police",
"post_office",                    
"Printax.am",
"private",                        
"public",   
"public_bath",                                       
"Rafayel Khalatyan",
"recycling",                      
"research",
"Robert",
"ruins",           
"services",
"shelter",
"shooting",
"sports",
"sports_centre",
"stop",
"stop_position",                  
"tailor",                         
"tea",
"telecomunication",               
"telephone",                       
"ticket",                         
"tobacco",
"tobacco distributor",            
"toilets",
"townhall",                       
"traffic_sign",                   
"transformer",
"travel_agency",                  
"tyres",                                                
"Varduhi",                                               
"vending_machine",                                      
"video_games",
"viewpoint",                      
"Vxo",
"waste_basket",                   
"waste_disposal",
"weapons",                        
"wine",
"wool",
"yes",
"zoo")


for(useless in useless2) {
  citypoints <- citypoints[-which(citypoints$TYPE == useless),]
}

shops <- c(
"beverages",
"books",
"boutique",
"convenience",
"department_store",
"Hagusti Ashkhar",
"Hagusti Ashxarh",                
"household_goods",
"houseware",                     
"ice_cream",
"jewelry",
"kiosk",
"marketplace",
"perfumery",
"radiotechnics",
"seafood",
"shoes",
"Shops",
"Socks_Store",
"spices",
"toys",
"VegaGroup"
)

for (shop in shops) {
  citypoints[citypoints$TYPE == shop,"TYPE"] <- "Shops"
}

citypoints[citypoints$TYPE == "hairdresser","TYPE"] <- "Barbershops, beauty salons"
citypoints[citypoints$TYPE == "Paty Beauty Hall","TYPE"] <- "Barbershops, beauty salons"
citypoints[citypoints$TYPE == "supermarket","TYPE"] <-
  "Supermarkets, malls"
citypoints[citypoints$TYPE == "Sasgroup","TYPE"] <-
  "Supermarkets, malls"
citypoints[citypoints$TYPE == "MG supermarket","TYPE"] <-
  "Supermarkets, malls"
citypoints[citypoints$TYPE == "mall","TYPE"] <-
  "Supermarkets, malls"

hospitals <- c(
"clinic",
"dentist",
"doctors",
"hospital",
"medical_supply",
"pharmacy")

for (hospital in hospitals) {
  citypoints[citypoints$TYPE == hospital,"TYPE"] <-
    "Hospitals, pharmacies"
}

schools <- c("school", "kindergarten", "educational_institution", "college")


for (school in schools) {
  citypoints[citypoints$TYPE == school,"TYPE"] <-
    "Schools, learning centers, kindergartens"
}

citypoints[citypoints$TYPE == "pub","TYPE"] <-
  "Restaurants, cafe, fast-food"
citypoints[citypoints$TYPE == "fast_food","TYPE"] <-
  "Restaurants, cafe, fast-food"
citypoints[citypoints$TYPE == "restaurant","TYPE"] <-
  "Restaurants, cafe, fast-food"
citypoints[citypoints$TYPE == "theatre","TYPE"] <-
  "Cinema, theater"
citypoints[citypoints$TYPE == "cinema","TYPE"] <-
  "Cinema, theater"
citypoints[citypoints$TYPE == "university","TYPE"] <-
  "Universities"


useless3 <- c("Hilton", "hotel", "hostel", "guest_house", "motel", "museum",
"The Carlson Rezidor Hotel Group",
"bollard")

for(useless in useless3) {
  citypoints <- citypoints[-which(citypoints$TYPE == useless),]
}

citypoints[citypoints$TYPE == "bus_station","TYPE"] <-
  "Bus stop"
citypoints[citypoints$TYPE == "bus_stop","TYPE"] <-
  "Bus stop"
citypoints[citypoints$TYPE == "coffee","TYPE"] <-
  "Shops"
citypoints[citypoints$TYPE == "confectionery","TYPE"] <-
  "Shops"
citypoints[citypoints$TYPE == "electronics","TYPE"] <-
  "Shops"
citypoints[citypoints$TYPE == "veterinary","TYPE"] <-
  "Hospitals, pharmacies"
citypoints[citypoints$TYPE == "place_of_worship","TYPE"] <-
  "Place of worship"

citypoints <- citypoints[,-c(1:4)]

write.csv(citypoints, "clean_data.csv")

clean_data <- read.csv("clean_data.csv")
clean_data <- clean_data[,-1]

clean_data$points <- 10





register_google(key = "AIzaSyAgZOeeQEwe6ipaBMa2hKUOMeSn1vZkBek")


yerevan <- get_map(location = 'yerevan', zoom = 11, maptype = "terrain-lines")


pointed_yerevan <- ggmap(yerevan) +
  geom_point(data = citypoints, aes(lon, lat, color = TYPE))


interactive_map <- ggplotly(pointed_yerevan)


print(interactive_map)


