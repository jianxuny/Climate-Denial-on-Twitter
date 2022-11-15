#++++++++++++++++++++++++++++++++++++++++++++++++#
#++++++++++++++++++++++++++++++++++++++++++++++++#

#          GEOCODE CLIMATE CHANGE TWEETS
#            
#         Contact: yangjx@smail.nju.edu.cn

#++++++++++++++++++++++++++++++++++++++++++++++++#
#++++++++++++++++++++++++++++++++++++++++++++++++#

rm(list = ls())
library(tidyverse)
library(data.table)
library(stringr)
library(RJSONIO)
library(tidygeocoder)
library(sf)
library(ggplot2)
library(pbapply)
library(textclean)
library(postmastr)
library(tidygeocoder)

# replace with your own data path
data.path          <- "D:/DL_tweets_data/1_reduced_csv"
clean.add.path     <- "D:/DL_tweets_data/2_geocode_csv/preprocessing/clean_add_csv"
unique.add.path    <- "D:/DL_tweets_data/2_geocode_csv/preprocessing/unique_add_csv"
geocode.add.path   <- "D:/DL_tweets_data/2_geocode_csv/preprocessing/geocode_add_rds"
geocode.tweet.path <- "D:/DL_tweets_data/2_geocode_csv/preprocessing/geocode_tweets"

# -------------------------------------------------------- #
# ---- 1 - Define helper functions to clean tweet data  ----
# -------------------------------------------------------- #

# ---- Function(1) - Clean state abbreviation ----
# Build a transformer dictionary
postmastr::pm_dictionary(type = "state", case = "lower") %>%
  filter(str_length(state.input) > 2) %>% 
  filter(!state.output %in% c("AA","AE","AP","FM","GU","MP","PW","AS","MH","PR","VI")) %>%
  rename(stateabb = state.output, statefull = state.input) %>%
  mutate(stateabb = tolower(stateabb)) %>% 
  add_row(stateabb = "d.c.", statefull = "district of columbia") %>%
  add_row(stateabb = "georgia", statefull = "georgia") %>% 
  add_row(stateabb = "tennessee", statefull = "tennessee") %>% 
  add_row(stateabb = "hawaii-kohala coast-big island", statefull = "kohala") %>%
  add_row(stateabb = "the big river, missouri", statefull = "missouri") %>%
  add_row(stateabb = "central il", statefull = "illinois") %>%
  add_row(stateabb = "south jersey", statefull = "new jersey") %>%
  add_row(stateabb = "southwest or", statefull = "oregon") %>%
  add_row(stateabb = "central ohio", statefull = "ohio") %>%
  add_row(stateabb = "north texas", statefull = "texas") %>%
  mutate(regstr_1 = paste0("^", stateabb, "$")) %>%
  mutate(regstr_2 = paste0("[, ]", stateabb, "$")) -> statedict

# Define function to transform abbreviation to full state name
abb2full_state <- function(text){
  for(i in 1:nrow(statedict)){
    text <- gsub(pattern = statedict[i, "regstr_1"], 
                 replacement = paste0(statedict[i, "statefull"], ",usa"), x = text)
    text <- gsub(pattern = statedict[i, "regstr_2"], 
                 replacement = paste0(" ", statedict[i, "statefull"]), x = text)}
  return(text)}

# ---- Function(2) - Clean city abbreviation ----
# Build a transformer dictionary
cityabb <- c("l.a.", "nyc", "philly", "chicagoland", "sl,ut", "alt", "la la land",
             "big apple", "windy city", "phx", "okc", "cincy", "cle", "chi")
cityfull <- c("los angeles", "new york city", "philadelphia", "chicago", 
              "salt lake city", "atlanta", "los angeles", "new york city", 
              "chicago", "phoenix", "oklahoma city", "cincinnati", "cleveland", 
              "chicago")
citydict <- data.frame(cityabb, cityfull)

abb2full_city <- function(text){
  for(i in 1:nrow(citydict)){
    text <- gsub(pattern = paste0("([^a-z]|^)(", citydict[i, "cityabb"], ")([^a-z]|$)") ,
                 replacement = paste0(" ", citydict[i, "cityfull"], " "), x = text)}
  return(text)}

# ---- Function(3) - Remove fake addresses ----
fakeadd <- c("earth", "planet earth", "global", "globally l planet earth", 
             "worldwide", "she/her", "from screen to boardroom", "everywhere",
             "global", "nowhere", "here", "he/him", "hell", "right here......",
             "world", "antarctica", "north america", "planet earth", 
             "international", "espaa", "here & there on treaty 1 land",
             "hng global", "pnw", "#nofixedaddress", "murdochistan","honeymoongondola@gmail.com",
             "midwest", "rt's are fyi purposes only", "somewhere", "the world",
             "edmonton", "they/them", "gaia.", "she/they", "mars", "mt morgan",
             "mother earth", "east coast", "global citizen", "home", "pale blue dot",
             "staying global", "milky way", "southern hemisphere", "universe", "eu",
             "kiama", "manning point", "internet", "onfreedomroad.info","aust", "nationwide",
             "world citizen", "earth in peril.", "hogwarts", "3rd rock from the sun",
             "the rose red empire", "unknown", "hear their or anywhere", "the universe",
             "ho's asking?", "earth, present day", "left coast", "everywhere.",
             "#northsydneyvotes", "oz", "dja dja wurrung land. never ceded.", "kulin nation",
             "citizen of the world", "neverland", "bogot", "the internet", "the anthropocene",
             "lower orbit", "together we are the solution", "#extinction", "united", 
             "god's country", "the south", "r3y1x4", "https://democracy.town/about/",
             "midwest usa", "somewhere over the rainbow", "right behind you...", 
             "going for an interview?", "thule", "boon warrung", "world wide", 
             "the boardroom,", "lenapehoking", "every day is earth day", "parts unknown",
             "blue marble", "the great northwest", "wonderland", "world #cop13youth @unccd",
             "space", "twitter", "wakanda", "online", "living with kyroskoh (sg)",
             "ngunnawal country", "heaven", "dying earth", "heaven", "our beautiful world!",
             "a pale blue dot", "no planet b", "the world", "dish with one spoon territory",
             "earth citizen", "around", "up north planet earth", "paradise", "world",
             "pacific nw", "at home", "above the maginot line", "live anywhere haven't died yet",
             "stolen land", "blue mountains", "where #blacklivesmatter", "he/they",
             "the real world", "south", "planet #earth, mostly.", "terra", "down under",
             "she / her", "here and there", "earth, milky way", "the moon", 'outer space',
             "anarchist jurisdiction", "#eyeswideopen #knowyourchemistry", "the field")


clean_tweet_location <- function(tweet){
  #tweet[user_location!='' | place_name!='' | coord_lat!='',] -> tweet
  
  tweet[,user_location := iconv(user_location, "latin1", "ASCII", sub="")]
  tweet[,user_location := str_to_lower(user_location)]
  tweet[,user_location := gsub("http.*", "", user_location, ignore.case = T)]
  tweet[,user_location := gsub("www.*", "", user_location, ignore.case = T)]
  tweet[,user_location := gsub(".*\\.org.*", "", user_location, ignore.case = T)]
  tweet[,user_location := gsub(".*\\.net.*", "", user_location, ignore.case = T)]
  tweet[,user_location := gsub(".*\\.gov.*", "", user_location, ignore.case = T)]
  tweet[,user_location := gsub(".*\\.edu.*", "", user_location, ignore.case = T)]
  tweet[,user_location := gsub(".*\\.co.*", "", user_location, ignore.case = T)]
  tweet[,user_location := gsub(".*\\.ac.*", "", user_location, ignore.case = T)]
  tweet[, user_location := gsub(".*(he|him|she|her|they|them|it|its)\\/(he|him|she|her|they|them|it|its).*",
                                    "", user_location, ignore.case = T)]
  tweet[, user_location := gsub(".*(he|him|she|her|they|them|it|its) (he|him|she|her|they|them|it|its).*",
                                    "", user_location, ignore.case = T)]
  tweet[,user_location := gsub(".*insta.*", "", user_location, ignore.case = T)]
  tweet[,user_location := gsub(".*ig:.*", "", user_location, ignore.case = T)]
  tweet[,user_location := ifelse(user_location %in% fakeadd, "", user_location)]
  tweet[,user_location := abb2full_state(user_location)]
  tweet[,user_location := abb2full_city(user_location)]
  
  tweet %>%
    mutate(user_location = ifelse(grepl("[^0-9]0\\.0000+", user_location), NA, user_location)) %>%
    mutate(coord_location = str_extract(user_location, "(-?\\d{1,3}\\.\\d+, ?-?\\d{1,3}\\.\\d+)")) %>%
    mutate(user_location = ifelse(is.na(coord_location), user_location, coord_location)) %>%
    mutate(user_location = tolower(user_location)) %>%
    select(-coord_location) -> tweet
  
  tweet[,user_location := gsub("[^a-z0-9 ,-.]", " ", user_location)] 
  tweet[,user_location := str_squish(user_location)]
  tweet[,user_location := str_trim(user_location)]
  
  #tweet[user_location!='' | place_name!='' | coord_lat!='',] -> tweet
  return(tweet)}

# ------------------------------------- #
# ------ 2 - Clean tweet dataset ------
# ------------------------------------- #
list.files(path = data.path, pattern = "*.csv") -> files
files <- files[order(nchar(files),files)] 

for (i in 4:length(files)) {
  tweet <- fread(file.path(data.path, files[i]))
  tweet <- clean_tweet_location(tweet)
  
  gsub("\\.(csv)", "_p1.csv", files[i]) -> FileName
  fwrite(tweet, file.path(clean.add.path, FileName) )
  cat("Clean File #", i, "\r")
}
rm(tweet)

# --------------------------------------------------- #
# ---- 3 - Extract unique addresses and user_id  ----
# --------------------------------------------------- #

list.files(path = clean.add.path, pattern = "*.csv") -> files
files <- files[order(nchar(files),files)] # order file names

# Mergedata and Select All the Geo Attributes
merge_df <- function(filepath){
  tweet <- fread(filepath) %>%
    dplyr::select(user_id_str, user_location, 
                  place_type, place_name, place_country, 
                  coord_lon, coord_lat)
  return(tweet)
}

tweets_list <- lapply(file.path(clean.add.path, files), merge_df)
tweets_all  <- rbindlist(tweets_list, use.names=TRUE)

# Extract all the Unique User ID & location
tweets_all %>% 
  group_by(user_id_str, user_location) %>%
  summarise(count = n()) %>%
  arrange(desc(count))-> user_id_location

fwrite(user_id_location, file.path(unique.add.path, "/user_id_location.csv"))

# Extract all the Unique User Locations
tweets_all %>% 
  group_by(user_location) %>%
  summarise(count = n()) %>%
  arrange(desc(count))-> unique_add

fwrite(unique_add, file.path(unique.add.path, "/unique_add_new.csv"))

# Extract all the Unique Place 
tweets_all %>%
  filter(place_country == "United States") %>%
  filter(place_type != "country") %>%
  group_by(place_name) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) -> unique_place
fwrite(unique_place, file.path(unique.add.path, "/unique_place.csv")) 

# Extract all the Unique User place & location 
tweets_all %>%
  filter(place_name != '') %>%
  group_by(user_id_str, place_name) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) -> user_id_place


fwrite(unique_place, file.path(unique.add.path, "/unique_id_place.csv")) 

# Split into separate files for geocoding
#fwrite(unique_add[1:200000,], file.path(unique.add.path,"unique_location_1.csv"))
#fwrite(unique_add[200001:400000,], file.path(unique.add.path,"unique_location_2.csv"))
#fwrite(unique_add[400001:600000,], file.path(unique.add.path,"unique_location_3.csv"))
#fwrite(unique_add[600001:nrow(unique_add),], file.path(unique.add.path,"unique_location_4.csv"))

# --------------------------------------------------------------- #
# ---- 4 - Geocode Unique User Location Using Nominatim API  ----
# --------------------------------------------------------------- #

unique_add <- fread(file.path(unique.add.path, "/unique_add_new.csv"))
location_list <- as.list(unique_add$user_location)
add_encode <- lapply(unique_add$user_location, URLencode)
make_url <- function(url_encode){
  url_api = paste0("https://nominatim.openstreetmap.org/search/",
                   url_encode,
                   "?format=json",
                   "&addressdetails=1&extratags=0&limit=1",
                   "&accept-language=en")
  return(url_api)}
add_url <- lapply(add_encode, make_url)

# load JSON files from nominatim api 

iter_count = length(add_url) %/% 10000
last_count = length(add_url) %% 10000
for(i in 1:(iter_count+1)){
  if(i <= iter_count){
    print(i)
    print(add_url[10000*(i-1)+1])
    json_list <- pblapply(add_url[seq(10000*(i-1) + 1, 10000*i, 1)], fromJSON)
    saveRDS(json_list, paste0(unique.add.path, "/location_list_", i, ".rds"))
  }
  if(last_count > 0 & i > iter_count){
    print(i)
    json_list <- pblapply(add_url[seq(10000*(i-1) + 1, 10000*(i-1) + last_count, 1)], fromJSON)
    saveRDS(json_list, paste0(unique.add.path, "/location_list_", i, ".rds"))
  }
}

# ------------------------------------- #
# ---- 5 - Load Address from Json  ----
# ------------------------------------- #

files <- list.files(path = geocode.add.path, pattern = "*.rds") 
files <- files[order(nchar(files),files)] 
geocode_list <- list()

for(j in 1:length(files)) {
  
  osm_json <- readRDS(file.path(geocode.add.path, files[j]))
  column_names = c("geocode_name", "geocode_lat", "geocode_lon", "geocode_country",
                   "geocode_state", "geocode_city", "geocode_county", "geocode_town", 
                   "geocode_office", "geocode_adm")
  geocode_res <- data.frame(matrix(nrow = 0, ncol = 10))
  colnames(geocode_res) <- column_names
  
  for (i in 1:length(osm_json)) {
    if(length(osm_json[[i]]) == 0){
      geocode_res[nrow(geocode_res)+1,] <- NA}
    
    if(length(osm_json[[i]]) != 0){
      geocode_name <- osm_json[[i]][[1]]$display_name
      geocode_lat  <- osm_json[[i]][[1]]$lat
      geocode_lon  <- osm_json[[i]][[1]]$lon
      
      add_info     <- names(osm_json[[i]][[1]]$address)
      geocode_country <- ifelse("country" %in% add_info, 
                                osm_json[[i]][[1]]$address['country'], NA)
      geocode_state   <- ifelse("state" %in% add_info, 
                                osm_json[[i]][[1]]$address['state'], NA)
      geocode_city    <- ifelse("city" %in% add_info, 
                                osm_json[[i]][[1]]$address['city'], NA)
      geocode_county  <- ifelse("county" %in% add_info, 
                                osm_json[[i]][[1]]$address['county'], NA)
      geocode_town    <- ifelse("town" %in% add_info, 
                                osm_json[[i]][[1]]$address['town'], NA)
      geocode_adm     <- ifelse("administrative" %in% add_info, 
                                osm_json[[i]][[1]]$address['administrative'], NA)
      geocode_office  <- ifelse("office" %in% add_info, 
                              osm_json[[i]][[1]]$address['office'], NA)
      
      df <- data.frame(geocode_name, geocode_lat, geocode_lon,
                       geocode_country, geocode_state, geocode_city, geocode_county,
                       geocode_town, geocode_office, geocode_adm)
      geocode_res <- rbind(geocode_res, df)}
  } 
  geocode_list <- c(geocode_list, list(geocode_res))
  
  print(j)
}

geocode_add <- rbindlist(geocode_list)
unique_add_geocode <- cbind(unique_add, geocode_add) 


# ------------------------------------------- #
# ---- 6 - Clean and add state/local tags ----
# ------------------------------------------- #

fake_add  <- c("dmv", "middle earth", "latin america", "nature", "out west", "braslia",
               "in your heart", "disneyland", "final capstone", "empire state", 
               "united states of resistance", "lost in space", "not found",	
               "south wales", "elsewhere", "everywhere, usa", "rural ontario",
               "the future, usa" , "americas finest city", "southeast usa", "everywhere usa",
               "nowhere middle", "great american northwest", "arctic", "venus",
               "the earth", "gotham city", "usa and global", "earth.", "southeastern usa",
               "aus", "middle america", "heartland usa", 'great northwest, usa',
               "southern usa", "southern us", "earth", "ring of pride", "your dreams",
               "royalty", "planet blue", "jamaica, w.i.", "astroworld", "the seven seas",
               "canadian", "international space station", "usa italy", "heartland",
               "pnw, usa", "great nw", "belfast and dublin", "on the road, usa",
               "field or classroom", "north by northwest", "deep in a blue state",
               "all over north america", "usa, earth", "southeastern united states",
               "outer and inner space place", "earth, ne usa", "earth, usa", "usa earth",
               "earth..", "village earth", "earth...", "spaceship earth", "jupiter",
               "atlantis", "mediterranean", "eye of the storm", "the eye of the storm",
               "minding my business", "no lists", "the woods", "the swamp", "space force",
               "inside the loop", "the mendoza line", "kingdom of god", "flyover usa",
               "somewhere in paradise", "your imagination", "disney world","fire is multi-national",
               "the tree of life", "us, australia the world", "swamp", "here there",
               "left coast, usa", "the left coast", "usa europe", "the library",
               "oxford", "north wales", "switze", "red state misery", "my laboratory",
               "follows you", "nightmare", "not your business", "in the kitchen",
               "utopia", "per", "nowhere in particular","left coast usa", "left coast of the us",
               "planet earth.", "planet earth", "roaming", "the planet earth", 
               "the south, usa", "usa south", "happydale", "stark tower", "she",
               "corporate america", "usa, planet earth", "fly over state, usa",
               "in a paradigm shift", "oh no", "north coast usa", "the ottawa valley",
               "south, usa", "on your left.", "a parallel universe", "parallel universe",
               "not of this world", "humane society", "in the trenches", "somewhere in time",
               "space, the final frontier", "linda jones", "between", "six feet under",
               "between airports", "somewhere in time..", "warm in bed", "just around the corner...",
               "adult neurology", "not of this world.", "spain us", "the ether",
               "in the blue sky", "retired", "in the ether", "ether", "in the crawlspace",
               "the long defeat", "1987-2491", "the wildwoods", "follow us", "the berkshires",
               "talkradio", "54.94837 n -1.376212 w", "tri-state", "internationalist",
               "united states of smash", "the west country", "parts unknown, canada",
               "biden-harris, usa", "the lone and level sands", "ottawa centre",
               "thailand and usa", "pennslyvania", "hell", "the edge of reality",
               "hockeytown", "hell.", "east coast, usa", "perth washington", "great white north",
               "east coast usa", "east coast us", "the great white north", "neptune",
               "trump country", "east of the atlantic", "god bless america", "yorkshire",
               "usa - worldwide", "appalachia", "complex vector space", "secret lair",
               "in quarantine", "9th circle of hell", "tosche station", "united states of awesome",
               "red state hell", "in the secret lair.", "stumptown", "in my lane",
               "little rhody", "knowhere", "duwamish land, washington", "greater vancouver",
               "space oddity", "vancouver, b.c.", "state of confusion", "pacific",
               "she they", "the sea", "sea", "citizen, usa", "yonder", "in the wilderness",
               "citizen", "down south", "itinerant", "king s landing", "we are where we are",
               "cloud 9", "north west alberta", "unofficial", "earth orbit", "globe",
               "god s country, usa", "why", "always moving", "future earth", "across america",
               "the continental", "the blue marble", "commonwealth of the west",
               "middle east", "northeastern us", "state", "the interwebs", "on the grid",
               "out and about", "she her", "here.in front of you.", "spain mostly",
               "disney", "infinite possibilities", "she her 18", "she her 19", "she her 20",
               "she her 21", "in your dreams", "on the moon", "on the lake", "all around you",
               "in your dreams...", "mid of west in the us", "lebanon geneva", "in a bookstore",
               "a good place", "btown", "on my throne", "in the stacks", "naptown",
               "art", "no clue", "proud indian", "all around you.", "delaware projects",
               "indian and global", "the neverglades", "i am of ireland", "home, where else",
               "home. where else", "lost in the usa", "on a dirt road", "on a cloud",
               "in the hollow", "in a galaxy", "west london", "couch", "east yorkshire",
               "united states of democracy", "show me state", "my secret garden",
               "dreamweaver", "my bedroom", "bunker", "heart of america", "the simulation",
               "local", "usa-ish...", "sky", "the couch", "state farm", "east yorkshire.",
               "near enough", "patch", "turn around", "contrary land", "suburbia",
               "land of 10,000 lakes", "all 50 states", "us midatlantic", "social media land",
               "trump", "land of blue smoke", "lenape land", "unicorn land", "human land",
               "i get around.", "trump usa", "hoosier land", "out of place", "u.s.",
               "treaty 6 territory", "somewhere, usa", 'gotham', "deep space 9", "somewhere usa",
               "somewhere in the usa", "somewhere in the south, usa", 'somewhere in the us',
               "somewhere in usa", "somewhere in united states", "somewhere in the forest",
               "winter", "exile", "united states of the i-5", 'small town, usa', "small town usa",
               "u.s", "treaty 1 territory", "the wilderness of the lost.", "deep space",
               "treaty 8 territory", "treaty six territory", "glorious usa", "is the price of",
               "crazy town usa", 'usa 220', "treaty one territory", "usa baby", "us and australia",
               "america the beautiful", "blue in a red state", "deep south", "the garden state",
               "second star to the right", "at the beach", 'far far away', "sky above you",
               "usa, canada, worldwide", "the deep south", "back in the usa", "way out west",
               "australia us", "shared spaces", "second star to the right...", "heaven on earth",
               "orion spur", "the face of the sun", "guardian", "the guardian", "on the cloud",
               "a blue america", "border state", "indoors", "middle earth fl", "that london",
               "not home", "the rock", 'asia pacific', "asia-pacific", "quarantine",
               "galaxy", "parler", "france usa", "the galaxy", "the whole wide world",
               "stuck in 1984.", "375 million acres", "in the shadows", 'america, usa',
               "my america", 'india, mobile 91-9650026623', "hell on earth", 'arab states',
               "usa america", "america usa", 'on ya burger', 'village hidden in the leaves',
               "minor", "black", "the sunny south,", "america usa.", "on an island",
               "your moms", "yikes", "canada usa", "eastern us", "usa canada", 
               "northern united states", "canada - usa", "a commonwealth", "canada and united states",
               "united states and canada", "northern forest usa", "usa canada uk",
               "sunny glasgow", "brigadoon", "eastern usa", "my feelings", "united states canada",
               "wibble", "the breeding swan", "canada and usa", "the dark side",
               "lex", "canada us", "northern usa", "right here", "in the clouds",
               "your heart", "remote", "airstrip one", "right here.", "my bag",
               "desert, earth", "remote island", "blue state, usa", "cosmopolitan",
               "at the nexus", "next adventure", "up in the clouds", "in a dream",
               "north central u.s.", "let s save", "sign up", "lost at sea", "reclaiming us",
               "38,000 ft", "lake country b.c.", "the edge of the earth", "central europe",
               "deep red south", "center of the universe", "midwest red", "taco bell",
               "midwest red state", "the road not taken", "midwest america", "center of universe",
               "my desk", "god s heart", "islands", "the admiral s arms", "behind enemy lines",
               "jerz", "usa for now", "in the 900 s", "osaka japan", "the moon, space",
               "good country canada", "the colonies", "usa... for now", "waterloo",
               "upper midwest, usa", "upper midwest", "miracle planet", "usa, eu",
               "your mom s", "dead center of the usa", "wherever", "the future", "heart",
               "mr rogers neighborhood", "wherever i am, united states", "on set",
               "on the set", "in a hammock.", "upstairs", "future", "on da rez, usa",
               "your closet", "in your closet", "you never know", "the cloud", "southwest us",
               "nowhere, usa", "cloud", "south florida, nevada", "usa cr", "atelier meridian",
               "southwest usa", "southwest-usa", "nowhere usa", "southwest, usa",
               "nowhere.", "ja", "south of nowhere", "west of nowhere", "nowhere",
               "nowhere,usa", "us southwes", "usa can", "sw usa", "in transit", "gta",
               "earth one", "blue state thank god", "in transit.", "a failed state",
               "crisis is real", "failed state", "in the silt.", "rd rock from the sun.",
               "western us", "western usa", "western united states", "the valley",
               "well-grounded", "off", "america s heartland", "inland northwest",
               "western, usa", "pants", "national", "pluto", "location", "lost in the sauce",
               "vegan", "united state", "the kitchen", "always usa", "kitchen","man of the united states",
               "the great outdoors", "lost in the world", "giliad", "greatest","lost to the ages",
               "warrior cabin", "the six", "lost in a book", "usa reds", "traveling usa",
               "bliss", "fired", "magic", "always on the", "twilight zone", "top of the world",
               "i love your dog", "the outer limits", "outer limits", "freedom, usa",
               "freedom", "from sea to shining sea", "gilead", "usa romans 12 21", "my head",
               "crying in the club", "purgatory", "sowams", "north america and world",
               "cloud 13", "centered", "beach", "west of the divide", "mountain",
               "kln", "wnc", "landlocked", "alone", "central usa", "lost sea", "wnc, usa",
               "tatooine", "tatooine...", "around and about", "lagging behind",
               "lgbtq", "victory", "black lives matter", "a child of the universe",
               "american commons", "house", "black lives matter.", "the old north",
               "a station near you", "near coffee", "a state of reason", "states of america","south hampshire",
               "in the usa", "turtle island", "this spaceship earth", 'earth, the blue planet'
               )

incor_add <- c("twin cities", "texas hill country", "east texas", "east texas usa", "north texas, usa", "north texas area",
               "massachussetts, usa", "north florida", "northwest florida","nw florida","state of florida", "south florida","south florida, usa","west central florida", "sunny south florida", "s. florida",
               "sweet home alabama",  "north alabama", "mia", "chicago, il south side", "west philadelphia", "delco", #Delaware County, Pennsylvania
               "the natural state, usa", "washington metro", "music city, usa", #nashville, tn
               "carmel, in usa", "my town, kentucky", "farmington, ct usa", "california republic", "southern cal",
               "evergreen state",   "us or, washington","washington, usa", "washington,usa", "washington", "sw washington", "bull city" ,# duram
               "washington, d.c. metro area", "nw ohio", "buckeye state", #ohio
               "sl utah", "kirkville, n.y.", "new york new york","burt n.y.", 
               "the mitten", "mitten state, usa", "the mitten state", "mitten state", "near detroit", "west michigan", "west michigan, usa",
               "south mississippi", "northwest indiana",  "trego, mt, usa", "concord, ma usa", 
               "foster, va.", "burke, va, usa", "herndon, va, usa", "central, ct - usa",
               "northern california", "socal", "norcal", "northern virginia", "so cal", "boston area", "dc metro area", "madison wisconsin",
               "central florida", "san antonio texas", "chicago area", "stl", "metropolitan chicago", "the great state of texas",
               "district of columbia,usa", "district of columbia, usa"
               )
corr_add <- c("minneapolis-saint paul", "texas, us", "texas, us", "texas, us", "texas, us", "texas, us", "massachusetts, us",
              "florida, us","florida, us", "florida, us", "florida, us",  "florida, us", "florida, us", "florida, us", "florida, us", "florida, us", 
              "alabama, us", "alabama, us", "miami, fl", "chicago, il", "philadelphia, pa", "delaware county, pennsylvania", "arkansas, us",
              "washington dc", "nashville, tennessee", "carmel, indiana", "kentucky, us", "farmington, connecticut", "california, us","california, us",
              "washington state, us", "washington state, us", "washington state, us", "washington state, us", "washington state, us", "washington state, us", 
              "durham, north carolina", "washington dc", "ohio, us", "ohio, us", "salt lake city, utah", 'kirkville, new york', "new york, new york",
              "burt, new york", "michigan, us", "michigan, us", "michigan, us","michigan, us","detroit, michigan","michigan, us","michigan, us",
              "mississippi, us", "indiana, us", "trego, montana", "concord, massachusetts", "foster, virginia", "burke, virginia", "herndon,virginia","connecticut, us",
              "california, us", "california, us", "california, us", "virginia, us", "california, us", "boston, massachusetts", "washington dc",
              "madison, wisconsin", "florida, us", "san antonio, texas", "chicago, il", "st. louis, missouri", "chicago, il", "texas, us",
               "washington dc", "washington dc")

incordf <- data.frame(incor_add, corr_add)
incordf_list <- as.list(incordf$corr_add)

make_url <- function(url_encode){
  url_api = paste0("https://nominatim.openstreetmap.org/search/",
                   url_encode,
                   "?format=json",
                   "&addressdetails=1&extratags=0&limit=1",
                   "&accept-language=en")
  return(url_api)}
add_url <- lapply(incordf_list, make_url)
osm_json <- pblapply(add_url, fromJSON)


column_names = c("geocode_name", "geocode_lat", "geocode_lon", "geocode_country",
                   "geocode_state", "geocode_city", "geocode_county", "geocode_town", 
                 "geocode_office", "geocode_adm")
geocode_res <- data.frame(matrix(nrow = 0, ncol = 10))
colnames(geocode_res) <- column_names
  
for (i in 1:length(osm_json)) {
  if(length(osm_json[[i]]) == 0){
      geocode_res[nrow(geocode_res)+1,] <- NA}
    
  if(length(osm_json[[i]]) != 0){
      geocode_name <- osm_json[[i]][[1]]$display_name
      geocode_lat  <- osm_json[[i]][[1]]$lat
      geocode_lon  <- osm_json[[i]][[1]]$lon
      
      add_info     <- names(osm_json[[i]][[1]]$address)
      geocode_country <- ifelse("country" %in% add_info, 
                                osm_json[[i]][[1]]$address['country'], NA)
      geocode_state   <- ifelse("state" %in% add_info, 
                                osm_json[[i]][[1]]$address['state'], NA)
      geocode_city    <- ifelse("city" %in% add_info, 
                                osm_json[[i]][[1]]$address['city'], NA)
      geocode_county  <- ifelse("county" %in% add_info, 
                                osm_json[[i]][[1]]$address['county'], NA)
      geocode_town    <- ifelse("town" %in% add_info, 
                                osm_json[[i]][[1]]$address['town'], NA)
      geocode_adm     <- ifelse("administrative" %in% add_info, 
                                osm_json[[i]][[1]]$address['administrative'], NA)
      geocode_office  <- ifelse("office" %in% add_info, 
                                osm_json[[i]][[1]]$address['office'], NA)
      
      df <- data.frame(geocode_name, geocode_lat, geocode_lon,
                       geocode_country, geocode_state, geocode_city, geocode_county,
                       geocode_town, geocode_office, geocode_adm)
      geocode_res <- rbind(geocode_res, df)}
} 

incor_add %>%
  data.frame(.) %>%
  left_join(., unique_add_geocode, by = c("." = "user_location")) %>%
  rename(user_location = ".") %>%
  select(user_location, count) %>%
  cbind(.,geocode_res) -> corrected_add

fuzzy_state <- c("new york, usa", "southern california","northern california, usa",
                 "southern california, usa", "west coast, usa", "western north america",
                 "so california", "california global", "so. california", "nor cal",
                 "norcal, usa", "new york", "upstate new york", "central new york",
                 "upstate, new york", "upstate new york, usa", "upstate", "republic of texas",
                 "central texas", "south texas", "texas florida", "the lone star state",
                 "west texas", "somewhere in texas", "blue texas forever", "the republic of texas",
                 "lone star state", "south central texas", "heart of texas", "texas forever",
                 "deep in the of texas", "sw florida", "ne florida", "somewhere in florida",
                 "central florida, usa", "so. florida", "so florida", "central illinois",
                 "southern illinois, usa", "southern illinois", "state of ohio",
                 "western pennsylvania", "southeastern pennsylvania", "north east pennsylvania",
                 "eastern pennsylvania", "southeast pennsylvania", "southwest ohio",
                 "somewhere in ohio", "north star state", "the great state of ohio",
                 "yeah georgia", "north georgia", "middle georgia", "n. georgia",
                 "western north carolina", "eastern north carolina", "rural north carolina",
                 "nc, republic", "northern michigan", "upper michigan", "mid-michigan",
                 "northern new jersey", "north jersey", "pacific northwest", "pacific northwest usa",
                 "wa state", "pacific nw.", "the pacific northwest", 'the other washington',
                 "the bay state", "east tennessee", "east tennessee, usa", "e.tennessee, usa",
                 "blue tennessee", "rural missouri", "wis. u.s", "se wisconsin",
                 "alabama u.s.", "somewhere in alabama", "southwest oregon", "oregon, the blue part",
                 "northwest ohio", "blue virginia", "lost in pennsylvania"
                 )



US_add_geocode <- unique_add_geocode %>%
  filter(! user_location %in% incor_add) %>%
  rbind(., corrected_add) %>%
  mutate(geocode_city    = ifelse(user_location %in% fuzzy_state, NA, geocode_city),
         geocode_county  = ifelse(user_location %in% fuzzy_state, NA, geocode_county)) %>%
  mutate(geocode_country = ifelse(user_location %in% fake_add, NA, geocode_country),
         geocode_state   = ifelse(user_location %in% fake_add, NA, geocode_state),
         geocode_city    = ifelse(user_location %in% fake_add, NA, geocode_city),
         geocode_county  = ifelse(user_location %in% fake_add, NA, geocode_county)
         ) %>%
  filter(geocode_country == "United States") %>%
  mutate(sp_scale = case_when(is.na(geocode_state) & is.na(geocode_county) & is.na(geocode_city) 
                              & is.na(geocode_town) & is.na(geocode_office) & is.na(geocode_adm) ~ "country",
                              
                              !is.na(geocode_state) & is.na(geocode_county) & is.na(geocode_city) 
                              & is.na(geocode_town) & is.na(geocode_office) & is.na(geocode_adm) ~ 'state',
                              
                              !is.na(geocode_county) | !is.na(geocode_city) |
                              !is.na(geocode_town) | !is.na(geocode_office) | !is.na(geocode_adm) ~ 'local')) %>%
  arrange(desc(count))

fwrite(US_add_geocode, file.path(unique.add.path, "US_add_geocode.csv"))

# --------------------------------------------------------------- #
# ---- 7 - Geocode Unique Place Attribute Using Nominatim API  ----
# --------------------------------------------------------------- #

unique_place <- fread(file.path(unique.add.path, "unique_place.csv"))


place_encode <- lapply(unique_place$place_name, URLencode)
make_url <- function(url_encode){
  url_api = paste0("https://nominatim.openstreetmap.org/search/",
                   url_encode,
                   "?format=json",
                   "&addressdetails=1&extratags=0&limit=1",
                   "&accept-language=en",
                   "&countrycodes=us")
  return(url_api)}
place_url <- lapply(place_encode, make_url)

# load JSON files from nominatim api
place_url[6569][[1]] = "https://nominatim.openstreetmap.org/search/WhatIf?format=json&addressdetails=1&extratags=0&limit=1&accept-language=en"
place_url[9130][[1]] = "https://nominatim.openstreetmap.org/search/Pearson%20Hall%20?format=json&addressdetails=1&extratags=0&limit=1&accept-language=en"
json_list <- pblapply(place_url, fromJSON)
saveRDS(json_list, file.path(unique.add.path, "place_geocode.rds"))

osm_json <- readRDS(file.path(unique.add.path, "place_geocode.rds"))
column_names = c("geocode_name", "geocode_lat", "geocode_lon", "geocode_country",
                   "geocode_state", "geocode_city", "geocode_county", "geocode_town", 
                   "geocode_office", "geocode_adm")
geocode_res <- data.frame(matrix(nrow = 0, ncol = 10))
colnames(geocode_res) <- column_names
  
for (i in 1:length(osm_json)) {
    if(length(osm_json[[i]]) == 0){
      geocode_res[nrow(geocode_res)+1,] <- NA}
    
    if(length(osm_json[[i]]) != 0){
      geocode_name <- osm_json[[i]][[1]]$display_name
      geocode_lat  <- osm_json[[i]][[1]]$lat
      geocode_lon  <- osm_json[[i]][[1]]$lon
      
      add_info     <- names(osm_json[[i]][[1]]$address)
      geocode_country <- ifelse("country" %in% add_info, 
                                osm_json[[i]][[1]]$address['country'], NA)
      geocode_state   <- ifelse("state" %in% add_info, 
                                osm_json[[i]][[1]]$address['state'], NA)
      geocode_city    <- ifelse("city" %in% add_info, 
                                osm_json[[i]][[1]]$address['city'], NA)
      geocode_county  <- ifelse("county" %in% add_info, 
                                osm_json[[i]][[1]]$address['county'], NA)
      geocode_town    <- ifelse("town" %in% add_info, 
                                osm_json[[i]][[1]]$address['town'], NA)
      geocode_adm     <- ifelse("administrative" %in% add_info, 
                                osm_json[[i]][[1]]$address['administrative'], NA)
      geocode_office  <- ifelse("office" %in% add_info, 
                                osm_json[[i]][[1]]$address['office'], NA)
      
      df <- data.frame(geocode_name, geocode_lat, geocode_lon,
                       geocode_country, geocode_state, geocode_city, geocode_county,
                       geocode_town, geocode_office, geocode_adm)
      geocode_res <- rbind(geocode_res, df)}
  } 

unique_place_geocode <- cbind(unique_place, geocode_res) 
unique_place_geocode %>%
  mutate(sp_scale = case_when(is.na(geocode_state) & is.na(geocode_county) & is.na(geocode_city) 
                            & is.na(geocode_town) & is.na(geocode_office) & is.na(geocode_adm) ~ "country",
                            
                            !is.na(geocode_state) & is.na(geocode_county) & is.na(geocode_city) 
                            & is.na(geocode_town) & is.na(geocode_office) & is.na(geocode_adm) ~ 'state',
                            
                            !is.na(geocode_county) | !is.na(geocode_city) |
                              !is.na(geocode_town) | !is.na(geocode_office) | !is.na(geocode_adm) ~ 'local')) %>%
  arrange(desc(count)) %>%
  filter(!is.na(geocode_country))-> unique_place_geocode


fwrite(unique_place_geocode, file.path(unique.add.path, "unique_place_geocode.csv"))

# ---------------------------------------- #
# ---- 8 - Join every unique user id  ----
# ---------------------------------------- #

user_id_location <- fread(file.path(unique.add.path, "user_id_location.csv"))
unique_add_geocode <- fread(file.path(unique.add.path, "US_add_geocode.csv"))
unique_place_geocode <- fread(file.path(unique.add.path, "unique_place_geocode.csv"))

# Join by user location
user_id_location %>%
  select(-count) %>%
  left_join(., y = unique_add_geocode, by = "user_location") %>%
  filter(!is.na(geocode_country) & sp_scale != "national") -> US_user_location

user_id_location %>%
  select(-count) %>%
  left_join(., y = unique_add_geocode, by = "user_location") %>%
  filter(is.na(geocode_country) | sp_scale != "local" ) -> non_US_user_location

# If user does not have location attribute, search for the place information
user_id_place %>%
  select(-count) %>%
  left_join(., y = unique_place_geocode, by = "place_name") %>%
  filter(!is.na(geocode_country)) %>%
  rename(user_location = place_name) -> US_user_place
  
non_US_user_location %>%
  select(user_id_str) %>%
  left_join(., US_user_place, by = 'user_id_str') %>%
  filter(sp_scale %in% c("state", "county")) -> new_US_user_location

# Make the final dictionary
final_id_location <- rbind(US_user_location, new_US_user_location) %>%
  arrange(sp_scale) %>%
  select(user_id_str, user_location, geocode_lat, geocode_lon, geocode_state, sp_scale) %>%
  distinct(user_id_str, .keep_all = T) 
  
fwrite(final_id_location, file.path(unique.add.path, "final_id_geocode.csv"))


# ---------------------------------------- #
# ---- 9 - Join back to raw dataset  ----
# ---------------------------------------- #

list.files(path = data.path, pattern = "*.csv") -> files
files <- files[order(nchar(files),files)] 
final_id_location <- fread(file.path(unique.add.path, "final_id_geocode.csv"))

for (i in 1:length(files)) {
  tweet <- fread(file.path(data.path, files[i]))
  tweet <- tweet %>%
    select(-user_location) %>%
    left_join(., final_id_location, by = "user_id_str") %>%
    filter(!is.na(sp_scale))
  
  FileName <- paste0("geocode_", i, ".csv") 
  fwrite(tweet, file.path(geocode.tweet.path, FileName) )
  cat("Clean File #", i, "\r")
}


list.files(path = geocode.tweet.path, pattern = "geocode_\\d") -> files
files <- files[order(nchar(files),files)] 
setwd(geocode.tweet.path)
filelist <- lapply(files, fread) 

geocode_tweets <- rbindlist(filelist, use.names = T)
geocode_tweets %>%
  filter(sp_scale == 'local') %>%
  fwrite(file.path(geocode.tweet.path, "county_tweet.csv"))

geocode_tweets %>%
  filter(sp_scale == 'state') %>%
  fwrite(file.path(geocode.tweet.path, "state_tweet.csv"))
