library(tidyverse)
library(rvest)
library(stringr)
library(viridis)
#---------------------------
# SCRAPE DATA FROM WIKIPEDIA
#---------------------------

df.lithium <- read_html("https://en.wikipedia.org/wiki/Lithium") %>%
  html_nodes("table") %>%
  .[[9]] %>%
  html_table() %>%
  as.tibble()



# INSPECT
df.lithium
#--------------------------------------------
# CHANGE COLUMN NAMES
# - the raw column names are capitalized and 
#   have some extra information
# - we will just clean them up
#--------------------------------------------

colnames(df.lithium) <- c('country', 'production', 'reserves', 'resources')

colnames(df.lithium)

#-----------------------------------------------
# REMOVE "World total"
# - this is a total amount that was
#   in the original data table
# - we need to remove, because it's not a
#   proper data record for a particular country
#-----------------------------------------------

df.lithium <- df.lithium %>% filter(country != 'World total')

df.lithium

#---------------------------------------------------------
# PARSE NUMBERS
# - the original numeric quantities in the table
#   were read-in as character data
# - we need to "parse" this information ....
#   & transform it from character into proper numeric data
#---------------------------------------------------------

# Strip out the 'notes' from the numeric data
#str_replace(df.lithium$production,"W\\[.*\\]", "") #test

df.lithium <- df.lithium %>% mutate(production = str_replace(production,"W\\[.*\\]", "-"))

# inspect
df.lithium


# Parse character data into numbers
df.lithium <- df.lithium %>% mutate(production = parse_number(production, na = '-')
                                    ,reserves = parse_number(reserves, na = '-')
                                    ,resources = parse_number(resources, na = '-')
)

# Inspect
df.lithium

#--------------
# GET WORLD MAP
#--------------

map.world <- map_data('world')
#----------------------------------------------------
# Get country names
# - we can use this list and cross-reference
#   with the country names in the scraped data
# - when we find names that are not the same between
#   this map data and the scraped data, we can recode
#   the values
#----------------------------------------------------

map_data('world') %>% group_by(region) %>% summarise() %>% print(n = Inf)
#--------------------------------------------
# RECODE COUNTRY NAMES
# - some of the country names do not match
#   the names we will use later in our map
# - we will re-code so that the data matches
#   the names in the world map
#--------------------------------------------

df.lithium <- df.lithium %>% mutate(country = if_else(country == "Canada (2010)", 'Canada'
                                                      ,if_else(country == "People's Republic of China", "China"
                                                               ,if_else(country == "United States", "USA"
                                                                        ,if_else(country == "DR Congo","Democratic Republic of the Congo", country))))
)


# Inspect
df.lithium
#-----------------------------------------
# JOIN DATA
# - join the map data and the scraped-data
#-----------------------------------------

df <- left_join(map.world, df.lithium, by = c('region' = 'country'))

#-----------
# PLOT DATA
#-----------

# BASIC MAP
basic_map<-ggplot(data = df, aes(x = long, y = lat, group = group)) +
  geom_polygon()


# LITHIUM RESERVES
lithium_reserves<-ggplot(data = df, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = reserves))


# LITHIUM PRODUCTION
lithium_production<-ggplot(data = df, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = production)) +
  scale_fill_viridis(option = 'plasma')


# LITHIUM RESOURCES
lithium_resources<-ggplot(data = df, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = resources)) +
  scale_fill_viridis(option = 'plasma')