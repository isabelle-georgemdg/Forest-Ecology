#create vector of lattitude and longitude from both population and soil
#for distance matrix

library(geosphere)
library(stringr)#included in tidyverse
table(kempAnemr$pop)
latlon_pop = cbind(site_p[c("long_pop", "lat_pop", "soil")])

latlon_pop = filter(latlon_pop, 
                    soil == "KMSF"|soil == "KSP"| soil == "KWW"|
                      soil == "LLW"| soil =="TTSF")

latlon_soil = cbind(site_s[c("long_soil", "lat_soil", "soil")])

latlon_soil = filter(latlon_soil, 
                     soil == "KMSF"|soil == "KNRS"| soil == "KWW"|
                       soil == "LLW"| soil =="NOW"|soil == "GTSP")
latlon_s=cbind(latlon_pop[c("long_pop", "lat_pop")])
latlon_p=cbind(latlon_soil[c("long_soil", "lat_soil")])

geo_dist = distm(latlon_s, latlon_p, fun = distGeo)
#add column and row names - R automatically sorts alphanumerically
geo_dist = cbind(geo_dist, latlon_pop$soil)
geo_dist = as.data.frame(geo_dist)
geo_dist =  geo_dist %>% 
  rename(
    GTSP = V1,
    KMSF = V2,
    KNRS = V3,
    KWW = V4,
    LLW = V5,
    NOW = V6,
    pop =V7)
#Reduce matrix to long data using gather
geo_long = gather(geo_dist, key = "soil", value = "distance", GTSP, NOW, KNRS, KWW, LLW, KMSF)

#merge with emmeans table
geo_long = geo_long %>% 
  mutate(soilxpop = paste(soil, pop, sep = "_"))
kempAnRgeo = merge(kempAnemr, 
                   geo_long[c("distance", "soilxpop")], by = "soilxpop")
