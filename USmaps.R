library(ggplot2)
library(readxl)
library(tidyverse)
library(plyr)
library(dplyr)
library(lme4)
library(lmerTest)
library(usmap)
library(ggrepel)

Site <- read_excel("Y:/ppath/labs/Lankau_Lab/Forest Ecology/Soil Results/Copy of soil site climate data for R.xlsx")

Site <- Site %>% 
  rename(
    lon = long,
    lat = lat) 
Site_t <- Site %>%
  select(lon,lat, Soil, aridity_tho_sum, exp)
#Site_t <- Site_t[,c(3,2,1,4, 5)]
Site_t <- usmap_transform(Site_t)
Site_qr <- subset(Site_t, Soil != "TTSF" & Soil != "KMSF" & Soil != "LLW")
Site_aneg <- subset(Site_t, Soil != "SSSP" & Soil != "GTSP" & Soil != "KNRS" & Soil != "NOW")
Site_field = subset(Site_t, Soil %in% c("APR", "KNRS","KMSF", "KWW", "KSP", "LLW", "TTSF"))

plot_usmap(include=c("WI", "IL"), fill = "grey95")+
  #geom_text_repel(data = Site_t,
   #               aes(x = lon.1, y = lat.1, label = Soil),
    #              size = 3, alpha = 0.9)+
  #or use geom_label_repel and add: label.r = unit(0.5, "lines"),label.size = 0.5)+
  geom_point(data = Site_field, size = 6, color = "grey10", shape = 21,
             aes(y=lat.1, x =lon.1,  fill = aridity_tho_sum))+
  scale_fill_gradient(low ="#78005C" , high = "#FF70DE", guide = guide_legend(reverse = TRUE))+
  labs(fill = "Aridity", title = "Aridity of Acer negundo Source Sites")+
  theme(legend.position = "left")

plot_usmap(include=c("WI", "IL"), fill = "grey95")+
  #geom_text_repel(data = Site_qr,
   #               aes(x = lon.1, y = lat.1, label = Soil),
    #              size = 3, alpha = 0.9)+
  #label.r = unit(0.5, "lines"),label.size = 0.5)+
  geom_point(data = Site_qr, size = 6, shape =21, color = "grey10", 
             aes(y=lat.1, x =lon.1, fill = aridity_tho_sum))+
  scale_fill_gradient(low ="#664200" , high = "#fcc560", guide = guide_legend(reverse = TRUE))+
  labs(fill = "Aridity", title = "Aridity of Quercus rubra Source Sites")+
  theme(legend.position = "right") 
