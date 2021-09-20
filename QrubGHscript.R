library(plyr)
library(dplyr)
library(lme4)
library(ggplot2)
library(car)
library(emmeans)
library(lmerTest)

qrub = read.csv("QrubGH_2018_2020.csv")
site = read.csv("careerGH_site_metadata.csv")
seedpop = read.csv("career_Qrub_Aneg_seedling_pop_metadata.csv")

qrub = qrub %>% rename(Soil = soil, 
                       Pop=seedling_pop)

qrub2 = left_join(qrub, site, by="Soil")
qrub3 = left_join(qrub2, seedpop, by="Pop")

qrub3$trtcombo = paste(qrub3$TempTrt, qrub3$WaterTrt2)
qrub4 = subset(qrub3, In_Live != "NP")
qrub5 = subset(qrub3, In_Live == "L")

a = lmer(I(mass_ag_w0 + mass_bg_w0)~aridity_sum_har*pop_aridity_sum_har*trtcombo + In_Height + In_SD + (1|Ghcode/Yearcode) , data=qrub5)

a = lmer(I(mass_ag_w0 + mass_bg_w0)~seedling_pop*soil*trtcombo + In_Height + In_SD + (1|Ghcode/Yearcode) , data=qrub5)


qrub4$distlat = qrub4$lat - qrub4$pop_lat
qrub4$diffarid = qrub4$aridity_sum_har - qrub4$pop_aridity_sum_har
qrub4$HvA = Recode(qrub4$distlat, "0 = 'H'; else = 'A'")

#from 2020
a = lmer(Total_Mass~WaterTrt*TempTrt*Microbe + 
           as.factor(Year) + (1|pop) + (1|soil), 
         data=subset(Qrub_p, SoilMix < 7 & Total_Mass > 0))

qrub5$distlat = qrub5$lat - qrub5$pop_lat
qrub5$diffarid = qrub5$aridity_sum_har - qrub5$pop_aridity_sum_har
qrub5$diffMAT = qrub5$MAT - qrub5$pop_MAT
qrub5$HvA = Recode(qrub5$distlat, "0 = 'H'; else = 'A'")

a = lmer(I(mass_ag_w0 + mass_bg_w0)~HvA*TempTrt*WaterTrt2 + In_Height + In_SD + (1|Ghcode/Yearcode) , data=qrub5)
a = lmer(I(mass_ag_w0 + mass_bg_w0)~distlat*TempTrt*WaterTrt2 + In_Height + In_SD + (1|Ghcode/Yearcode) , data=qrub5)
a = lmer(I(mass_ag_w0 + mass_bg_w0)~diffMAT*TempTrt*WaterTrt2 + In_Height + In_SD + (1|Ghcode/Yearcode) , data=qrub5)

a = lmer(I(mass_ag_w0 + mass_bg_w0)~MAT*pop_MAT*TempTrt*WaterTrt2 + In_Height + In_SD + (1|Ghcode/Yearcode) , data=qrub5)

qr1 = lmer(I(mass_ag_w0 + mass_bg_w0)~scale(MAT)*scale(pop_MAT)*trtcombo + In_Height + In_SD + (1|Ghcode/Yearcode) , data=qrub5)
qr2 = lmer(I(mass_ag_w0 + mass_bg_w0)~scale(MAT)*scale(pop_MAT)*TempTrt*WaterTrt + In_Height + In_SD + (1|Ghcode/Yearcode) , data=qrub6)


qrub6 = subset(qrub5, In_Height > 0 & In_SD > 0 & mass_ag_w0 >= 0 & mass_bg_w0 >= 0 & SoilMix < 7)
qr3 = lm(I(mass_ag_w0 + mass_bg_w0)~In_Height + In_SD, data = qrub6)


qrub6$res <- qr3$residuals

ggplot(data = qrub6, aes(x = MAT, y = res))+
  geom_point(aes(color = pop_MAT))+
  geom_smooth(method = lm, se = FALSE, color = "black")+
  facet_grid(WaterTrt~TempTrt, scales = "free")
ggplot(data = qrub6, aes(x= pop_MAT, y = res))+
  geom_point(aes(color = MAT))+
  geom_smooth(method = lm, se = FALSE, color = "black")+
  facet_grid(WaterTrt~TempTrt, scales = "free")

qr4 = lmer(I(mass_ag_w0 + mass_bg_w0)~scale(aridity_sum_har)*scale(pop_aridity_sum_har)*trtcombo + In_Height + In_SD + (1|Ghcode/Yearcode) , data=qrub6)
qr5 = lmer(I(mass_ag_w0 + mass_bg_w0)~scale(aridity_sum_har)*scale(pop_aridity_sum_har)*TempTrt*WaterTrt + In_Height + In_SD + (1|Ghcode/Yearcode) , data=qrub6)

labels <- c(C = "Cool Temp. 26 C", H = "Warm Temp. 29 C", D= "Drought Trt.", W = "Well Watered Trt.")
(mass_ag_w0 + mass_bg_w0)
ggplot(data = qrub6, aes(x= aridity_sum_har, y = (mass_ag_w0 + mass_bg_w0)))+
  geom_point( size = 3, alpha = 0.75, aes(color = pop_aridity_tho_sum))+
  geom_smooth(method = lm, se = FALSE, color = "grey50")+
  facet_grid(WaterTrt~TempTrt, scales = "free", 
             labeller = labeller(WaterTrt = labels, TempTrt = labels))+
  scale_color_gradient(low ="#805300" , high = "#FFC457")+
  theme_light()+
  theme(legend.position = "none")+
  labs(y= "Mean Total Biomass", 
       x = "Aridity of Soil Source",
       title="Biomass of Quercus rubra Tree Seedlings")

ggplot(data = qrub6, aes(x= pop_aridity_sum_har, y = (mass_ag_w0 + mass_bg_w0)))+
  geom_point(aes(color = aridity_sum_har, shape = HvA))+
  geom_smooth(method = lm, se = FALSE, color = "black")+
  facet_grid(WaterTrt~TempTrt, scales = "free")

qr6 = lmer(I(mass_ag_w0 + mass_bg_w0)~HvA + scale(aridity_sum_har)*TempTrt*WaterTrt2 + In_Height + In_SD + (1|Ghcode/Yearcode) , data=qrub5)

a = lmer(I(mass_ag_w0 + mass_bg_w0)~HvA*TempTrt*WaterTrt2 + In_Height + In_SD + (1|Ghcode/Yearcode) , data=qrub6)

qW = lmer(I(mass_ag_w0 + mass_bg_w0)~HvA + TempTrt + In_Height + In_SD + (1|Ghcode/Yearcode) , data=subset(qrub6, SoilMix < 7 & WaterTrt == "W"))
qD = lmer(I(mass_ag_w0 + mass_bg_w0)~HvA + TempTrt + In_Height + In_SD + (1|Ghcode/Yearcode) , data=subset(qrub6, SoilMix < 7 & WaterTrt == "D"))

aem = emmeans(a,~HvA*TempTrt*WaterTrt2)
aem = as.data.frame(aem)


Qrbar <- ddply(qrub6, 
               c("HvA", "WaterTrt", "TempTrt"), 
               summarise,
               n = length(mass_ag_w0),
               mass_mean = mean(mass_ag_w0+mass_bg_w0),
               sd = sd(mass_ag_w0+mass_bg_w0),
               se=sd/sqrt(n),
               meanN = mean(n))
Qrbar = Qrbar %>% mutate(Microbe = if_else(HvA == "A", "foreign",if_else(HvA == "H", "home", "none")))
Qrbar2 = subset(Qrbar, Microbe != "none")

ggplot(aem, aes(y=emmean, x=HvA))+ #,fill = pop_aridity_sum_har))+
  geom_bar(stat = "identity", fill = "#B8821F")+
  geom_errorbar(color = "grey29", aes(ymin=emmean-SE, ymax=emmean+SE))+
  facet_grid(WaterTrt2~TempTrt, scales = "free", 
             labeller = labeller(WaterTrt = labels, TempTrt = labels))+
  theme_light()+
  labs(y= "Mean Total Biomass", 
       x = "Microbe: Foreign or Home",
       title="Biomass of Quercus rubra Tree Seedlings")


ggplot(Qrbar, aes(y=mass_mean, x=HvA))+ #,fill = pop_aridity_sum_har))+
  geom_bar(stat = "identity")+
  geom_errorbar(color = "grey29", aes(ymin=mass_mean-se, ymax=mass_mean+se))+
  facet_grid(WaterTrt~TempTrt)+
  theme_light()



a = lmer(I(mass_ag_w0 + mass_bg_w0)~scale(MAT)+scale(pop_MAT) + In_Height + In_SD + (1|Ghcode/Yearcode) , data=subset(qrub5, trtcombo == "C A"))
a = lmer(I(mass_ag_w0 + mass_bg_w0)~scale(MAT)+scale(pop_MAT) + In_Height + In_SD + (1|Ghcode/Yearcode) , data=subset(qrub5, trtcombo == "C A"))
a = lmer(I(mass_ag_w0 + mass_bg_w0)~scale(MAT)+scale(pop_MAT) + In_Height + In_SD + (1|Ghcode/Yearcode) , data=subset(qrub5, trtcombo == "C A"))
a = lmer(I(mass_ag_w0 + mass_bg_w0)~scale(MAT)+scale(pop_MAT) + In_Height + In_SD + (1|Ghcode/Yearcode) , data=subset(qrub5, trtcombo == "C A"))

aEm = emmeans(a, ~MAT*pop_MAT*TempTrt*WaterTrt2 + In_Height + In_SD + (1|Ghcode/Yearcode))
aEm = as.data.frame(summary(aEm) [c("aridity_sum_har", "pop_aridity_sum_har", "TrtCombo", "emmean", "SE")])

ggplot(a, aes(y=emmean, x=aridity_sum_har, fill = pop_aridity_sum_har))+
  geom_bar(stat = "identity")+
  geom_errorbar(color = "grey29", aes(ymin=emmean-SE, ymax=emmean+SE))+
  facet_grid(~TrtCombo)+
  theme_light()