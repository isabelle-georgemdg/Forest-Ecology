library(readxl)
library(lme4)
library(emmeans)
library(tidyr)
library(plyr)
library(dplyr)
library(ggplot2)
library(lmerTest)

#merging 202 An spreadsheets together
An1 = read_excel("Forest Ecology/Greenhouse Experiments 2020/AnBook1.xlsx")
An2 = read_excel("Forest Ecology/Greenhouse Experiments 2020/AnBook2.xlsx")
An3 = read_excel("Forest Ecology/Greenhouse Experiments 2020/AnBook3.xlsx")
An4 = read_excel("Forest Ecology/Greenhouse Experiments 2020/AnBook4.xlsx")
An20 = merge(An1, An2, by = "Pot")
An20 = merge(An20, An3, by = "Pot")
An20 = merge(An20, An4, by = "Pot")
write.csv(An20, "An20.csv")
#edited the spreadsheet in excel 

#final spreadsheet
An <- read_excel("Boxelder1820.xlsx")
#Add home and foreign microbe treatment
An = An %>% mutate(Microbe = if_else(Soil == Pop, 
                                         "home", if_else(Soil != Pop, "foreign", "NA")))
#import site data for soil and population and merge with An data
site = read.csv("careerGH_site_metadata.csv")
seedpop = read.csv("career_Qrub_Aneg_seedling_pop_metadata.csv")

AnS= left_join(An, site, by="Soil")
AnP = left_join(AnS, seedpop, by="Pop")

#add combo treatment
AnP$TrtCombo =  paste(AnP$TempTrt, AnP$WaterTrt)

#Subset by initial L or D 
AnLD = subset(AnP, In_LD != "NO PLANT")
AnL = subset(AnP, In_LD == "L")
AnEndL = subset(AnP, End_LD == "L")

a = lmer(I(AG_mass + BG_mass)~aridity_sum_har*pop_aridity_sum_har*TrtCombo + In_He + In_SD + (1|GHCode/YearCode) , data=AnL)
#population by soil is too many combinations so not a good model
b = lmer(I(AG_mass + BG_mass)~Pop*Soil*TrtCombo + In_He + In_SD + (1|GHCode/YearCode) , data=AnL)
c = lmer(I(AG_mass + BG_mass)~Microbe*TempTrt*WaterTrt + In_He + In_SD + (1|GHCode/YearCode) , data=AnL)
cEm = emmeans(c, ~Microbe*TempTrt*WaterTrt)
cEm = as.data.frame(summary(cEm) [c("Microbe", "TempTrt", "WaterTrt", "emmean", "SE")])

#Bar graph of model c
ggplot(cEm, aes(y=emmean, x=Microbe, fill = Microbe))+
  geom_bar(stat = "identity")+
  geom_errorbar(color = "grey29", aes(ymin=emmean-SE, ymax=emmean+SE))+
  facet_grid(WaterTrt~TempTrt)+
  theme_light()

aEm = emmeans(a, ~aridity_sum_har*pop_aridity_sum_har*TrtCombo)
aEm = as.data.frame(summary(aEm) [c("aridity_sum_har", "pop_aridity_sum_har", "TrtCombo", "emmean", "SE")])

an1 = lmer(I(AG_mass + BG_mass)~scale(MAT)*scale(pop_MAT)*TrtCombo + In_He + In_SD + (1|GHCode/YearCode) , AnL)
an2 = lmer(I(AG_mass + BG_mass)~scale(MAT)*scale(pop_MAT)*WaterTrt*TempTrt + In_He + In_SD + (1|GHCode/YearCode) , AnL)

Ansub = subset(AnL, In_He > 0 & In_SD > 0 & AG_mass >= 0 & BG_mass >= 0)
an3 = lm(I(AG_mass + BG_mass)~In_He + In_SD, data = AnL)

Ansub$res <- an3$residuals

ggplot(data = Ansub, aes(x= MAT, y = res))+
  geom_point(aes(color = pop_MAT))+
  geom_smooth(method = lm, se = FALSE, color = "black")+
  facet_grid(WaterTrt~TempTrt, scales = "free")
ggplot(data = Ansub, aes(x= pop_MAT, y = res))+
  geom_point(aes(color = MAT))+
  geom_smooth(method = lm, se = FALSE, color = "black")+
  facet_grid(WaterTrt~TempTrt, scales = "free")

an4 = lmer(I(AG_mass + BG_mass)~scale(aridity_sum_har)*scale(pop_aridity_sum_har)*TrtCombo + In_He + In_SD + (1|GHCode/YearCode) , AnL)
an5 = lmer(I(AG_mass + BG_mass)~scale(aridity_sum_har)*scale(pop_aridity_sum_har)*WaterTrt*TempTrt + In_He + In_SD + (1|GHCode/YearCode) , AnL)
AnLs = subset(AnL, SoilMix <7)
an6 = lmer(I(AG_mass + BG_mass)~Microbe*TempTrt*WaterTrt + In_He + In_SD + (1|GHCode/YearCode) , data=AnLs)
anem = emmeans(an6,~Microbe*TempTrt*WaterTrt)
anem = as.data.frame(anem)

an7 = lmer(I(AG_mass + BG_mass)~Microbe + scale(aridity_sum_har)*TempTrt*WaterTrt + In_He + In_SD + (1|GHCode/YearCode) , data=subset(AnL, SoilMix <7)

ggplot(data = Anfl, aes(x= aridity_sum_har, y = (AG_mass + BG_mass)))+
  geom_point(position = "jitter", size = 3, alpha = 0.75, aes(color = pop_aridity_tho_sum, shape = Microbe))+
  geom_smooth(method = lm, se = FALSE, color = "grey50")+
  facet_grid(WaterTrt~TempTrt, scales = "free", 
             labeller = labeller(WaterTrt = labels, TempTrt = labels))+
  scale_color_gradient(low ="#78005C" , high = "#FF70DE")+
  theme_light()+
  theme(legend.position = "none")+
  labs(y= "Mean Total Biomass", 
       x = "Aridity of Soil Source",
       title="Biomass of Acer negundo Tree Seedlings")

ggplot(data = Ansub, aes(x= aridity_sum_har, y = res))+
  geom_point(aes(color = pop_aridity_sum_har))+
  geom_smooth(method = lm, se = FALSE, color = "black")+
  facet_grid(WaterTrt~TempTrt, scales = "free")
ggplot(data = Ansub, aes(x= pop_aridity_sum_har, y = res))+
  geom_point(aes(color = aridity_sum_har))+
  geom_smooth(method = lm, se = FALSE, color = "black")+
  facet_grid(WaterTrt~TempTrt, scales = "free")

Anbar <- ddply(Ansub, 
                 c("Microbe", "WaterTrt", "TempTrt"), 
                 summarise,
                 n = length(AG_mass),
                 mass_mean = mean(AG_mass+BG_mass),
                 sd = sd(AG_mass+BG_mass),
                 se=sd/sqrt(n),
                 meanN = mean(n))

ggplot(anem, aes(y=emmean, x=Microbe))+ #,fill = pop_aridity_sum_har))+
  geom_bar(stat = "identity", fill = "#B52D95")+
  geom_errorbar(color = "grey29", aes(ymin=emmean-SE, ymax=emmean+SE))+
  facet_grid(WaterTrt~TempTrt, scales = "free", 
             labeller = labeller(WaterTrt = labels, TempTrt = labels))+
  theme_light()+
  labs(y= "Mean Total Biomass", 
       x = "Microbe: Foreign or Home",
       title="Biomass of Acer negundo Tree Seedlings")


#Bar graph of model c
ggplot(data = AnL, aes(x= MAT, y = (AG_mass+BG_mass)))+
  geom_point(aes(color = pop_MAT))+
  geom_smooth(method = lm, se = FALSE, color = "black")+
  facet_grid(WaterTrt~TempTrt, scales = "free")


