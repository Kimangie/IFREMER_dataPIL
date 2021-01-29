####################################################################.
# Angela Larivain - Jan2021 
##Station de Lorient

### 2021; Running under R version 4.0.3

rm(list=ls())
setwd("~/Data_extraction/SACROIS_PIL_2000-2020")
##### Packages-------------------------
library(plyr)
library(tidyverse)
library(data.table)
library(MASS)
library(reshape2)
#--------------------------------------------.
####### useful Variables 
#Area 7.de with Douarn
Area<-c("25E4", "25E5", "26E6","26E7","26E8","27E5", "27E6", "27E7", "27E9", "28E3", "28E4", "28E5", "28E6", "28E7", "28E8", "28E9", "28F0", "29E5", "29E7", "29E8", "29E9", "29F0", "29F1", "30E9", "30F0")
# Area 7.de - Channel without 25E4 25E5
Area7<-c("26E6","26E7","26E8","27E5", "27E6", "27E7", "27E9", "28E3", "28E4", "28E5", "28E6", "28E7", "28E8", "28E9", "28F0", "29E5", "29E7", "29E8", "29E9", "29F0", "29F1", "30E9", "30F0")
#Purse seiner in Douarn
bolinch <- c("PS", "PS1", "PSX", "SX")


########### Load SACROIS and subset on 7de + LIEU_COD return in 7de + the gear sampled and used for sardine ###########
#load ~ each raw file for unique date was combined in 2000-2020 period and the date split in D/M/Y - see cleaning_bddPIL.R
NewFrame <- read.csv("FRAchannel_sardine_2000_2020_ready.csv") #99,265 obs. 70 var (sup. X)
NewFrame$X <- NULL

# keep without the Douarn Bay
NewF.7de <- filter(NewFrame, SECT_COD_SACROIS_NIV5 %in% Area7) #62,346 
NewF.7de$SECT_COD_SACROIS_NIV5 <- as.factor(NewF.7de$SECT_COD_SACROIS_NIV5)
table(NewF.7de$SECT_COD_SACROIS_NIV5)
table(NewF.7de$ESP_COD_FAO) #OK ALL PIL
str(NewF.7de)
NewF.7de <- dplyr::select(NewF.7de, -PAVILLON) #cause all in FRA
# select interest variables
NewF.7de <- dplyr::select(NewF.7de, ID, CFR_COD:MAREE_DATE_RET, TPS_MER, LIEU_COD_DEP_SACROIS:METIER_COD_SACROIS, METIER_DCF_5_COD:ENGIN_COD, SECT_COD_SACROIS_NIV5, CAPT_ID,STOCK_ORGP, ORIGINE_QUANT_POIDS_VIF:Time) #33 var

#rename variables
NewF.7de <- rename(NewF.7de, "port_ret" = LIEU_COD_RET_SACROIS,
       "Gear" = ENGIN_COD,
       "rects" = SECT_COD_SACROIS_NIV5,
       "land_sac_kg" = QUANT_POIDS_VIF_SACROIS)


### Check port (port_ret) --------------------
quant_port_ret <- ddply(NewF.7de,c("port_ret"), summarise,
                        N_port = length(port_ret), 
                        sum = sum(land_sac_kg, na.rm=T)) #79 

round(tapply(NewF.7de$land_sac_kg, 
             list(NewF.7de$rects, 
                  NewF.7de$port_ret),sum, na.rm=TRUE),0) #45T XDZ!! to remove considering the area
#sup XDZ
NewF.7de <- NewF.7de %>%
  dplyr::filter(port_ret!="XDZ") # -30 obs.
table(NewF.7de$port_ret)
unique(NewF.7de$port_ret) #78

  ## separate all in Manche (code for maritime districts) / Others
######### vector 
# 58 codes ports : 56 pour les 13 Quartiers maritimes + NLSCE/IJN/NLVLI
car <-c(unique(subset(NewF.7de, substring(port_ret,2,3)=="BL")$port_ret), unique(subset(NewF.7de, substring(port_ret,2,3)=="BR")$port_ret), unique(subset(NewF.7de, substring(port_ret,2,3)=="CH")$port_ret), unique(subset(NewF.7de, substring(port_ret,2,3)=="CN")$port_ret), unique(subset(NewF.7de, substring(port_ret,2,3)=="DK")$port_ret), unique(subset(NewF.7de, substring(port_ret,2,3)=="DP")$port_ret), unique(subset(NewF.7de, substring(port_ret,2,3)=="FC")$port_ret), unique(subset(NewF.7de, substring(port_ret,2,3)=="LH")$port_ret), unique(subset(NewF.7de, substring(port_ret,2,3)=="MX")$port_ret), unique(subset(NewF.7de, substring(port_ret,2,3)=="PL")$port_ret), unique(subset(NewF.7de, substring(port_ret,2,3)=="SB")$port_ret),unique(subset(NewF.7de, substring(port_ret,2,3)=="SM")$port_ret), "NLSCE", "IJN", "NLVLI") 
#-----------------------> #CHECK POUR FAIRE un TAPPLY.. FUNCTION??
#
NewF.7de_port <- filter(NewF.7de, port_ret %in% car)  #port Façade Manche
#------------.
NewF7_portOTH <- filter(NewF.7de, !port_ret %in% car) # autres ports
#
round(tapply(NewF7_portOTH$land_sac_kg, 
             list(NewF7_portOTH$rects, 
                  NewF7_portOTH$port_ret),sum, na.rm=TRUE),0)

#### Correct some code port and keep only 4 (in initial dataframe)
 #CRU = La Réunion..
NewF.7de$port_ret[NewF.7de$port_ret =="CRU"] <- "ALS"
 #XXX - nav=242089 usually in Dieppe -> as XDP - do it back in NewF.7de
NewF.7de$port_ret[NewF.7de$port_ret =="XXX"] <- "XDP"

# XCC correct: 
NewF7_portOTH$Gear[NewF7_portOTH$port_ret =="XCC" & NewF7_portOTH$rects == "26E6"] #PS1 et PS
# replace
# 29E5 -> 25E5 (ok delete) & 26E6 -> 24E6 ou 16E8?? 
NewF.7de$rects[NewF.7de$port_ret =="XCC" & NewF.7de$rects == "26E6"] <- "24E6"
table(NewF.7de$port_ret)

port_keep <- c("AAY", "ALS", "NLVLI", "XCC")

NewF.7de <- NewF.7de %>% 
  dplyr::filter(port_ret %in% car | port_ret %in% port_keep) #62,178
unique(NewF.7de$port_ret) #60
#rm PLHEL..
NewF.7de <- NewF.7de %>% 
  dplyr::filter(!port_ret == "PLHEL")
#
NewF7_portOTH <- filter(NewF.7de, !port_ret %in% car)
round(tapply(NewF7_portOTH$land_sac_kg, 
             list(NewF7_portOTH$rects, 
                  NewF7_portOTH$port_ret),sum, na.rm=TRUE),0)

## rm XCC, recode rects for AAY/ALS --> Golfe to remove and keep NLVLI port_ret
 # add NLVLI in car
NewF.7de <- NewF.7de %>% 
  dplyr::filter(port_ret %in% car) #62,017  
unique(NewF.7de$port_ret) #56 ports
#
quant_port_ret <- ddply(NewF.7de,c("port_ret"), summarise,
                        N_port = length(port_ret), 
                        sum_qt = sum(land_sac_kg, na.rm=T),
                        perc_rank = sum_qt/87616375*100) #56

quant_port_ret <- quant_port_ret %>%
  arrange(desc(perc_rank))


#Top '12' to plot - < 0.1%
t_land <- dplyr::slice(quant_port_ret, 1:12)

## PLOT Qtité
ggplot(t_land,aes(x=port_ret,y=sum_qt))+
  geom_bar(stat="identity")

table(NewF.7de$Gear)
### Check Qtité by gear
Gear_Qtsac <- ddply(NewF.7de, c("ENGIN_COD_SACROIS"), summarise,
                           N_gear    = length(ENGIN_COD_SACROIS), 
                           sum_land = sum(land_sac_kg, na.rm=T))
#187 NA - ?
ggplot(Gear_Qtsac,aes(x=ENGIN_COD_SACROIS,y=sum_land))+
  geom_bar(stat="identity") #22 codes engins

round(tapply(NewF.7de$land_sac_kg, 
             NewF.7de$ENGIN_COD_SACROIS,sum, na.rm=TRUE),0)

table(NewF.7de$ENGIN_COD_SACROIS, NewF.7de$Gear)
#
gear_na <- NewF.7de %>% filter_at(vars(ENGIN_COD_SACROIS),any_vars(is.na(.)))   
NewF.7de %>% dplyr::select(ENGIN_COD_SACROIS, Gear, METIER_COD_SACROIS, METIER_DCF_5_COD)
#
round(tapply(gear_na$land_sac_kg, 
             list(gear_na$NAVS_COD,
                  gear_na$Gear), sum, na.rm=TRUE),0)

#### Correct NA in ENGIN_COD_SACROIS (for land in tons) ------
# GES -> OTM
NewF.7de$ENGIN_COD_SACROIS[is.na(NewF.7de$ENGIN_COD_SACROIS) & NewF.7de$Gear =="GES" & NewF.7de$NAVS_COD == "716582" | NewF.7de$NAVS_COD == "651429" | NewF.7de$NAVS_COD == "925601"] <- "OTM"

NewF.7de$ENGIN_COD_SACROIS[is.na(NewF.7de$ENGIN_COD_SACROIS) & NewF.7de$Gear =="GN" & NewF.7de$NAVS_COD == "738594"] <- "OTB" #8 T 
 ## 735386 - OTB?
NewF.7de$ENGIN_COD_SACROIS[is.na(NewF.7de$ENGIN_COD_SACROIS) & NewF.7de$Gear =="OTB" | NewF.7de$Gear =="TB" & NewF.7de$NAVS_COD == "735386"] <- "OTB" #possible OTM
NewF.7de$ENGIN_COD_SACROIS[is.na(NewF.7de$ENGIN_COD_SACROIS) & NewF.7de$Gear =="TBB" & NewF.7de$NAVS_COD == "735386"] <- "TBB"
#
NewF.7de$ENGIN_COD_SACROIS[is.na(NewF.7de$ENGIN_COD_SACROIS) & NewF.7de$Gear =="OTB" & NewF.7de$NAVS_COD == "734964"] <- "OTM"

NewF.7de$ENGIN_COD_SACROIS[is.na(NewF.7de$ENGIN_COD_SACROIS) & NewF.7de$Gear =="PTB" | NewF.7de$Gear == "SPR" & NewF.7de$NAVS_COD == "644260"] <- "OTB"
 # 735383-PTM +
NewF.7de$ENGIN_COD_SACROIS[is.na(NewF.7de$ENGIN_COD_SACROIS) & NewF.7de$Gear =="PTB" | NewF.7de$Gear == "SPR" & NewF.7de$NAVS_COD == "735383"] <- "PTM"
### Saving some kilos
 ## TBB - OTB 
NewF.7de$ENGIN_COD_SACROIS[is.na(NewF.7de$ENGIN_COD_SACROIS) & NewF.7de$Gear =="TBB" & NewF.7de$NAVS_COD == "734736"] <- "OTB"
 # SPR 
NewF.7de$ENGIN_COD_SACROIS[is.na(NewF.7de$ENGIN_COD_SACROIS) & NewF.7de$Gear =="SPR" & NewF.7de$NAVS_COD == "934685"] <- "SPR" #following SACROIS recoding
#SDV -> SDN 
NewF.7de$ENGIN_COD_SACROIS[is.na(NewF.7de$ENGIN_COD_SACROIS) & NewF.7de$Gear =="SDV"] <- "SDN"
#FPO 268 kg et LTL 185 kg #error code ..
NewF.7de$ENGIN_COD_SACROIS[is.na(NewF.7de$ENGIN_COD_SACROIS) & NewF.7de$Gear =="FPO" & NewF.7de$NAVS_COD == "645006"] <- "FPO" #weird catch!
NewF.7de$ENGIN_COD_SACROIS[is.na(NewF.7de$ENGIN_COD_SACROIS) & NewF.7de$Gear =="LTL" & NewF.7de$NAVS_COD == "925621"] <- "OTB" #?

#--------------------------------------------------------------------.
### Verify
gear_na <- NewF.7de %>% filter_at(vars(ENGIN_COD_SACROIS),any_vars(is.na(.)))

round(tapply(gear_na$land_sac_kg, 
            list(gear_na$NAVS_COD,
                 gear_na$Gear), sum, na.rm=TRUE),0) #few kilos that will be deleted after checking for the targeting catch of sardines
### Delete and keep without NA Gear
NewF.7de <- NewF.7de %>% filter_at(vars(ENGIN_COD_SACROIS),any_vars(!is.na(.)))

Gear_Qtsac <- ddply(NewF.7de, c("ENGIN_COD_SACROIS"), summarise,
                    N_gear    = length(ENGIN_COD_SACROIS), 
                    sum_land = sum(land_sac_kg, na.rm=T),
                    percent_rank = sum_land/87616169*100)

pie(Gear_Qtsac$percent_rank, labels = Gear_Qtsac$ENGIN_COD_SACROIS, main="Pie Chart of Gear types catching Sardines")

### regroup some gear in category
table(NewF.7de$ENGIN_COD_SACROIS)


### Select targeting Gear ----------------
 # First select gear/metiers catching > 50 kg??

round(tapply(NewF.7de$land_sac_kg, 
             list(NewF.7de$METIER_COD_SACROIS,
                  NewF.7de$ENGIN_COD_SACROIS), sum, na.rm=TRUE),0)

## Landings tot_sac month/Year
summary(NewF.7de) #5,145 NA's in landings
nf_na <- NewF.7de %>% filter_at(vars(land_sac_kg),any_vars(is.na(.))) #5,145 ok
# 186 NAVS concerned and it is each Year
sac_ym <- ddply(NewF.7de, c("Year", "Month"), summarise,
                  Nb_records = length(land_sac_kg),
                  tot  = sum(land_sac_kg, na.rm=T),
                  mean = mean(land_sac_kg, na.rm=T),
                  max = max(land_sac_kg, na.rm=T),
                  min = min(land_sac_kg, na.rm=T))

## 2010 PCB contamination -----------------------------------------------------
NewF.7de_2000 <- NewF.7de %>%
                  dplyr::filter(Year <= 2010)
NewF.7de_2010 <- NewF.7de %>%
  dplyr::filter(Year > 2010)
#
 ## Dbq/port before-after
land_lieu <- ddply(NewF.7de_2000,c("port_ret"), summarise,
                        N_port = length(port_ret), 
                        sum_qt = sum(land_sac_kg, na.rm=T))
land_lieu <- land_lieu %>%
              arrange(desc(sum_qt))
t <- dplyr::slice(land_lieu, 1:5)
ggplot(t,aes(x=port_ret,y=sum_qt))+
  geom_bar(stat="identity")
#
land_lieu <- ddply(NewF.7de_2010,c("port_ret"), summarise,
                   N_port = length(port_ret), 
                   sum_qt = sum(land_sac_kg, na.rm=T))
land_lieu <- land_lieu %>%
  arrange(desc(sum_qt))
t <- dplyr::slice(land_lieu, 1:5)
ggplot(t,aes(x=port_ret,y=sum_qt))+
  geom_bar(stat="identity")

  ## Catch_by_gear before-after
catch_g2000 <- ddply(NewF.7de_2000,c("ENGIN_COD_SACROIS"), summarise,
                   N_port = length(ENGIN_COD_SACROIS), 
                   sum_qt = sum(land_sac_kg, na.rm=T))
catch_g2000 <- catch_g2000 %>%
  arrange(desc(sum_qt))
t <- dplyr::slice(catch_g2000, 1:5)
ggplot(t,aes(x=ENGIN_COD_SACROIS,y=sum_qt))+
  geom_bar(stat="identity")
#
catch_g10 <- ddply(NewF.7de_2010,c("ENGIN_COD_SACROIS"), summarise,
                   N_port = length(ENGIN_COD_SACROIS), 
                   sum_qt = sum(land_sac_kg, na.rm=T))
catch_g10 <- catch_g10 %>%
  arrange(desc(sum_qt))
t <- dplyr::slice(catch_g10, 1:5)
ggplot(t,aes(x=ENGIN_COD_SACROIS,y=sum_qt))+
  geom_bar(stat="identity")
#
## Check Douarnenez [2000-2020] --------------------------------------------
Douarn <- c("25E4", "25E5")
# /!\ check port XCC/XDZ - 29E5 --> 25E5
NewFrame %>%
  dplyr::filter(SECT_COD_SACROIS_NIV5 =="29E5" & LIEU_COD_RET_SACROIS =="XCC") %>%
  dplyr::select(NAVS_COD, ENGIN_COD_SACROIS, QUANT_POIDS_VIF_SACROIS) #PS AND same vessel!

NewFrame$SECT_COD_SACROIS_NIV5[NewFrame$LIEU_COD_RET_SACROIS =="XCC" | NewFrame$LIEU_COD_RET_SACROIS =="XDZ" & NewFrame$SECT_COD_SACROIS_NIV5 =="29E5"] <- "25E5"

douarn <- filter(NewFrame, SECT_COD_SACROIS_NIV5 %in% Douarn) #36,919
glimpse(douarn)

douarn <- dplyr::select(douarn, CFR_COD:MAREE_DATE_RET, TPS_MER, LIEU_COD_DEP_SACROIS:METIER_COD_SACROIS, METIER_DCF_5_COD:ENGIN_COD, SECT_COD_SACROIS_NIV5, CAPT_ID,STOCK_ORGP, ORIGINE_QUANT_POIDS_VIF:Time)

summary(douarn)
 # ++ in 25E5 
 # 131,889 tons
##Catch by gear in respective rects
round(tapply(douarn$QUANT_POIDS_VIF_SACROIS, 
             list(douarn$ENGIN_COD_SACROIS, 
                  douarn$SECT_COD_SACROIS_NIV5),sum, na.rm=TRUE),0)
#
round(tapply(douarn$QUANT_POIDS_VIF_SACROIS, 
             list(douarn$LIEU_COD_RET_SACROIS, 
                  douarn$SECT_COD_SACROIS_NIV5),sum, na.rm=TRUE),0)

