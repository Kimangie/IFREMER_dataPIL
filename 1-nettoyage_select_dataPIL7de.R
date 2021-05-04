####################################################################.
# Angela Larivain - Jan-May 2021
##Station de Lorient

### 2021; Running under R version 4.0.3

rm(list=ls())
setwd("~/Data_extraction/SACROIS_PIL_2000-2020")
##### Packages-------------------------
# library(plyr)
library(tidyverse)
library(data.table)
library(MASS, exclude = 'filter')
library(reshape2)
library(lubridate)
#
library(fishualize) #just for fun 
#--------------------------------------------.

# check for direct importation of the 3 files (SACROIS / VENTES / LOGBOOK)

####### useful Variables 
zone <- c("27.7.e","27.7.d")
#Area 7.de with Douarn
Area<-c("25E4", "25E5", "26E6","26E7","26E8","27E5", "27E6", "27E7", "27E9", "28E3", "28E4", "28E5", "28E6", "28E7", "28E8", "28E9", "28F0", "29E5", "29E7", "29E8", "29E9", "29F0", "29F1", "30E9", "30F0")
# Area 7.de - Channel without 25E4 25E5
Area7<-c("26E6","26E7","26E8","27E5", "27E6", "27E7", "27E9", "28E3", "28E4", "28E5", "28E6", "28E7", "28E8", "28E9", "28F0", "29E5", "29E7", "29E8", "29E9", "29F0", "29F1", "30E9", "30F0")
#Purse seiner in Douarn
bolinch <- c("PS", "PS1", "PSX", "SX")
Douarn <- c("25E4", "25E5")
#
##SACROIS (FRA/PIL) 2000-2020 #------------------------------------------
# APRIL 2021 UPDATE COMPIL SACROIS_BRUT
NewFrame <- get(load("FRA_PIL_2000_2020_sacrois_27.7.de.RData"))
# nf <- read.csv("FRAchannel_sardine_2000_2020_ready.csv", sep=";") #99,265 obs. 72 var (sup. X) - date already separate 
# head(NewFrame)
# names(NewFrame)
# #NewFrame <- read.csv("FRA_Channel_PIL_2000_2020_sacrois.csv", header=T, na.strings = "") # Separate the date!
# NewFrame <- select(NewFrame, -X, -X.1)

#### separate the date, lubridate #------------------------------------------------------------
str(NewFrame$DATE_SEQ)
# Convert time to a time format with lubridate
# ymd_hms()
# hms()
# ymd()
NewFrame$DATE_SEQ <- dmy_hms(NewFrame$DATE_SEQ)
# Extract a year/month component & create quarter:
NewFrame$Year <- year(NewFrame$DATE_SEQ)
NewFrame$Month <- month(NewFrame$DATE_SEQ)
NewFrame$Day <- day(NewFrame$DATE_SEQ)
NewFrame$quarter <- quarter(NewFrame$DATE_SEQ)

names(NewFrame)
str(NewFrame) # some factor in string
summary(NewFrame)

# list_names <- colnames(NewFrame)[sapply(NewFrame, is.factor)]
is.fact <- sapply(NewFrame, is.factor)
NewFrame[, is.fact] <- sapply(NewFrame[, is.fact], as.character)

## FOUND FUNCTION
NewFrame$NAVS_COD <- as.integer(NewFrame$NAVS_COD)
NewFrame$MAREE_ID <- as.integer(NewFrame$MAREE_ID)
NewFrame$SEQ_ID <- as.integer(NewFrame$SEQ_ID)
NewFrame$MAILLAGE <- as.integer(NewFrame$MAILLAGE)
NewFrame$Year <- as.integer(NewFrame$Year)
NewFrame$Month <- as.integer(NewFrame$Month)

str(NewFrame)

####~~ Correct errors before subset#------------------------------------------------
# All *DZ in XDZ!
table(NewFrame$LIEU_COD_RET_SACROIS)
# NewFrame$LIEU_COD_RET_SACROIS <- as.character(NewFrame$LIEU_COD_RET_SACROIS)

port_d <- c(unique(subset(NewFrame, substring(LIEU_COD_RET_SACROIS,2,3)=="DZ")$LIEU_COD_RET_SACROIS))
NewFrame$LIEU_COD_RET_SACROIS[NewFrame$LIEU_COD_RET_SACROIS %in% port_d] <- "XDZ"

#/!\ check port XCC/XDZ - 29E5 --> 25E5
NewFrame %>%
  dplyr::filter(SECT_COD_SACROIS_NIV5 =="29E5" & LIEU_COD_RET_SACROIS =="XCC") %>%
  dplyr::select(NAVS_COD, ENGIN_COD_SACROIS, QUANT_POIDS_VIF_SACROIS) #PS AND same vessel 898402!

##
NewFrame$SECT_COD_SACROIS_NIV5[NewFrame$SECT_COD_SACROIS_NIV5 =="29E5" & NewFrame$LIEU_COD_RET_SACROIS%in%c("XCC","XDZ")] <- "25E5"

#WAR ROAG III
NewFrame %>%
  filter(NAVS_COD%in%898415 & LIEU_COD_RET_SACROIS%in%c("XLO","XCC","XDZ","AGV")) %>%
  group_by(LIEU_COD_RET_SACROIS, SECT_COD_SACROIS_NIV5, ENGIN_COD_SACROIS) %>%
  summarize(PROD=sum(QUANT_POIDS_VIF_SACROIS,na.rm=T))

NewFrame$SECT_COD_SACROIS_NIV5[NewFrame$NAVS_COD%in%898415 & NewFrame$LIEU_COD_RET_SACROIS%in%"XDZ"& NewFrame$SECT_COD_SACROIS_NIV5%in%"26E6"] <- "25E5"

NewFrame %>%
  filter(NAVS_COD%in%898415 & LIEU_COD_RET_SACROIS%in%c("XLO","XCC","AGV")) %>%
  group_by(LIEU_COD_RET_SACROIS, SECT_COD_SACROIS_NIV5, ENGIN_COD_SACROIS) %>%
  summarize(PROD=sum(QUANT_POIDS_VIF_SACROIS,na.rm=T))

NewFrame$SECT_COD_SACROIS_NIV5[NewFrame$NAVS_COD%in%898415 & NewFrame$LIEU_COD_RET_SACROIS%in%c("XLO","XCC")& NewFrame$SECT_COD_SACROIS_NIV5%in%"25E5"] <- "24E6"

NewFrame$SECT_COD_SACROIS_NIV5[NewFrame$NAVS_COD%in%898415 & NewFrame$LIEU_COD_RET_SACROIS%in%c("AGV", "XGV")] <- "24E5"

NewFrame$SECT_COD_SACROIS_NIV5[NewFrame$NAVS_COD%in%898415 & NewFrame$LIEU_COD_RET_SACROIS%in%"XCC" & NewFrame$SECT_COD_SACROIS_NIV5%in%"26E8"] <- "16E8"

#LA SARDANE - ~duplicates? all in 28E6 - correct just for XDZ..
NewFrame %>%
  filter(NAVS_COD%in%365109 & LIEU_COD_RET_SACROIS%in%"XDZ") %>%
  group_by(LIEU_COD_RET_SACROIS, SECT_COD_SACROIS_NIV5, ENGIN_COD_SACROIS) %>%
  summarize(PROD=sum(QUANT_POIDS_VIF_SACROIS,na.rm=T))

NewFrame$SECT_COD_SACROIS_NIV5[NewFrame$NAVS_COD%in%365109 & NewFrame$LIEU_COD_RET_SACROIS%in%"XDZ"& NewFrame$SECT_COD_SACROIS_NIV5%in%"28E6"] <- "25E5"

NewFrame$SECT_COD_SACROIS_NIV5[NewFrame$NAVS_COD%in%365109 & NewFrame$LIEU_COD_RET_SACROIS%in%"AGV"& NewFrame$SECT_COD_SACROIS_NIV5%in%"28E6"] <- "26E8"

round(tapply(NewFrame$QUANT_POIDS_VIF_SACROIS, 
             list(NewFrame$SECT_COD_SACROIS_NIV5, 
                  NewFrame$LIEU_COD_RET_SACROIS),sum, na.rm=TRUE),0) 


#CRU - mistake with ALS 
Navs_cru <- c(unique(NewFrame$NAVS_COD[which(NewFrame$LIEU_COD_RET_SACROIS=="CRU")])) #give the concerned vessels = 2
# 753550 and 753315
#
NewFrame %>%
  filter(NAVS_COD%in%Navs_cru) %>%
  group_by(NAVS_COD, LIEU_COD_RET_SACROIS, SECT_COD_SACROIS_NIV5) %>%
  summarize(PROD=sum(QUANT_POIDS_VIF_SACROIS,na.rm=T))

NewFrame$LIEU_COD_RET_SACROIS[NewFrame$LIEU_COD_RET_SACROIS%in%"CRU"] <- "ALS"
#
# Bay of Biscay miscoding
NewFrame$SECT_COD_SACROIS_NIV5[NewFrame$NAVS_COD%in%460612&NewFrame$LIEU_COD_RET_SACROIS%in%"AAY"& NewFrame$SECT_COD_SACROIS_NIV5%in%"26E6"] <- "24E6"
NewFrame$SECT_COD_SACROIS_NIV5[NewFrame$NAVS_COD%in%612252&NewFrame$LIEU_COD_RET_SACROIS%in%"ALS"& NewFrame$SECT_COD_SACROIS_NIV5%in%"29E7"] <- "22E7"
NewFrame$SECT_COD_SACROIS_NIV5[NewFrame$NAVS_COD%in%c(715569,753550,799250) &NewFrame$LIEU_COD_RET_SACROIS%in%"ALS"& NewFrame$SECT_COD_SACROIS_NIV5%in%"27E7"] <- "22E7" #NK?715569

NewFrame$SECT_COD_SACROIS_NIV5[NewFrame$NAVS_COD%in%753315&NewFrame$LIEU_COD_RET_SACROIS%in%"ALS"& NewFrame$SECT_COD_SACROIS_NIV5%in%"28E7"] <- "22E7"
NewFrame$SECT_COD_SACROIS_NIV5[NewFrame$NAVS_COD%in%753357&NewFrame$LIEU_COD_RET_SACROIS%in%"ALS"& NewFrame$SECT_COD_SACROIS_NIV5%in%"28E5"] <- "22E5"
NewFrame$SECT_COD_SACROIS_NIV5[NewFrame$NAVS_COD%in%753491&NewFrame$LIEU_COD_RET_SACROIS%in%"ALS"& NewFrame$SECT_COD_SACROIS_NIV5%in%c("27E6", "26E7", "28E7")] <- "22E7"


unique(NewFrame$LIEU_COD_RET_SACROIS)
Navs_XXX <- c(unique(NewFrame$NAVS_COD[which(NewFrame$LIEU_COD_RET_SACROIS=="XXX")]))

NewFrame %>%
  filter(NAVS_COD%in%Navs_XXX) %>%
  group_by(NAVS_COD, LIEU_COD_RET_SACROIS, SECT_COD_SACROIS_NIV5) %>%
  summarize(PROD=sum(QUANT_POIDS_VIF_SACROIS,na.rm=T)) # 31 kg.. WHERE? 

NewFrame$LIEU_COD_RET_SACROIS[NewFrame$LIEU_COD_RET_SACROIS %in% "XXX" & NewFrame$NAVS_COD%in%642089] <- "XSM"
#
rm(list=ls(pat ="Navs"))

# select interest variables
names(NewFrame)
unique(NewFrame$ESP_COD_FAO)

var_keep = c("CFR_COD", "NAVS_COD", "PAVILLON", "MAREE_ID", "MAREE_DATE_DEP", "MAREE_DATE_RET", "TPS_MER", "LIEU_COD_DEP_SACROIS", "LIEU_COD_RET_SACROIS", "SEQ_ID", "DATE_SEQ", "METIER_COD_SACROIS", "METIER_DCF_5_COD", "METIER_DCF_6_COD", "ENGIN_COD_SACROIS", "ENGIN_COD", "MAILLAGE", "DIMENSION", "SECT_COD_SACROIS_NIV3", "SECT_COD_SACROIS_NIV5", "TP_NAVIRE_SACROIS", "TP_NAVIRE_MOYENNE", "CAPT_ID", "STOCK_ORGP", "ORIGINE_QUANT_POIDS_VIF", "QUANT_POIDS_VIF_SACROIS", "QUANT_POIDS_VIF_SIPA", "QUANT_POIDS_VIF_MOYENNE", "ORIGINE_MONTANT_EUROS", "MONTANT_EUROS_SACROIS", "time", "Day", "Month", "Year", "quarter")

NewFrame <- NewFrame %>%
  dplyr::select(all_of(var_keep))
  
#rename variables
head(NewFrame)
NewFrame <- rename(NewFrame, "port_ret" = LIEU_COD_RET_SACROIS,
                             "Gear" = ENGIN_COD,
                             "zone" = SECT_COD_SACROIS_NIV3,
                             "rects" = SECT_COD_SACROIS_NIV5,
                             "land_sac_kg" = QUANT_POIDS_VIF_SACROIS)

table(NewFrame$rects)
sum(NewFrame$land_sac_kg[NewFrame$rects%in%c("24E5", "24E6")], na.rm=T) #682 T relocate in 8.a

round(tapply(NewFrame$land_sac_kg, 
             list(NewFrame$rects, 
                  NewFrame$port_ret),sum, na.rm=TRUE),0) 

prod_zone_year <- NewFrame %>%
  filter(Year >= 2010) %>%
  group_by(Year, zone) %>%
  summarize(nav=length(NAVS_COD), prod=sum(land_sac_kg, na.rm=T)) %>%
  mutate(prod_tons=prod/1000)


###### Explore landings as NAs --------------------------------------
summarise_all(NewFrame, ~sum(is.na(.))) #Locate NAs

sac_na <- NewFrame %>%
  filter(is.na(land_sac_kg))

na_year_zone <- sac_na %>%
  filter(Year >=2010) %>%
  group_by(Year, zone) %>%
  summarize(n=length(MAREE_ID)) 




##-------------------------------------------------------.

ggplot(data=NewFrame) +
  geom_point(mapping=aes(x=Year, y=land_sac_kg)) +
  facet_grid(quarter~zone) + 
  theme_bw() +
  xlab("Year") + ylab("Landings (kg)") +
  theme(axis.title.x = element_text(size=16), axis.title.y = element_text(size=16),
        axis.text.x = element_text(face="bold", color="#1a0730", size=14, angle=0),
        strip.text.y=element_text(size=12,angle=0),
        axis.text.y = element_text(face="bold", color="#05000a", size=12, angle=0), 
        legend.title = element_text(size=16), 
        legend.text = element_text(size=14))


NewFrame %>%
  filter(ENGIN_COD_SACROIS%in%bolinch & !rects%in%Douarn) %>%
  group_by(NAVS_COD, ENGIN_COD_SACROIS, rects, port_ret) %>%
  summarize(PROD=sum(land_sac_kg,na.rm=T))


Channel_PIL <- filter(NewFrame, rects %in% Area) #7de+Douarn - (remove the recodage in BoB)
table(Channel_PIL$rects)

round(tapply(Channel_PIL$land_sac_kg, 
             list(Channel_PIL$rects, 
                  Channel_PIL$port_ret),sum, na.rm=TRUE),0)
# (Channel_PIL %>%
#     filter(port_ret=="XCC") %>%
#     group_by(NAVS_COD, rects) %>%
#     summarize(count=length(rects), catch=sum(land_sac_kg, na.rm=T)) %>%
#     as.data.frame())


### Check Qtité by gear
library(plyr)
(Gear_Qtsac <- ddply(Channel_PIL, c("ENGIN_COD_SACROIS"), summarise,
                    N_gear    = length(ENGIN_COD_SACROIS), 
                    sum_land = sum(land_sac_kg, na.rm=T),
                    percent_rank = sum_land/219341662*100))
#203 NA - ?
ggplot(Gear_Qtsac,aes(x=ENGIN_COD_SACROIS,y=sum_land))+
  geom_bar(stat="identity") #24 codes engins

round(tapply(Channel_PIL$land_sac_kg, 
             Channel_PIL$ENGIN_COD_SACROIS,sum, na.rm=TRUE),0)

table(Channel_PIL$ENGIN_COD_SACROIS, Channel_PIL$Gear)
#

Channel_PIL %>% dplyr::select(ENGIN_COD_SACROIS, Gear, METIER_COD_SACROIS, METIER_DCF_6_COD)
#
round(tapply(Channel_PIL$land_sac_kg, 
             list(Channel_PIL$NAVS_COD,
                  Channel_PIL$Gear), sum, na.rm=TRUE),0)

####~~ Correct ENGIN_COD_SACROIS / Metiers NA  --------------------------------
Channel_PIL$ENGIN_COD_SACROIS[Channel_PIL$ENGIN_COD_SACROIS==""] <- NA
# Channel_PIL$ENGIN_COD_SACROIS <- gsub("", NA, Channel_PIL$ENGIN_COD_SACROIS)
gear_na <- Channel_PIL %>% filter_at(vars(ENGIN_COD_SACROIS), any_vars(is.na(.)))

round(tapply(gear_na$land_sac_kg, 
             list(gear_na$NAVS_COD,
                  gear_na$Gear), sum, na.rm=TRUE),0)
# GES -> OTM
Channel_PIL %>%
  filter(NAVS_COD%in%c(716582, 651429, 925601) & Gear=="GES") %>%
  group_by(NAVS_COD, ENGIN_COD_SACROIS, Gear) %>%
  summarize(PROD=sum(land_sac_kg,na.rm=T))
  
Channel_PIL$ENGIN_COD_SACROIS[is.na(Channel_PIL$ENGIN_COD_SACROIS) & Channel_PIL$Gear =="GES" & Channel_PIL$NAVS_COD%in%c("716582", "651429","925601")] <- "OTM"

#bolinch
Channel_PIL$ENGIN_COD_SACROIS[is.na(Channel_PIL$ENGIN_COD_SACROIS) & Channel_PIL$Gear %in% bolinch] <- "PS"
#

Channel_PIL$ENGIN_COD_SACROIS[is.na(Channel_PIL$ENGIN_COD_SACROIS) & Channel_PIL$Gear =="GN" & Channel_PIL$NAVS_COD == "738594"] <- "OTB" #8 T 

## 735386 - OTB?
Channel_PIL$ENGIN_COD_SACROIS[is.na(Channel_PIL$ENGIN_COD_SACROIS) & Channel_PIL$Gear =="OTB" | Channel_PIL$Gear =="TB" & Channel_PIL$NAVS_COD == "735386"] <- "OTB" #possible OTM

Channel_PIL$ENGIN_COD_SACROIS[is.na(Channel_PIL$ENGIN_COD_SACROIS) & Channel_PIL$Gear =="TBB" & Channel_PIL$NAVS_COD == "735386"] <- "TBB"
#
Channel_PIL$ENGIN_COD_SACROIS[is.na(Channel_PIL$ENGIN_COD_SACROIS) & Channel_PIL$Gear =="OTB" & Channel_PIL$NAVS_COD == "734964"] <- "OTM"

Channel_PIL$ENGIN_COD_SACROIS[is.na(Channel_PIL$ENGIN_COD_SACROIS) & Channel_PIL$Gear =="PTB" | Channel_PIL$Gear == "SPR" & Channel_PIL$NAVS_COD == "644260"] <- "OTB"
# 735383-PTM +
Channel_PIL$ENGIN_COD_SACROIS[is.na(Channel_PIL$ENGIN_COD_SACROIS) & Channel_PIL$Gear =="PTB" | Channel_PIL$Gear == "SPR" & Channel_PIL$NAVS_COD == "735383"] <- "PTM"

### Saving some kilos
## TBB - OTB 
Channel_PIL$ENGIN_COD_SACROIS[is.na(Channel_PIL$ENGIN_COD_SACROIS) & Channel_PIL$Gear =="TBB" & Channel_PIL$NAVS_COD == "734736"] <- "OTB"
# SPR 
Channel_PIL$ENGIN_COD_SACROIS[is.na(Channel_PIL$ENGIN_COD_SACROIS) & Channel_PIL$Gear =="SPR" & Channel_PIL$NAVS_COD == "934685"] <- "PTM" #
#SDV -> SDN 
Channel_PIL$ENGIN_COD_SACROIS[is.na(Channel_PIL$ENGIN_COD_SACROIS) & Channel_PIL$Gear =="SDV"] <- "SDN"
#FPO 268 kg et LTL 185 kg #error code ..
Channel_PIL$ENGIN_COD_SACROIS[is.na(Channel_PIL$ENGIN_COD_SACROIS) & Channel_PIL$Gear =="FPO" & Channel_PIL$NAVS_COD == "645006"] <- "MIS" #weird catch!
Channel_PIL$ENGIN_COD_SACROIS[is.na(Channel_PIL$ENGIN_COD_SACROIS) & Channel_PIL$Gear =="LTL" & Channel_PIL$NAVS_COD == "925621"] <- "OTB" #?
#correct fileyeur BR
Channel_PIL$ENGIN_COD_SACROIS[is.na(Channel_PIL$ENGIN_COD_SACROIS) & Channel_PIL$Gear =="DRB" & Channel_PIL$NAVS_COD == "732240"] <- "GNS"

#--------------------------------------------------------------------.
### Verify
gear_na <- Channel_PIL %>% filter_at(vars(ENGIN_COD_SACROIS), any_vars(is.na(.))) #Few kilos

round(tapply(gear_na$land_sac_kg, 
             list(gear_na$NAVS_COD,
                  gear_na$Gear), sum, na.rm=TRUE),0) #reallocate in MIS_MIS

#few kilos that will be renamed in MIS after checking for the targeting catch of sardines

### Rename in MIS_MIS
sum(Channel_PIL$land_sac_kg[is.na(Channel_PIL$ENGIN_COD_SACROIS)], na.rm=T) #138 kg
Channel_PIL$ENGIN_COD_SACROIS[is.na(Channel_PIL$ENGIN_COD_SACROIS)] <- "MIS"

Channel_PIL <- Channel_PIL %>%
  mutate(land_sac_tons=land_sac_kg/1000)

sum(Channel_PIL$land_sac_tons, na.rm=T) #219341.7 
Gear_Qtsac <- ddply(Channel_PIL, c("ENGIN_COD_SACROIS"), summarise,
                    N_gear    = length(ENGIN_COD_SACROIS), 
                    land = sum(land_sac_tons, na.rm=T),
                    percent_rank = land/219341.7*100) #7de - 87658179

color_paired <- c("#a6cee3","#1f78b4", "#b2df8a", "#33a02c","#fb9a99","#e31a1c","#fdbf6f","#ff7f00","#cab2d6","#6a3d9a")
pie(Gear_Qtsac$percent_rank, labels = Gear_Qtsac$ENGIN_COD_SACROIS, col=color_paired, main="Pie Chart of Gear types catching Sardines")
Gear_Qtsac <- Gear_Qtsac %>%
  arrange(desc(land))
t <- dplyr::slice(Gear_Qtsac, 1:5)

pie(t$percent_rank, labels = paste0(round(t$percent_rank),"%"), col=color_paired, main="Pie Chart of Gear types catching Sardines") +
  legend("topleft", legend=t$ENGIN_COD_SACROIS, fill=color_paired) # This one, without grouping ZONE

# Graph FR landings in tons by main type of gear by ICES area
Gear_Qtsac_zone <- ddply(Channel_PIL, c("ENGIN_COD_SACROIS", "zone"), summarise,
                    N_gear    = length(ENGIN_COD_SACROIS), 
                    land = sum(land_sac_tons, na.rm=T),
                    percent_rank = land/219341.7*100) 
Gear_Qtsac_zone <- Gear_Qtsac_zone %>%
  arrange(desc(land))
t <- dplyr::slice(Gear_Qtsac_zone, 1:5)

ggplot(t , aes(x=ENGIN_COD_SACROIS, y=land, fill=zone)) + 
  geom_bar(stat ="identity") +
  scale_color_fish_d(option="Coryphaena_hippurus") + 
  theme_bw() +
  xlab("Gear (ENGIN_COD_SACROIS)") + ylab("FR landings (tons)") + labs(fill='ICES area') +
  theme(axis.title.x = element_text(size=16), axis.title.y = element_text(size=16),
        axis.text.x = element_text(face="bold", color="#1a0730", 
                                   size=14, angle=0),
        axis.text.y = element_text(face="bold", color="#05000a", 
                                   size=14, angle=0),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14))

### regroup METIERS
# 
# look at cumsum per metier
head(Channel_PIL$METIER_DCF_6_COD)
(prod_by_met <- Channel_PIL %>% 
  group_by(METIER_DCF_6_COD) %>% 
  dplyr::summarize(prod=sum(land_sac_kg, na.rm=T)) %>% 
  arrange(desc(prod)))

prod_by_met$cumsum <- cumsum(prod_by_met$prod) / sum(prod_by_met$prod)
(prod_by_met[1:20,])

Channel_PIL$METIER_DCF_6_COD[Channel_PIL$METIER_DCF_6_COD==""] <- NA

# Organize the levels of the factor by the order in the cumsum
prod_by_met$METIER_DCF_6_COD <- factor(prod_by_met$METIER_DCF_6_COD, levels=prod_by_met$METIER_DCF_6_COD)

# Create a barplot with the threshold as a hline
ggplot(filter(prod_by_met, cumsum<0.99), aes(x=METIER_DCF_6_COD, y=cumsum)) + geom_bar(stat="identity") + theme_light() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + scale_x_discrete("Métier", expand=c(0, 0)) + scale_y_continuous("Cum. production (kg)") + geom_hline(aes(yintercept = 0.99), linetype=2)

# Check per year
(stats <- Channel_PIL %>% 
    group_by(Year) %>% 
    dplyr::summarize(prod.tons=sum(land_sac_tons, na.rm = T)
                     #, effort=sum(TPS_MER, na.rm=T) #check effort calcul 
                     ))
# smooth plot just check
ggplot(stats, aes(x=Year, y=prod.tons)) + geom_smooth(color="gray40") + geom_point() + scale_y_continuous("Production (tons)") + theme_light()

# Plots the time series per final metiers (top5?)
(stats <- Channel_PIL %>% 
    group_by(Year, METIER_DCF_6_COD) %>% 
    dplyr::summarize(prd.ton=sum(land_sac_tons, na.rm = T)) %>% 
    group_by(Year) %>%
    arrange(desc(prd.ton)) %>% 
    slice(1:5))
# Check that with have the first five harbour per year
dplyr::count(stats, Year) # o

ggplot(stats, aes(x=Year, y=prd.ton)) + geom_smooth(color="gray") + geom_jitter() + facet_wrap(~METIER_DCF_6_COD)

 ggplot(stats, aes(x=Year, y=prd.ton)) + geom_bar(stat="identity") + facet_wrap(~METIER_DCF_6_COD, strip.position = "right", dir="v")

table(Channel_PIL$ENGIN_COD_SACROIS) 
 
 
## Check without PS(not taking account of 25E4/25E5)
stats <- stats %>%
  filter(!str_starts(METIER_DCF_6_COD, "PS"))

ggplot(stats, aes(x=Year, y=prd.ton)) + 
  geom_bar(stat="identity") + 
  facet_grid(METIER_DCF_6_COD~., scales="free"
             ) +
  theme_bw() +
  xlab("Year") + ylab("Landings (tons)") +
  theme(axis.title.x = element_text(size=16), axis.title.y = element_text(size=16),
        axis.text.x = element_text(face="bold", color="#1a0730", 
                                   size=14, angle=0),
        strip.text.y=element_text(size=12,angle=0),
        axis.text.y = element_text(face="bold", color="#05000a", 
                                   size=12, angle=0))

### ~~~ Rename / check NAs in metiers --------------------------------------------------.
# Select targeting Gear ---------------------------------------------------------------------------.
# Keep met_6_DCF  in OTM/PTM/OTB & PS to rename & catch > 0.1% to classify 
#others in MIS_MIS
round(tapply(Channel_PIL$land_sac_kg, 
             list(Channel_PIL$METIER_DCF_6_COD,
                  Channel_PIL$ENGIN_COD_SACROIS), sum, na.rm=TRUE),0)

sum(Channel_PIL$land_sac_tons, na.rm=T) #219341.7 Tons 

Met_6_DCF_Qtsac <- ddply(Channel_PIL, c("METIER_DCF_6_COD"), summarise,
                         N_met    = length(METIER_DCF_6_COD), 
                         sum_landT = sum(land_sac_tons, na.rm=T),
                         percent_rank = sum_landT/219341.7*100) #134 code met & 218 in NA to redistrib

Met_6_DCF_Qtsac <- Met_6_DCF_Qtsac %>% arrange(desc(sum_landT))

Met_6_DCF_Qtsac_7de <- Met_6_DCF_Qtsac %>%
  filter(!str_starts(METIER_DCF_6_COD, "PS"))

#keep NA & metiers catching >1%
met_na <- Channel_PIL %>% filter_at(vars(METIER_DCF_6_COD),any_vars(is.na(.))) #172

#What in NAs --> knowledge!
round(tapply(met_na$land_sac_kg, 
             list(met_na$NAVS_COD,
                  met_na$ENGIN_COD_SACROIS), sum, na.rm=TRUE),0)

###Correct METIER_DCF_6
Channel_PIL$METIER_DCF_6_COD[is.na(Channel_PIL$METIER_DCF_6_COD) & Channel_PIL$ENGIN_COD_SACROIS %in%  c("OTB", "PTM") & Channel_PIL$NAVS_COD %in% 735383] <- "PTM_SPF_32_69_0"

Channel_PIL$METIER_DCF_6_COD[is.na(Channel_PIL$METIER_DCF_6_COD) & Channel_PIL$ENGIN_COD_SACROIS =="OTB" & Channel_PIL$NAVS_COD %in% 644260] <- "PTM_SPF_32_69_0"

Channel_PIL$METIER_DCF_6_COD[is.na(Channel_PIL$METIER_DCF_6_COD) & Channel_PIL$ENGIN_COD_SACROIS %in% "OTB" & Channel_PIL$NAVS_COD %in% 734964] <- "OTM_SPF_32_69_0"
Channel_PIL$METIER_DCF_6_COD[is.na(Channel_PIL$METIER_DCF_6_COD) & Channel_PIL$ENGIN_COD_SACROIS %in% "OTM" & Channel_PIL$NAVS_COD %in% 716582] <- "OTM_SPF_32_69_0"

#934685 - PTMMAX.. Dim maillage?? - PTM_SPF_32_69_0
Channel_PIL %>%
  filter(NAVS_COD =="934685") %>%
  dplyr::select(port_ret, Gear, ENGIN_COD_SACROIS, METIER_DCF_6_COD, rects, land_sac_tons) #Put in PTM

Channel_PIL$METIER_DCF_6_COD[is.na(Channel_PIL$METIER_DCF_6_COD) & Channel_PIL$ENGIN_COD_SACROIS %in% c("PTM","SPR") & Channel_PIL$NAVS_COD %in% 934685] <- "PTM_SPF_32_69_0"

## PS (898415 / 911295)
  ###-------------------- /!\ 933781 error code PS --> MIS_MIS
Channel_PIL %>%
  filter(NAVS_COD =="911295" #& is.na(METIER_DCF_6_COD)
         ) %>%
  dplyr::select(port_ret, Gear, ENGIN_COD_SACROIS, METIER_DCF_6_COD, rects, land_sac_kg)

Channel_PIL$METIER_DCF_6_COD[is.na(Channel_PIL$METIER_DCF_6_COD) & Channel_PIL$NAVS_COD %in% 898415] <- "PS_SPF_0_16_0"
Channel_PIL$METIER_DCF_6_COD[is.na(Channel_PIL$METIER_DCF_6_COD) & Channel_PIL$NAVS_COD %in% 911295] <- "PS_SPF_16_31_0"

## /!\ Nav_COD 735386 & 738594  - OTB/drague.. 19 + 8T à virer!? in MIS_MIS
Channel_PIL %>%
  filter(NAVS_COD =="735386" & is.na(METIER_DCF_6_COD )) %>%
  dplyr::select(port_ret, Gear, ENGIN_COD_SACROIS, METIER_DCF_6_COD, rects, land_sac_tons)

Channel_PIL$METIER_DCF_6_COD[is.na(Channel_PIL$METIER_DCF_6_COD) & Channel_PIL$NAVS_COD %in% c(735386, 738594)] <- "MIS_MIS_0_0_0"

#recheck
Met_6_DCF_Qtsac <- ddply(Channel_PIL, c("METIER_DCF_6_COD"), summarise,
                         N_met    = length(METIER_DCF_6_COD), 
                         sum_landT = sum(land_sac_tons, na.rm=T),
                         percent_rank = sum_landT/219341.7*100) #Still 57 na

Met_6_DCF_Qtsac <- Met_6_DCF_Qtsac %>% arrange(desc(sum_landT))
met_na <- Channel_PIL %>% filter_at(vars(METIER_DCF_6_COD),any_vars(is.na(.)))

round(tapply(met_na$land_sac_tons, 
             list(met_na$NAVS_COD,
                  met_na$ENGIN_COD_SACROIS), sum, na.rm=TRUE),0) #in MIS 

####----------------------Others in MIS_MIS
#keep OTM/PTM/OTB & PS
met_cod6 <- c(unique(subset(Met_6_DCF_Qtsac, substring(METIER_DCF_6_COD, 2,3) == "TM")$METIER_DCF_6_COD), unique(subset(Met_6_DCF_Qtsac, substring(METIER_DCF_6_COD, 1,3) == "OTB")$METIER_DCF_6_COD)) #51 
## 
PS_cod6 <- c(unique(subset(Met_6_DCF_Qtsac, substring(METIER_DCF_6_COD, 1,2) == "PS")$METIER_DCF_6_COD)) #13 
#########################################################################################.
#rename 
Channel_PIL$metiers <- rep(NA,nrow(Channel_PIL))

#NewF.7de$metiers[!NewF.7de$ENGIN_COD_SACROIS %in% c("OTM", "PTM", "OTB")] <- "MIS_MIS_0_0_0"
#
round(tapply(Channel_PIL$land_sac_tons[Channel_PIL$ENGIN_COD_SACROIS %in% c("OTM", "PTM", "OTB")], 
             list(Channel_PIL$METIER_DCF_6_COD[Channel_PIL$ENGIN_COD_SACROIS %in% c("OTM", "PTM", "OTB")],
                  Channel_PIL$ENGIN_COD_SACROIS[Channel_PIL$ENGIN_COD_SACROIS %in% c("OTM", "PTM", "OTB")]), sum, na.rm=TRUE),0) 

##Refactoring to match ices fleet
MIS <- c(unique(subset(Channel_PIL, substring(METIER_DCF_6_COD, 1,3) == "MIS")$METIER_DCF_6_COD))
Channel_PIL <- Channel_PIL%>%
  mutate(metiers=ifelse(METIER_DCF_6_COD%in%MIS,
                         "MIS_MIS_0_0_0", metiers))
# Channel_PIL$metiers[subset(Channel_PIL, substring(METIER_DCF_6_COD, 1,3) == "MIS")$METIER_DCF_6_COD] <- "MIS_MIS_0_0_0"

#remaining NAs (as only few T left)
Channel_PIL$metiers[is.na(Channel_PIL$METIER_DCF_6_COD)] <- "MIS_MIS_0_0_0"

## Keep METIER_DCF_6_COD name in met_cod6 & others in MIS_MIS 
Channel_PIL$metiers[Channel_PIL$METIER_DCF_6_COD %in% met_cod6] <- Channel_PIL$METIER_DCF_6_COD[Channel_PIL$METIER_DCF_6_COD %in% met_cod6]
 # check ok
table(Channel_PIL$METIER_DCF_6_COD[Channel_PIL$METIER_DCF_6_COD %in% met_cod6],
      Channel_PIL$metiers[Channel_PIL$METIER_DCF_6_COD %in% met_cod6])

# Put ALL PS in the same code
Channel_PIL$metiers[Channel_PIL$METIER_DCF_6_COD %in% PS_cod6] <- "PS_All_0_0_All"

# others 
Channel_PIL$metiers[is.na(Channel_PIL$metiers)] <- "MIS_MIS_0_0_0"

table(Channel_PIL$METIER_DCF_6_COD, Channel_PIL$metiers)
Channel_PIL %>% filter_at(vars(metiers),any_vars(is.na(.))) #0


round(tapply(Channel_PIL$land_sac_kg[Channel_PIL$metiers %in% met_cod6], 
             list(Channel_PIL$METIER_DCF_6_COD[Channel_PIL$metiers %in% met_cod6],
                  Channel_PIL$ENGIN_COD_SACROIS[Channel_PIL$metiers %in% met_cod6]), sum, na.rm=TRUE),0)

Metier_catch <- ddply(Channel_PIL, c("metiers"), summarise,
                      N_met    = length(metiers), 
                      sum_landT = sum(land_sac_tons, na.rm=T),
                      percent_rank = sum_landT/219341.7*100) # 53 metiers

Metier_catch <- Metier_catch %>% arrange(desc(sum_landT))
#Plot Gear type Vs metiers type
par(mfrow = c(1,2))
#
pie(Gear_Qtsac$percent_rank, labels = Gear_Qtsac$ENGIN_COD_SACROIS, main="Pie Chart of Gear types catching Sardines")

pie(Metier_catch$percent_rank, labels = Metier_catch$metiers, main="Pie Chart of Fleet (DCF_6_COD)")

### Landing harbour ####
#-----------------------#
### check port (port_ret) 
(quant_port_ret <- ddply(Channel_PIL,c("port_ret"), summarise,
                         N_port = length(port_ret), 
                         sum = sum(land_sac_kg, na.rm=T))) #89 - (79 after correct)

plyr::count(nchar(Channel_PIL$port_ret))
Channel_PIL$nchar <- nchar(Channel_PIL$port_ret)

Channel_PIL$quartiers[Channel_PIL$nchar==3] <- substr(Channel_PIL$port_ret, start=2, stop=3)[Channel_PIL$nchar==3]
Channel_PIL$quartiers[Channel_PIL$nchar==5] <- Channel_PIL$port_ret[Channel_PIL$nchar==5]


round(tapply(Channel_PIL$land_sac_tons, 
             list(Channel_PIL$rects, 
                  Channel_PIL$quartiers),sum, na.rm=TRUE),0) #45T XDZ!! to remove if wanted without douarn 


table(Channel_PIL$port_ret[Channel_PIL$rects%in%Douarn])
#
round(tapply(Channel_PIL$land_sac_tons[Channel_PIL$rects%in%Douarn], 
             list(Channel_PIL$rects[Channel_PIL$rects%in%Douarn], 
                  Channel_PIL$port_ret[Channel_PIL$rects%in%Douarn]),sum, na.rm=TRUE),0)

unique(Channel_PIL$port_ret) #89
unique(Channel_PIL$quartiers) #34

unique(Channel_PIL$NAVS_COD[Channel_PIL$quartiers%in%"DZ" & !Channel_PIL$rects%in%Douarn])

Channel_PIL %>%
  dplyr::filter(NAVS_COD%in%c("734964","899957","801809","785315","785720") & 
                  quartiers%in%"DZ" & !rects%in%Douarn) %>%
  dplyr::select(NAVS_COD, zone, rects, Year)

sum(Channel_PIL$land_sac_tons[Channel_PIL$quartiers%in%"DZ" & !Channel_PIL$rects%in%Douarn], na.rm=T) #30 T

round(tapply(Channel_PIL$land_sac_tons[Channel_PIL$quartiers%in%"DZ" & !Channel_PIL$rects%in%Douarn], 
             list(Channel_PIL$rects[Channel_PIL$quartiers%in%"DZ" & !Channel_PIL$rects%in%Douarn], 
                  Channel_PIL$NAVS_COD[Channel_PIL$quartiers%in%"DZ" & !Channel_PIL$rects%in%Douarn]),sum, na.rm=TRUE),0) # 734964

Channel_PIL %>%
  filter(NAVS_COD==734964 & quartiers%in%"DZ") %>%
  select(Gear, zone, rects, quartiers, Year) %>%
  group_by(quartiers, rects) %>%
  summarize(prod=sum(land_sac_tons, na.rm=T)) #OTB when not in 25E4/25E5



## separate all in Manche (code for maritime districts) / Others
##~ 13 Quartiers maritimes + NLSCE/IJN/NLVLI/SHN??
# car <-c(unique(subset(Channel_PIL, substring(port_ret,2,3)=="BL")$port_ret), unique(subset(Channel_PIL, substring(port_ret,2,3)=="BR")$port_ret), unique(subset(Channel_PIL, substring(port_ret,2,3)=="CH")$port_ret), unique(subset(Channel_PIL, substring(port_ret,2,3)=="CN")$port_ret), unique(subset(Channel_PIL, substring(port_ret,2,3)=="DK")$port_ret), unique(subset(Channel_PIL, substring(port_ret,2,3)=="DP")$port_ret), unique(subset(Channel_PIL, substring(port_ret,2,3)=="DZ")$port_ret), unique(subset(Channel_PIL, substring(port_ret,2,3)=="FC")$port_ret), unique(subset(Channel_PIL, substring(port_ret,2,3)=="LH")$port_ret), unique(subset(Channel_PIL, substring(port_ret,2,3)=="MX")$port_ret), unique(subset(Channel_PIL, substring(port_ret,2,3)=="PL")$port_ret), unique(subset(Channel_PIL, substring(port_ret,2,3)=="SB")$port_ret),unique(subset(Channel_PIL, substring(port_ret,2,3)=="SM")$port_ret), "NLSCE", "IJN", "NLVLI", "SHN") 
#

###---------- Extract the 13 quartiers for Channel not Douarn  --------------------
# setwd("~/Data_extraction")
# Channel_PIL<- read.csv("sacroisRData_PIL7de_douarn_2000-2020.csv")
## Channel_PIL_prev <- read.csv("sacrois_PIL7de_douarn_2000-2020.csv")

unique(Channel_PIL$quartiers)

round(tapply(Channel_PIL$land_sac_tons, 
             list(Channel_PIL$rects, 
                  Channel_PIL$quartiers),sum, na.rm=TRUE),0)

mar_channel <- c("BR", "BL", "CH", "CN", "DK", "DP", "DZ", "FC", "LH", "MX", "PL", "SB", "SM","NLSCE", "JN", "NLVLI", "HN") #17 

### Create variable for Channel/Douarn 
Channel_PIL$region <- rep(NA,nrow(Channel_PIL))
Channel_PIL <- Channel_PIL %>%
  mutate(region=ifelse(quartiers%in%mar_channel&!rects%in%Douarn,
                                "Channel", region))
Channel_PIL <- Channel_PIL %>%
  mutate(region=ifelse(rects%in%Douarn,
                       "Douarn", region))
Channel_PIL$region <- as.factor(Channel_PIL$region)
summary(Channel_PIL$region)
table(Channel_PIL$rects, Channel_PIL$quartiers, Channel_PIL$region)

na_region <- Channel_PIL %>%
  dplyr::filter(is.na(region)) %>%
  group_by(MAREE_ID, Year) %>%
  dplyr::summarize(land=sum(land_sac_tons, na.rm=T)) # 18T over the time-series
 

# version Atelier-R
Channel_PIL_port <- filter(Channel_PIL, !is.na(region)) #99569 

head(Channel_PIL_port)
unique(Channel_PIL_port$port_ret) #
unique(Channel_PIL_port$ENGIN_COD_SACROIS) #PS present
dim(Channel_PIL_port)

round(tapply(Channel_PIL_port$land_sac_tons, 
             list(Channel_PIL_port$ENGIN_COD_SACROIS, 
                  Channel_PIL_port$rects),sum, na.rm=TRUE),0) #PS in 25E4/25E5 ok 

# write.csv(Channel_PIL_port, "~/Data_extraction/sacroisRData_PIL7de_douarn_2000-2020.csv", row.names = F) #quarter/separate date/gear/metiers/maritime district corrected ...


(stats <- Channel_PIL_port %>% 
    group_by(port_ret, Year, quartiers) %>% 
    dplyr::summarize(prd.tons=sum(land_sac_tons, na.rm = T)) %>% 
    group_by(Year) %>%
    arrange(desc(prd.tons)) %>% 
    slice(1:5))
# Check that with have the first five harbour per year
dplyr::count(stats, Year) # o

# Plots the time series per harbour
ggplot(stats, aes(x=Year, y=prd.tons)) + geom_smooth(color="gray") + geom_jitter() + facet_wrap(~quartiers, scales="free_y")
#
round(tapply(Channel_PIL_port$land_sac_tons, 
             list(Channel_PIL_port$rects, 
                  Channel_PIL_port$quartiers),sum, na.rm=TRUE),0)

#
sum(Channel_PIL_port$land_sac_tons, na.rm=T) #219323.8
(quant_port_ret <- ddply(Channel_PIL_port,c("port_ret"), summarise,
                         N_port = length(port_ret), 
                         sum_qt = sum(land_sac_tons, na.rm=T),
                         perc_rank = sum_qt/219323.8*100)) #

quant_port_ret <- quant_port_ret %>%
  arrange(desc(perc_rank))


#Top '12' to plot - < 0.1%
t_land <- dplyr::slice(quant_port_ret, 1:12)

## PLOT Qtité
ggplot(t_land,aes(x=port_ret,y=sum_qt))+
  geom_bar(stat="identity")

table(Channel_PIL_port$ENGIN_COD_SACROIS)


# Subset Channel only
######################################################################################.
##---->>>> Production gear/metiers/port_ret or quartiers in 7.de   <<<<----

NewF.7de <- filter(Channel_PIL_port, region%in%"Channel") #Without Douarn
#any PS? - Yes but NAs or 0
round(tapply(NewF.7de$land_sac_tons[NewF.7de$ENGIN_COD_SACROIS%in%bolinch], 
             list(NewF.7de$rects[NewF.7de$ENGIN_COD_SACROIS%in%bolinch], 
                  NewF.7de$port_ret[NewF.7de$ENGIN_COD_SACROIS%in%bolinch]),sum, na.rm=TRUE),0)
NewF.7de %>%
  filter(ENGIN_COD_SACROIS %in% bolinch) %>%
  dplyr::select(NAVS_COD, rects, ENGIN_COD_SACROIS, metiers, land_sac_kg, Year)

(PS_gear <- NewF.7de %>% 
    filter(ENGIN_COD_SACROIS %in% bolinch) %>% 
    group_by(port_ret) %>%
    do(Tcatch=sum(.$land_sac_tons, na.rm=T)) %>%
    as.data.frame())                            #nothing!

# NewF.7de <- NewF.7de %>% filter(!str_starts(metiers, "PS")) #
NewF.7de <- NewF.7de %>% filter(ENGIN_COD_SACROIS!="PS") #61,919 

NewF.7de$rects <- as.factor(NewF.7de$rects)
table(NewF.7de$rects)
table(NewF.7de$port_ret)

NewF.7de <- select(NewF.7de, -PAVILLON) #cause all in FRA
# write.csv(NewF.7de, "~/Data_extraction/sacroisRData_PIL7de_only_2000-2020.csv", row.names = F)

# round(tapply(NewF.7de$land_sac_tons, 
#              list(NewF.7de$metiers,
#                   NewF.7de$ENGIN_COD_SACROIS), sum, na.rm=TRUE),0)

sum(NewF.7de$land_sac_tons, na.rm=T) # 87689.18 tons!

(stats <- NewF.7de %>% 
    group_by(Year, ENGIN_COD_SACROIS) %>% 
    dplyr::summarize(prd.tons=sum(land_sac_tons, na.rm = T)) %>% 
    # group_by(Year) %>%
    arrange(Year)) #%>% 
#slice(1:5))
# Check that with have the first five harbour per year
dplyr::count(stats, Year) # o

Gear_Qtsac <- ddply(NewF.7de, c("ENGIN_COD_SACROIS"), summarise,
                    N_gear    = length(ENGIN_COD_SACROIS), 
                    sum_land = sum(land_sac_tons, na.rm=T),
                    percent_rank = sum_land/87689.18*100)
(Gear_Qtsac <- Gear_Qtsac %>% arrange(desc(sum_land)))

(stats <- stats %>%
    filter(ENGIN_COD_SACROIS%in%c("OTM","PTM", "OTB")))

stats$ENGIN_COD_SACROIS <- factor(stats$ENGIN_COD_SACROIS, levels=c("OTM", "PTM", "OTB"))

ggplot(stats, aes(x=Year, y=prd.tons)) + 
  geom_bar(stat="identity") + 
  facet_grid(ENGIN_COD_SACROIS~., #scales="free"
  ) +
  theme_bw() +
  xlab("Year") + ylab("Landings (tons)") +
  theme(axis.title.x = element_text(size=16), axis.title.y = element_text(size=16),
        axis.text.x = element_text(face="bold", color="#1a0730", 
                                   size=14, angle=0),
        strip.text.y=element_text(size=12,angle=0),
        axis.text.y = element_text(face="bold", color="#05000a",
                                   size=12, angle=0))


### landings port
(stats <- NewF.7de %>% 
    group_by(port_ret, Year, quartiers) %>% 
    dplyr::summarize(prd.tons=sum(land_sac_tons, na.rm = T)) %>% 
    group_by(Year) %>%
    arrange(desc(prd.tons)) %>% 
    slice(1:5))
# Check that with have the first five harbour per year
dplyr::count(stats, Year) # o

stats_5 <- stats %>%
  filter(quartiers%in%c("BL", "DP", "FC", "JN", "NLSCE"))

stats_5$quartiers <- factor(stats_5$quartiers, levels=c("FC","NLSCE","JN","BL", "DP"))
# Plots the time series per harbour
ggplot(stats_5, aes(x=Year, y=prd.tons)) + 
  geom_bar(stat="identity") +
  facet_grid(quartiers~., #scales="free"
             ) +
  geom_vline(aes(xintercept = 2010), linetype=2) +
  xlab("Year") + ylab("Landings (tons)") +
  theme_bw() +
  theme(axis.title.x = element_text(size=16), axis.title.y = element_text(size=16),
        axis.text.x = element_text(face="bold", color="#1a0730", 
                                   size=14, angle=0),
        strip.text.y=element_text(size=12,angle=0),
        axis.text.y = element_text(face="bold", color="#05000a", 
                                   size=12, angle=0))


##--~ INTERCATCH FORMAT - autoices metiers# check compil_dataset.R script------------------------------------------------------------------
# setwd("~/Data_extraction")
# Channel_PIL_port <- read.csv("sacrois_PIL7de_2000-2020.csv")
# Channel_PIL_port$ID <- seq.int(nrow(Channel_PIL_port))
# Channel_PIL_port <- dplyr::select(Channel_PIL_port, -X)

## Prod in year by Quarter
round(tapply(NewF.7de$land_sac_tons, 
             list(NewF.7de$Year,
                  NewF.7de$quarter), sum, na.rm=TRUE),0)

NewF.7de$quarter <- as.factor(NewF.7de$quarter)

(qtsac <- ddply(NewF.7de, c("Year", "quarter"), summarise,
                    N_mar    = length(MAREE_ID), 
                    prod_sac_T = sum(land_sac_tons, na.rm=T)))

ggplot(qtsac, aes(x=Year, y=prod_sac_T, fill=quarter)) + 
  geom_bar(stat="identity", aes(fill=quarter), position="dodge") +
  xlab("Year") + ylab("Landings (tons)") + labs(fill='Quarter') +
  #scale_fill_brewer(palette = "Accent") +
  scale_fill_fish_d(option="Acanthurus_leucosternon") +
  add_fishape(family = "Clupeidae",
              option = "Jenkinsia_lamprotaenia",
              xmin = 2000, xmax = 2002, ymin = 5000, ymax = 6000,
              fill = fish(option = "Acanthurus_leucosternon", n = 4)[2],
              alpha = 0.9) +
  theme_gray() +
  theme(axis.title.x = element_text(size=16), axis.title.y = element_text(size=16),
        axis.text.x = element_text(face="bold", color="#1a0730", 
                                   size=14, angle=0),
        axis.text.y = element_text(face="bold", color="#05000a", 
                                   size=12, angle=0),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14))

#just year
(qtsac_y <- ddply(NewF.7de, c("Year"), summarise,
                N_nav    = length(NAVS_COD), 
                prod_sac_T = sum(land_sac_tons, na.rm=T)))
#
ggplot(qtsac_y, aes(x=Year, y=prod_sac_T)) + 
  geom_bar(stat="identity", fill="Blue") +
  xlab("Year") + ylab("Landings (tons)") +
    add_fishape(family = "Clupeidae",
              option = "Jenkinsia_lamprotaenia",
              xmin = 2000, xmax = 2002, ymin = 10000, ymax = 12000,
              fill = fish(option = "Acanthurus_leucosternon", n = 4)[2],
              alpha = 0.9) +
  theme_bw() +
  theme(axis.title.x = element_text(size=16), axis.title.y = element_text(size=16),
        axis.text.x = element_text(face="bold", color="#1a0730", 
                                   size=14, angle=0),
        axis.text.y = element_text(face="bold", color="#05000a", 
                                   size=12, angle=0))

### 2010 PCB contamination -----------------------------------------------------
NewF.7de_2000 <- NewF.7de %>%
                  dplyr::filter(Year <= 2010)
NewF.7de_2010 <- NewF.7de %>%
  dplyr::filter(Year > 2010)
#
 ## Dbq/port before-after
land_lieu <- ddply(NewF.7de_2000,c("port_ret"), summarise,
                        N_port = length(port_ret), 
                        sum_qt = sum(land_sac_tons, na.rm=T))
land_lieu <- land_lieu %>%
              arrange(desc(sum_qt))
t <- dplyr::slice(land_lieu, 1:5)

ggplot(t,aes(x=port_ret,y=sum_qt))+
  geom_bar(stat="identity") + #ylim(0, 61e+06) + 
  xlab("Landing port") + ylab("Landings (tons)") +
  theme_bw() +
  theme(axis.title.x = element_text(size=16), axis.title.y = element_text(size=16),
        axis.text.x = element_text(face="bold", color="#1a0730", 
                                   size=14, angle=0),
        axis.text.y = element_text(face="bold", color="#05000a", 
                                   size=14, angle=45))
        
#
land_lieu <- ddply(NewF.7de_2010,c("port_ret"), summarise,
                   N_port = length(port_ret), 
                   sum_qt = sum(land_sac_tons, na.rm=T))
land_lieu <- land_lieu %>%
  arrange(desc(sum_qt))
t <- dplyr::slice(land_lieu, 1:5)

ggplot(t,aes(x=port_ret,y=sum_qt))+
  geom_bar(stat="identity") + #ylim(0, 60e+06) + 
  xlab("Landing port") + ylab("Landings (tons)") +
  theme_bw() +
  theme(axis.title.x = element_text(size=16), axis.title.y = element_text(size=16),
        axis.text.x = element_text(face="bold", color="#1a0730", 
                                   size=14, angle=0),
        axis.text.y = element_text(face="bold", color="#05000a", 
                                   size=14, angle=45))

  ## Catch_by_gear before-after
catch_g2000 <- ddply(NewF.7de_2000,c("ENGIN_COD_SACROIS"), summarise,
                   N_port = length(ENGIN_COD_SACROIS), 
                   sum_qt = sum(land_sac_tons, na.rm=T))
catch_g2000 <- catch_g2000 %>%
  arrange(desc(sum_qt))
t <- dplyr::slice(catch_g2000, 1:5)
ggplot(t,aes(x=ENGIN_COD_SACROIS,y=sum_qt))+
  geom_bar(stat="identity")
#
catch_g10 <- ddply(NewF.7de_2010,c("ENGIN_COD_SACROIS"), summarise,
                   N_port = length(ENGIN_COD_SACROIS), 
                   sum_qt = sum(land_sac_tons, na.rm=T))
catch_g10 <- catch_g10 %>%
  arrange(desc(sum_qt))
t <- dplyr::slice(catch_g10, 1:5)
ggplot(t,aes(x=ENGIN_COD_SACROIS,y=sum_qt))+
  geom_bar(stat="identity")
#

#### Douarnenez in 2-Douarnenez_select.R


#### VENTES 7.de + douarn ##---------------------------------------------------------------------------------
#~~ Load and separate the date ####
ventes <- read.csv("~/Data_extraction/ISIH-503343-donneesVente-20210118160841.txt", sep=";")
#
#ventes.b <- read.csv("~/Data_extraction/ISIH-503343-donneesVente-20210203152138.txt", sep=";")
head(ventes)
table(ventes$LIEU_COD, ventes$TLIEU_COD)

setwd("~/Data_extraction")
library(readxl)
ref_harb <- read_excel("referentiels_SIH/REF_PORT.xls", 
                       sheet = "REF_PORT")
head(ref_harb)


round(tapply(ventes$SEQV_QUANT, 
             list(ventes$LIEU_COD,
                  ventes$TLIEU_COD), sum, na.rm=TRUE),0)
##-------------------------------------------------------------.
### Separate the date (with vente dat_deb)
str(ventes$VENTE_DAT_DEB)
ventes$VENTE_DAT_DEB <- dmy_hms(ventes$VENTE_DAT_DEB)
# Extract a component & create quarter:
ventes$Year <- year(ventes$VENTE_DAT_DEB)
ventes$Month <- month(ventes$VENTE_DAT_DEB)
ventes$quarter <- quarter(ventes$VENTE_DAT_DEB)
names(ventes)
summary(ventes)
#
ventes %>%
  dplyr::select(VENTE_DAT_DEB, Year, Month, quarter)
##--------------------------------------------------------------.

# Prefer a left_join than a merge!
#ventes_year <- merge(ventes, ref_harb, by.x = "LIEU_COD", by.y="Code_lieu", all.x=T)
head(ventes)
ventes_year <- left_join(ventes, dplyr::rename(ref_harb, LIEU_COD=Code_lieu), by="LIEU_COD")
head(ventes_year)

lieu_vente <- c(unique(ventes_year$LIEU_COD)) #77
car <- c(unique(NewF.7de$port_ret)) #56  

unique(lieu_vente[!lieu_vente %in% car]) # Lieu de vente pas dans sac! 
table(ventes_year$LIEU_COD)


summary(ventes_year$SEQV_QUANT)# keep neg values
which(is.na(ventes_year$SEQV_QUANT))#0

Navs_sac <- c(unique(NewF.7de$NAVS_COD)) #577

#--------------------------------------------------------------.
#### select ventes in 7de according to landing ports (sac vessels ?)
ventes_year$ncar <- nchar(ventes_year$LIEU_COD) #create vector for character number in the LIEU_COD

#Then split the 3 character COD to keep the 2 end letters for the maritime district only.
ventes_year$marit[ventes_year$ncar==3] <- substr(ventes_year$LIEU_COD, start=2, stop=3)[ventes_year$ncar==3]
ventes_year$marit[ventes_year$ncar==5] <- ventes_year$LIEU_COD[ventes_year$ncar==5]

head(ventes_year$marit)
#Keep only the Channel shore as in SACROIS (13 districts + 4 Netherlands)
ventes_sac <- filter(ventes_year, marit%in%c("BR", "BL", "CH", "CN", "DK", "DP", "DZ", "FC", "LH", "MX", "PL", "SB", "SM","NLSCE", "JN", "NLVLI", "SHN"))

#ventes_sac_nav <- dplyr::filter(ventes_sac, NAVS_COD %in% Navs_sac) #73,281!
unique(ventes_sac$LIEU_COD)
unique(ventes_sac$marit)


#----------------------------------------------------------------------.
### /!\ doublons de certaines lignes - repeat "Port"xxx or "Criée"xxx
round(tapply(ventes_sac$SEQV_QUANT, list(ventes_sac$Year,
                                         ventes_sac$marit),sum, na.rm=TRUE),0)

table(ventes_sac$Type_lieu, ventes_sac$LIEU_COD)
table(ventes_sac$Type_lieu, ventes_sac$TLIEU_COD) # TLIEU_COD???

round(tapply(ventes_sac$SEQV_QUANT, list(ventes_sac$LIEU_COD,
                                         ventes_sac$Type_lieu),sum, na.rm=TRUE),0)
 # Keep Port "Type_lieu" BUT /!\ to keep NLSCE/NLVLI/IJN 
ventes_sac %>%
  filter(Type_lieu%in%"P" | LIEU_COD%in%c("NLSCE", "NLVLI", "IJN")) %>%
  group_by(ncar) %>%
  dplyr::summarize(prod=sum(SEQV_QUANT, na.rm = T))#

ventes_sac <- filter(ventes_sac, Type_lieu%in%"P" | LIEU_COD%in%c("NLSCE", "NLVLI", "IJN")) #114,441
  
anyDuplicated(ventes_sac)
# check total catch per year
round(tapply(ventes_sac$SEQV_QUANT, list(ventes_sac$Year
                                         #,ventes_sac$marit
                                         ),sum, na.rm=TRUE),0)
summary(ventes_sac)
# write.csv(ventes_sac,"~/Data_extraction/Ventes_PIL7de_Douarn_2000.2020.csv", row.names = F)

###--->>> Production /graphs -----
# landings port
(stats <- ventes_sac %>% 
   group_by(LIEU_COD, Year, marit) %>% 
   dplyr::summarize(prd.kg=sum(SEQV_QUANT, na.rm = T)) %>% 
   group_by(Year) %>%
   arrange(desc(prd.kg)) %>% 
   slice(1:5))
# Check that with have the first five harbour per year
dplyr::count(stats, Year) # 
stats <- stats %>%
  mutate(Tcatch_tons = prd.kg/1000)

stats_5 <- stats %>%
   filter(marit%in%c("BL", "DP", "FC", "JN", "NLSCE"))
# 
stats_5$marit <- factor(stats_5$marit, levels=c("FC", "DP","BL","NLSCE","JN"))
# Plots the time series per harbour
ggplot(stats, aes(x=Year, y=Tcatch_tons)) + 
  geom_bar(stat="identity") +
  facet_grid(marit~., #scales="free"
  ) +
  geom_vline(aes(xintercept = 2010), linetype=2) +
  xlab("Year") + ylab("Landings (tons)") +
  theme_bw() +
  theme(axis.title.x = element_text(size=16), axis.title.y = element_text(size=16),
        axis.text.x = element_text(face="bold", color="#1a0730", 
                                   size=14, angle=0),
        strip.text.y=element_text(size=12,angle=0),
        axis.text.y = element_text(face="bold", color="#05000a", 
                                   size=12, angle=0))


# Split douarn
unique(ventes_sac$LIEU_COD)

d_ventes <- ventes_sac %>%
  filter(LIEU_COD=="XDZ")   #77,522

# write.csv(d_ventes, "~/Data_extraction/Ventes_douarn_PIL_2000.2020.csv")
##-------------------------------------------------------.

# Keep without XDZ (7.de)
channel_ventes <- ventes_sac %>%
  filter(LIEU_COD!="XDZ")  #36,919

ventes_ym <- ddply(channel_ventes, c("Year", "Month"), summarise,
                   N_port    = length(LIEU_COD), 
                   land_kg = sum(SEQV_QUANT))

round(tapply(channel_ventes$SEQV_QUANT, list(channel_ventes$Year),sum, na.rm=TRUE),0)



#### Split 2020 #----------------------------
channel_ventes2020 <- channel_ventes %>%
  filter(Year%in%2020)

summary(channel_ventes2020)
head(channel_ventes2020)
sum(channel_ventes2020$SEQV_QUANT)

channel_ventes2020 <- channel_ventes2020%>%
  mutate(Qt_tons = SEQV_QUANT/1000)

round(tapply(channel_ventes2020$Qt_tons, list(channel_ventes2020$SEQV_CATL,
                                                 channel_ventes2020$Month),sum, na.rm=TRUE),0)
unique(channel_ventes2020$NAVS_COD)

channel_ventes2020 %>%
  group_by(marit) %>%
  dplyr::summarize(prod=sum(Qt_tons)) %>%
  #dplyr::filter(prod >= 0.5) %>%
  arrange(desc(prod))#



#-------------------------------------------------------------------.
##### Comp sacrois/ventes - spot the diff #--------------------------------------------------------------------
round(tapply(channel_ventes$SEQV_QUANT, 
             list(channel_ventes$LIEU_COD, 
                  channel_ventes$Year),sum, na.rm=TRUE),0)
#
Prod_in_harbour <- ddply(channel_ventes, c("LIEU_COD"), summarise,
                         N_port    = length(LIEU_COD), 
                         land_kg = sum(SEQV_QUANT))    #28 ports
#
(Prod_port_sac <- ddply(NewF.7de,c("port_ret"), summarise,
                         N_port = length(port_ret), 
                         sum = sum(land_sac_tons, na.rm=T))) #57 
#combine with previous summarise quant in port return in SACROIS
comp_sac_ventes <- left_join(Prod_port_sac, dplyr::rename(Prod_in_harbour, port_ret=LIEU_COD), by="port_ret")
#
comp_sac_ventes <- comp_sac_ventes %>% dplyr::filter(sum > 50) #Still 40 ports

#  #more NAVS in Sac7de than in ventes..
# nav_diff <- unique(Navs_sac[!Navs_sac %in% ventes_sac$NAVS_COD]) #50 à 60 in sac, not in ventes
# 
# NewF.7de %>%
#   dplyr::filter(NAVS_COD %in% nav_diff) %>%
#   summarise(total_catch = sum(land_sac_kg, na.rm=T)) #40.5 t
# 
# resume_ventes7de <- ventes_sac %>%
#   dplyr::group_by(NAVS_COD, Year) %>%
#   dplyr::summarize(prod=sum(SEQV_MONTANT, na.rm = T))
# 
# ## Spotted recurrent vessels with disparate landings 
# NewF.7de %>%
#   dplyr::filter(NAVS_COD == c("726643","716582","716999") & Year == c("2004", "2005", "2009", "2010")) %>%
#   dplyr::select(NAVS_COD, port_ret, rects, land_sac_kg, Year, Month)
# 
# 
# ventes_sac %>%
#   dplyr::filter(NAVS_COD == c("726643") & Year == c("2005")) %>%
#   dplyr::select(NAVS_COD, LIEU_COD, SEQV_QUANT, SEQV_MONTANT, Year, Month)
# NewF.7de %>%
#   dplyr::filter(NAVS_COD == c("726643") & Year == c("2005")) %>%
#   dplyr::select(NAVS_COD, port_ret, rects, land_sac_kg, Year, Month)






#################;
#### LOGBOOK  ##---------------------------------------------------------------------------------
#################;
#~~ 7.de (-25E4/25E5) ####
  # < 2009 (not available on the SIH)

Log <- read_excel("~/Data_extraction/ISIH_donneesMaree-2000_2008_CVignot.xlsx", 
                  sheet = "Marees_brutes_Sardine")
  # 2009 - 2020
Logbook <- read.csv("~/Data_extraction/ISIH-503343-donneesMaree-2009_2020.txt", sep=";", stringsAsFactors = FALSE)

glimpse(Log)
glimpse(Logbook)

# Diff 4 variables in Logbook > 2009 not in < 2009: 
var_diff <- c(setdiff(names(Logbook), names(Log)))
# [1] "TLIEU_COD_RET_PERE" "LIEU_COD_RET_PERE"  "TSECT_COD_PERE"     "SECT_COD_PERE"      "SEQP_PROF"
Logbook %>%
  dplyr::select("TLIEU_COD_RET_PERE", "LIEU_COD_RET_PERE", "TSECT_COD_PERE", "SECT_COD_PERE", "SEQP_PROF")

Logbook %>%
  dplyr::select("SECT_COD", "SECT_COD_PERE")

# Keep same variables in both databases to join
Logbook <- Logbook %>%
  dplyr::select(-TLIEU_COD_RET_PERE, -LIEU_COD_RET_PERE, -TSECT_COD_PERE, -SECT_COD_PERE)

Logbook <- as_tibble(Logbook)
Log <- as_tibble(Log)

# Join the 2 df - bind rows
sapply(Log, class)
# Convert columns to match (conflicts some character class as.integer)
Log[c("MAREE_RAP", "MAREE_ORI_ID", "MAREE_QUAL_ENG", "MAREE_QUAL_TEMPS", "MAREE_QUAL_SECT", "MAREE_QUAL_DATE", "CAPT_GENER", "CAPT_RAP")] <- lapply(
  Log[c("MAREE_RAP", "MAREE_ORI_ID", "MAREE_QUAL_ENG", "MAREE_QUAL_TEMPS", "MAREE_QUAL_SECT", "MAREE_QUAL_DATE", "CAPT_GENER", "CAPT_RAP")], 
  as.integer)

#Log %>% mutate_at(is.character, as.integer)
LogAll <- dplyr::bind_rows(Log, Logbook) #151,281 OK!
head(LogAll)
rm(Logbook)
# Checking data matching SACROIS -------------------------------------------------------
# Separate the date (with Maree_dat_dep) -- Lubridate #
str(LogAll$MAREE_DAT_DEP) #chr
#
LogAll$MAREE_DAT_DEP <- dmy_hms(LogAll$MAREE_DAT_DEP)
# Extract a component & create quarter:
LogAll$Year <- year(LogAll$MAREE_DAT_DEP)
LogAll$Month <- month(LogAll$MAREE_DAT_DEP)
LogAll$quarter <- quarter(LogAll$MAREE_DAT_DEP)
names(LogAll)

# Relocate some missing SECT_COD -----------
table(LogAll$SECT_COD)
Area7_log <- c("", "007000", "007D00", "007E00", "27", "25E4", "25E5", "26E6","26E7","26E8","27E5", "27E6", "27E7", "27E9", "28E3", "28E4", "28E5", "28E6", "28E7", "28E8", "28E9", "28F0", "29E5", "29E7", "29E8", "29E9", "29F0", "29F1", "30E9", "30F0")

### check port
table(LogAll$LIEU_COD_RET)

round(tapply(LogAll$CAPT_QTOT, 
             list(LogAll$LIEU_COD_RET
                  #, LogAll$Year
                  ),sum, na.rm=TRUE),0) #230

plyr::count(nchar(LogAll$LIEU_COD_RET))
LogAll %>%
  filter(nchar(LIEU_COD_RET)==2) #SN
LogAll$nchar <- nchar(LogAll$LIEU_COD_RET)

LogAll$quartiers[LogAll$nchar==3] <- substr(LogAll$LIEU_COD_RET, start=2, stop=3)[LogAll$nchar==3]
LogAll$quartiers[LogAll$nchar==5] <- LogAll$LIEU_COD_RET[LogAll$nchar==5]

unique(LogAll$quartiers)

#Correct LIEU_COD_RET / SECT_COD / Vessels  
# All *DZ in XDZ
unique(subset(LogAll, substring(LIEU_COD_RET,2,3)=="DZ")$LIEU_COD_RET)
 # ou
unique(LogAll$LIEU_COD_RET[LogAll$quartiers%in%"DZ"])

LogAll$LIEU_COD_RET[LogAll$quartiers %in% "DZ"] <- "XDZ"

Logbook_dt <- as.data.table(LogAll)
# Fishing in 29E5 and landing in XCC --> 25E5
Logbook_dt %>%
  filter(SECT_COD=="29E5" & LIEU_COD_RET=="XCC") # same vessel 898402 gear code in NA (code 725) but is a PS
# Logbook_dt %>%
#   filter(NAVS_COD%in%898402) #PS
Logbook_dt$SECT_COD[Logbook_dt$SECT_COD %in% "29E5" & Logbook_dt$LIEU_COD_RET %in% "XCC"] <- "25E5"
#WAR ROAG III
Logbook_dt[NAVS_COD %in% 898415 & SECT_COD %in% c("27","") & LIEU_COD_RET %in% "XDZ", c("SECT_COD") := list("25E5")]
Logbook_dt[NAVS_COD %in% 898415 & SECT_COD %in% c("27","", "008000") & LIEU_COD_RET %in% c("XLO","XCC"), c("SECT_COD") := list("24E6")]
Logbook_dt[NAVS_COD %in% 898415 & SECT_COD %in% c("27","") & LIEU_COD_RET %in% "AGV", c("SECT_COD") := list("24E5")]
#Le NATIF II
Logbook_dt[NAVS_COD %in% 719993 & SECT_COD %in% c("27",""), c("SECT_COD") := list("22E7")]
##
table(Logbook_dt$NAVS_COD[Logbook_dt$LIEU_COD_RET %in%"CRU"], Logbook_dt$SECT_COD[Logbook_dt$LIEU_COD_RET %in%"CRU"])
# idem CRU in ALS
Logbook_dt$LIEU_COD_RET[Logbook_dt$LIEU_COD_RET %in% "CRU"] <- "ALS"

## Filter 7.de & douarn -------
car <- c("BR", "BL", "CH", "CN", "DK", "DP", "DZ", "FC", "LH", "MX", "PL", "SB", "SM", "NLSCE", "JN", "NLVLI", "HN")


Log7ded <- Logbook_dt %>%
  filter(SECT_COD%in%Area7_log & quartiers%in%car) #37,524

summary(Log7ded) # Cut before 2000
Log7ded <- Log7ded %>%
  filter(Year >= 2000) #36,836

Log7ded <- Log7ded %>%
  dplyr::select(MAREE_ID, NAVS_COD, LIEU_COD_PAYSDEB, TLIEU_COD_RET, LIEU_COD_RET, LIEU_COD_DEP, MAREE_DAT_DEP, MAREE_DAT_RET, MAREE_ORI_ID, MAREE_DUREE, PECHE_ID, ENGIN_COD, ENGF_COD, PECHE_MAILLAGE, SEQP_ID, SEQP_NB_OPER, SEQP_TP_NAVH, SECT_COD, ESP_COD, ESPF_COD, CAPT_ID:CAPT_QTOT, Month, Year, quarter, quartiers)

table(Log7ded$LIEU_COD_PAYSDEB)
Log7ded %>%
  filter(LIEU_COD_PAYSDEB=="NLD")%>%
  dplyr::select(NAVS_COD, LIEU_COD_RET, ENGF_COD, SECT_COD, Year) #Just keep as present ports in SACROIS

round(tapply(Log7ded$CAPT_QTOT, 
             list(Log7ded$SECT_COD, 
                  Log7ded$quartiers),sum, na.rm=TRUE),0) #

table(Log7ded$LIEU_COD_RET[Log7ded$SECT_COD%in%""])
unique(Log7ded$LIEU_COD_RET) #46
unique(Log7ded$quartiers) #17

## SHN??
Log7ded %>%
  filter(LIEU_COD_RET=="SHN") %>%
  dplyr::select(NAVS_COD, ENGF_COD, ENGIN_COD, Year, CAPT_QTOT) #Keep

round(tapply(Log7ded$CAPT_QTOT, 
             list(Log7ded$SECT_COD, 
                  Log7ded$LIEU_COD_RET),sum, na.rm=TRUE),0)

## Gear code
#NA et NK!
gear_na <- Log7ded %>% filter_at(vars(ENGF_COD), any_vars(is.na(.)))
round(tapply(gear_na$CAPT_QTOT, 
             list(gear_na$LIEU_COD_RET),sum, na.rm=TRUE),0)
#
round(tapply(gear_na$CAPT_QTOT, 
             list(gear_na$ENGIN_COD),sum, na.rm=TRUE),0)

round(tapply(gear_na$CAPT_QTOT, 
             list(gear_na$NAVS_COD,
                  gear_na$quartiers),sum, na.rm=TRUE),0)

# Correct as SACROIS
# GES -> OTM
Log7ded$ENGF_COD[is.na(Log7ded$ENGF_COD) & Log7ded$NAVS_COD == "716582" | Log7ded$NAVS_COD == "651429" | Log7ded$NAVS_COD == "925601"] <- "OTM"
#
Log7ded$ENGF_COD[is.na(Log7ded$ENGF_COD) & Log7ded$NAVS_COD == "738594"] <- "OTB" #8 T 
## 735386 - OTB
Log7ded %>%
  filter(NAVS_COD%in%735386) #OTB
Log7ded$ENGF_COD[is.na(Log7ded$ENGF_COD) & Log7ded$NAVS_COD == "735386"] <- "OTB" #
#
Log7ded$ENGF_COD[is.na(Log7ded$ENGF_COD) & Log7ded$NAVS_COD == "734964"] <- "OTM"

Log7ded$ENGF_COD[is.na(Log7ded$ENGF_COD) & Log7ded$NAVS_COD == "644260"] <- "OTB"
# 735383-PTM +
Log7ded$ENGF_COD[is.na(Log7ded$ENGF_COD) & Log7ded$NAVS_COD == "735383"] <- "PTM"
###
#SDV / SV -> SDN?
Log7ded %>%
  filter(ENGF_COD=="SDV") %>%
  group_by(NAVS_COD, quartiers) %>%
  summarize(sum=sum(CAPT_QTOT, na.rm=T)) #35 T just in DZ .. BOLINCHEUR

Log7ded$ENGF_COD[Log7ded$ENGF_COD =="SDV" & Log7ded$NAVS_COD=="385555"] <- "PS"
Log7ded$ENGF_COD[Log7ded$ENGF_COD =="SV" & Log7ded$NAVS_COD=="716633"] <- "PS"

Log7ded %>%
  filter(ENGF_COD=="SDN") %>%
  group_by(NAVS_COD, quartiers) %>%
  summarize(sum=sum(CAPT_QTOT, na.rm=T)) #keep for now 

#FPO 268 kg et LTL 185 kg #error code ..
Log7ded$ENGF_COD[is.na(Log7ded$ENGF_COD) & Log7ded$NAVS_COD == "645006"] <- "MIS" #weird catch!

# Check for NK coding
Log7ded %>%
  filter(ENGF_COD=="NK" & quartiers=="DZ") %>%
  group_by(NAVS_COD, quartiers) %>%
  summarize(sum=sum(CAPT_QTOT, na.rm=T))

# Bolinch / XDZ
Log7ded %>%
  filter(ENGF_COD%in%bolinch&!LIEU_COD_RET%in%"XDZ") %>%
  dplyr::select(NAVS_COD, ENGIN_COD, SECT_COD, LIEU_COD_RET, Year)
# rename all existing bolinch same way
Log7ded$ENGF_COD[Log7ded$ENGF_COD%in%bolinch] <- "PS"
# 
table(Log7ded$ENGF_COD, Log7ded$ENGIN_COD)
Log7ded %>%
  filter(NAVS_COD%in%365109)

Log7ded$ENGF_COD[!is.na(Log7ded$ENGIN_COD) & is.na(Log7ded$ENGF_COD)]

#--------------------------------------------------------------------.
### Verify
gear_na <- Channel_PIL %>% filter_at(vars(ENGF_COD), any_vars(is.na(.))) #Few kilos

round(tapply(gear_na$land_sac_kg, 
             list(gear_na$NAVS_COD,
                  gear_na$Gear), sum, na.rm=TRUE),0) #reallocate in MIS_MIS

unique(gear_na$NAVS_COD)
#few kilos that will be deleted after checking for the targeting catch of sardines
### Rename in MIS_MIS
Channel_PIL$ENGIN_COD_SACROIS[is.na(Channel_PIL$ENGIN_COD_SACROIS)] <- "MIS"

Gear_Qtsac <- ddply(Channel_PIL, c("ENGIN_COD_SACROIS", "zone"), summarise,
                    N_gear    = length(ENGIN_COD_SACROIS), 
                    sum_land = sum(land_sac_kg, na.rm=T),
                    percent_rank = sum_land/218991559*100) #7de - 87658179

write.csv(Log7ded, "~/Data_extraction/Marees7de_douarn_2000-2020.csv")

#### ~~ Prod Log -------
quant_port_mar <- Log7ded %>%
  group_by(LIEU_COD_RET) %>%
  summarise(Tcatch = sum(CAPT_QTOT, na.rm=T))%>%
  mutate(Tcatch_tons = Tcatch/1000) %>%
  arrange(desc(Tcatch_tons))

land_lim <- quant_port_mar %>% 
  dplyr::filter(Tcatch_tons > 60) %>%
  arrange(desc(Tcatch_tons))

land_lim <- dplyr::slice(quant_port_mar, 1:10)

ggplot(land_lim,aes(x=LIEU_COD_RET,y=Tcatch_tons)) +
  geom_bar(stat="identity") + 
  xlab("Landing port") + ylab("Landings (tons)") +
  theme_bw() +
  theme(axis.title.x = element_text(size=16), axis.title.y = element_text(size=16),
        axis.text.x = element_text(face="bold", color="#1a0730", 
                                   size=14, angle=0),
        axis.text.y = element_text(face="bold", color="#05000a", 
                                   size=14, angle=45))
round(tapply(Log7ded$CAPT_QTOT, 
             Log7ded$Year, sum, na.rm=TRUE),0)
table(Log7ded$Month, Log7ded$Year)



# Log_sih <- Log_sih %>%
#   filter(!LIEU_COD_RET %in% c("XDZ", "XXX"))


