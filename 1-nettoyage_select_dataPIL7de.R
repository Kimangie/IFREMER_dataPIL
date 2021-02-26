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
library(lubridate)
#--------------------------------------------.
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
########### SACROIS (FRA/PIL) 2000-2020 #------------------------------------------
  
#load ~ each raw file for unique date was combined in 2000-2020 period and the date split in D/M/Y - see cleaning_bddPIL.R
NewFrame <- read.csv("FRAchannel_sardine_2000_2020_ready.csv") #99,265 obs. 70 var (sup. X) - date already separate 
#NewFrame <- read.csv("FRA_Channel_PIL_2000_2020_sacrois.csv", header=T, na.strings = "") # Separate the date!
NewFrame <- dplyr::select(NewFrame, -X)

#### separate the date, lubridate #------------------------------------------------------------
str(NewFrame$DATE_SEQ)
# Convert time to a time format with lubridate
# ymd_hms()
# hms()
# ymd()
NewFrame$DATE_SEQ <- dmy_hms(NewFrame$DATE_SEQ)
# Extract a component & create quarter:
NewFrame$Year <- year(NewFrame$DATE_SEQ)
NewFrame$Month <- month(NewFrame$DATE_SEQ)
NewFrame$quarter <- quarter(NewFrame$DATE_SEQ)
names(NewFrame)

#####~~ Correct errors before subset#------------------------------------------------
# All *DZ in XDZ!
table(NewFrame$LIEU_COD_RET_SACROIS)
unique(subset(NewFrame, substring(LIEU_COD_RET_SACROIS,2,3)=="DZ")$LIEU_COD_RET_SACROIS)
port_d <- c("XDZ", "DDZ", "CDZ", "BDZ")
NewFrame$LIEU_COD_RET_SACROIS[NewFrame$LIEU_COD_RET_SACROIS %in% port_d] <- "XDZ"

#/!\ check port XCC/XDZ - 29E5 --> 25E5
NewFrame %>%
  dplyr::filter(SECT_COD_SACROIS_NIV5 =="29E5" & LIEU_COD_RET_SACROIS =="XCC") %>%
  dplyr::select(NAVS_COD, ENGIN_COD_SACROIS, QUANT_POIDS_VIF_SACROIS) #PS AND same vessel!

##
NewFrame$SECT_COD_SACROIS_NIV5[NewFrame$LIEU_COD_RET_SACROIS =="XCC" | NewFrame$LIEU_COD_RET_SACROIS =="XDZ" & NewFrame$SECT_COD_SACROIS_NIV5 =="29E5"] <- "25E5"

NewFrame_dt <- as.data.table(NewFrame)
#WAR ROAG III
NewFrame_dt[NAVS_COD %in% 898415 & LIEU_COD_RET_SACROIS %in% "XDZ", c("SECT_COD_SACROIS_NIV5") := list("25E5")]
NewFrame_dt[NAVS_COD %in% 898415 & LIEU_COD_RET_SACROIS %in% c("XLO","XCC"), c("SECT_COD_SACROIS_NIV5") := list("24E6")]
NewFrame_dt[NAVS_COD %in% 898415 & LIEU_COD_RET_SACROIS %in% "AGV", c("SECT_COD_SACROIS_NIV5") := list("24E5")]

#LA SARDANE - ~duplicates? all in 28E6 - correct just for XDZ..
NewFrame_dt$SECT_COD_SACROIS_NIV5[NewFrame_dt$NAVS_COD =="365109" & NewFrame_dt$ENGIN_COD_SACROIS %in% bolinch & !NewFrame$SECT_COD_SACROIS_NIV5 %in% Douarn & !is.na(NewFrame$QUANT_POIDS_VIF_SACROIS) & NewFrame$LIEU_COD_RET_SACROIS == "XDZ"] <- "25E5"

# select interest variables
names(NewFrame_dt)
NewFrame_dt <- dplyr::select(NewFrame_dt, ID, CFR_COD:MAREE_DATE_RET, TPS_MER, LIEU_COD_DEP_SACROIS:METIER_COD_SACROIS, METIER_DCF_5_COD:ENGIN_COD, SECT_COD_SACROIS_NIV3,SECT_COD_SACROIS_NIV5, TP_NAVIRE_SACROIS, TP_NAVIRE_MOYENNE, CAPT_ID,STOCK_ORGP, ORIGINE_QUANT_POIDS_VIF:quarter) #38 var

#rename variables
head(NewFrame_dt)
NewFrame_dt <- as_tibble(NewFrame_dt)
NewFrame_dt <- dplyr::rename(NewFrame_dt, "port_ret" = LIEU_COD_RET_SACROIS,
                   "Gear" = ENGIN_COD,
                   "zone" = SECT_COD_SACROIS_NIV3,
                   "rects" = SECT_COD_SACROIS_NIV5,
                   "land_sac_kg" = QUANT_POIDS_VIF_SACROIS)

round(tapply(NewFrame_dt$land_sac_kg, 
             list(NewFrame_dt$rects, 
                  NewFrame_dt$port_ret),sum, na.rm=TRUE),0) 

#CRU - mistake with ALS 
NewFrame_dt %>%
  filter(port_ret %in% "CRU")
#
NewFrame_dt$port_ret[NewFrame_dt$port_ret %in% "CRU"] <- "ALS"
#
NewFrame_dt %>%
  filter(port_ret %in% "XXX") %>%
  dplyr::select(NAVS_COD, rects, land_sac_kg) # 31 kg.. WHERE? 
nav_XXX <- NewFrame_dt %>%
  filter(NAVS_COD %in% 642089)
dplyr::count(nav_XXX, port_ret, rects) #in XSM probably

NewFrame_dt$port_ret[NewFrame_dt$port_ret %in% "XXX" & NewFrame_dt$NAVS_COD%in%642089] <- "XSM"

NewFrame <- as_tibble(NewFrame_dt)
rm(NewFrame_dt)

table(NewFrame$rects)

Channel_PIL <- filter(NewFrame, rects %in% Area) #7de+Douarn 
table(Channel_PIL$rects)

###~~ Check port (port_ret) --------------------
(quant_port_ret <- ddply(Channel_PIL,c("port_ret"), summarise,
                        N_port = length(port_ret), 
                        sum = sum(land_sac_kg, na.rm=T))) #79 (after correct)

Channel_PIL$quartiers <- substr(Channel_PIL$port_ret, start=2, stop=3)

round(tapply(Channel_PIL$land_sac_kg, 
             list(Channel_PIL$rects, 
                  Channel_PIL$quartiers),sum, na.rm=TRUE),0) #45T XDZ!! to remove if wanted without douarn 


table(Channel_PIL$port_ret)
unique(Channel_PIL$port_ret) #91
unique(Channel_PIL$quartiers) #31

## separate all in Manche (code for maritime districts) / Others
##~ 13 Quartiers maritimes + NLSCE/IJN/NLVLI/SHN??
# car <-c(unique(subset(Channel_PIL, substring(port_ret,2,3)=="BL")$port_ret), unique(subset(Channel_PIL, substring(port_ret,2,3)=="BR")$port_ret), unique(subset(Channel_PIL, substring(port_ret,2,3)=="CH")$port_ret), unique(subset(Channel_PIL, substring(port_ret,2,3)=="CN")$port_ret), unique(subset(Channel_PIL, substring(port_ret,2,3)=="DK")$port_ret), unique(subset(Channel_PIL, substring(port_ret,2,3)=="DP")$port_ret), unique(subset(Channel_PIL, substring(port_ret,2,3)=="DZ")$port_ret), unique(subset(Channel_PIL, substring(port_ret,2,3)=="FC")$port_ret), unique(subset(Channel_PIL, substring(port_ret,2,3)=="LH")$port_ret), unique(subset(Channel_PIL, substring(port_ret,2,3)=="MX")$port_ret), unique(subset(Channel_PIL, substring(port_ret,2,3)=="PL")$port_ret), unique(subset(Channel_PIL, substring(port_ret,2,3)=="SB")$port_ret),unique(subset(Channel_PIL, substring(port_ret,2,3)=="SM")$port_ret), "NLSCE", "IJN", "NLVLI", "SHN") 
#
# Channel_PIL_port <- filter(Channel_PIL, port_ret %in% car)  #port Façade Manche + XDZ
#------------.
# NewF7_portOTH <- filter(Channel_PIL, !port_ret %in% car) # autres ports
#----------------------#quicker below 

# Extract the 13 quartiers 
	# version Atelier-R
Channel_PIL_port <- filter(Channel_PIL, quartiers%in%c("BR", "BL", "CH", "CN", "DK", "DP", "DZ", "FC", "LH", "MX", "PL", "SB", "SM") | port_ret%in% c("NLSCE", "IJN", "NLVLI", "SHN"))
head(Channel_PIL_port)
dim(Channel_PIL_port)

(stats <- Channel_PIL_port %>% 
    group_by(port_ret, Year, quartiers) %>% 
    dplyr::summarize(prd.kg=sum(land_sac_kg, na.rm = T)) %>% 
    group_by(Year) %>%
    arrange(desc(prd.kg)) %>% 
    slice(1:5))
# Check that with have the first five harbour per year
dplyr::count(stats, Year) # o

# Plots the time series per harbour
ggplot(stats, aes(x=Year, y=production.kg)) + geom_smooth(color="gray") + geom_jitter() + facet_wrap(~quartiersfr, scales="free_y")
#
round(tapply(Channel_PIL_port$land_sac_kg, 
             list(Channel_PIL_port$rects, 
                  Channel_PIL_port$port_ret),sum, na.rm=TRUE),0)

unique(Channel_PIL_port$port_ret) 
#rm PLHEL..
Channel_PIL_port <- Channel_PIL_port %>% 
  dplyr::filter(!port_ret == "PLHEL")
#
unique(Channel_PIL_port$port_ret) #62 ports
#
sum(Channel_PIL_port$land_sac_kg, na.rm=T)
(quant_port_ret <- ddply(Channel_PIL_port,c("port_ret"), summarise,
                        N_port = length(port_ret), 
                        sum_qt = sum(land_sac_kg, na.rm=T),
                        perc_rank = sum_qt/202976148*100)) #

quant_port_ret <- quant_port_ret %>%
  arrange(desc(perc_rank))


#Top '12' to plot - < 0.1%
t_land <- dplyr::slice(quant_port_ret, 1:12)

## PLOT Qtité
ggplot(t_land,aes(x=port_ret,y=sum_qt))+
  geom_bar(stat="identity")


table(Channel_PIL_port$Gear)
### Check Qtité by gear
Gear_Qtsac <- ddply(Channel_PIL_port, c("ENGIN_COD_SACROIS"), summarise,
                    N_gear    = length(ENGIN_COD_SACROIS), 
                    sum_land = sum(land_sac_kg, na.rm=T))
#201 NA - ?
ggplot(Gear_Qtsac,aes(x=ENGIN_COD_SACROIS,y=sum_land))+
  geom_bar(stat="identity") #24 codes engins

round(tapply(Channel_PIL_port$land_sac_kg, 
             Channel_PIL_port$ENGIN_COD_SACROIS,sum, na.rm=TRUE),0)

table(Channel_PIL_port$ENGIN_COD_SACROIS, Channel_PIL_port$Gear)
#

Channel_PIL_port %>% dplyr::select(ENGIN_COD_SACROIS, Gear, METIER_COD_SACROIS, METIER_DCF_6_COD)
#
round(tapply(Channel_PIL_port$land_sac_kg, 
             list(Channel_PIL_port$NAVS_COD,
                  Channel_PIL_port$Gear), sum, na.rm=TRUE),0)

####~~ Correct ENGIN_COD_SACROIS / Metiers NA  --------------------------------
gear_na <- Channel_PIL_port %>% filter_at(vars(ENGIN_COD_SACROIS), any_vars(is.na(.)))

round(tapply(gear_na$land_sac_kg, 
             list(gear_na$NAVS_COD,
                  gear_na$Gear), sum, na.rm=TRUE),0)
# GES -> OTM
Channel_PIL_port$ENGIN_COD_SACROIS[is.na(Channel_PIL_port$ENGIN_COD_SACROIS) & Channel_PIL_port$Gear =="GES" & Channel_PIL_port$NAVS_COD == "716582" | Channel_PIL_port$NAVS_COD == "651429" | Channel_PIL_port$NAVS_COD == "925601"] <- "OTM"
#bolinch
Channel_PIL_port$ENGIN_COD_SACROIS[is.na(Channel_PIL_port$ENGIN_COD_SACROIS) & Channel_PIL_port$Gear %in% bolinch] <- "PS"
#

Channel_PIL_port$ENGIN_COD_SACROIS[is.na(Channel_PIL_port$ENGIN_COD_SACROIS) & Channel_PIL_port$Gear =="GN" & Channel_PIL_port$NAVS_COD == "738594"] <- "OTB" #8 T 
## 735386 - OTB?
Channel_PIL_port$ENGIN_COD_SACROIS[is.na(Channel_PIL_port$ENGIN_COD_SACROIS) & Channel_PIL_port$Gear =="OTB" | Channel_PIL_port$Gear =="TB" & Channel_PIL_port$NAVS_COD == "735386"] <- "OTB" #possible OTM
Channel_PIL_port$ENGIN_COD_SACROIS[is.na(Channel_PIL_port$ENGIN_COD_SACROIS) & Channel_PIL_port$Gear =="TBB" & Channel_PIL_port$NAVS_COD == "735386"] <- "TBB"
#
Channel_PIL_port$ENGIN_COD_SACROIS[is.na(Channel_PIL_port$ENGIN_COD_SACROIS) & Channel_PIL_port$Gear =="OTB" & Channel_PIL_port$NAVS_COD == "734964"] <- "OTM"

Channel_PIL_port$ENGIN_COD_SACROIS[is.na(Channel_PIL_port$ENGIN_COD_SACROIS) & Channel_PIL_port$Gear =="PTB" | Channel_PIL_port$Gear == "SPR" & Channel_PIL_port$NAVS_COD == "644260"] <- "OTB"
# 735383-PTM +
Channel_PIL_port$ENGIN_COD_SACROIS[is.na(Channel_PIL_port$ENGIN_COD_SACROIS) & Channel_PIL_port$Gear =="PTB" | Channel_PIL_port$Gear == "SPR" & Channel_PIL_port$NAVS_COD == "735383"] <- "PTM"
### Saving some kilos
## TBB - OTB 
Channel_PIL_port$ENGIN_COD_SACROIS[is.na(Channel_PIL_port$ENGIN_COD_SACROIS) & Channel_PIL_port$Gear =="TBB" & Channel_PIL_port$NAVS_COD == "734736"] <- "OTB"
# SPR 
Channel_PIL_port$ENGIN_COD_SACROIS[is.na(Channel_PIL_port$ENGIN_COD_SACROIS) & Channel_PIL_port$Gear =="SPR" & Channel_PIL_port$NAVS_COD == "934685"] <- "PTM" #
#SDV -> SDN 
Channel_PIL_port$ENGIN_COD_SACROIS[is.na(Channel_PIL_port$ENGIN_COD_SACROIS) & Channel_PIL_port$Gear =="SDV"] <- "SDN"
#FPO 268 kg et LTL 185 kg #error code ..
Channel_PIL_port$ENGIN_COD_SACROIS[is.na(Channel_PIL_port$ENGIN_COD_SACROIS) & Channel_PIL_port$Gear =="FPO" & Channel_PIL_port$NAVS_COD == "645006"] <- "MIS" #weird catch!
Channel_PIL_port$ENGIN_COD_SACROIS[is.na(Channel_PIL_port$ENGIN_COD_SACROIS) & Channel_PIL_port$Gear =="LTL" & Channel_PIL_port$NAVS_COD == "925621"] <- "OTB" #?
#correct fileyeur BR
Channel_PIL_port$ENGIN_COD_SACROIS[is.na(Channel_PIL_port$ENGIN_COD_SACROIS) & Channel_PIL_port$Gear =="DRB" & Channel_PIL_port$NAVS_COD == "732240"] <- "GNS"

#--------------------------------------------------------------------.
### Verify
gear_na <- Channel_PIL_port %>% filter_at(vars(ENGIN_COD_SACROIS), any_vars(is.na(.))) #Few kilos

round(tapply(gear_na$land_sac_kg, 
             list(gear_na$NAVS_COD,
                  gear_na$Gear), sum, na.rm=TRUE),0) #reallocate in MIS_MIS

unique(gear_na$NAVS_COD)
 #few kilos that will be deleted after checking for the targeting catch of sardines
### Rename in MIS_MIS
Channel_PIL_port$ENGIN_COD_SACROIS[is.na(Channel_PIL_port$ENGIN_COD_SACROIS)] <- "MIS"

Gear_Qtsac <- ddply(Channel_PIL_port, c("ENGIN_COD_SACROIS", "zone"), summarise,
                    N_gear    = length(ENGIN_COD_SACROIS), 
                    sum_land = sum(land_sac_kg, na.rm=T),
                    percent_rank = sum_land/202976148*100) #7de - 87658179

color_paired <- c("#a6cee3","#1f78b4", "#b2df8a", "#33a02c","#fb9a99","#e31a1c","#fdbf6f","#ff7f00","#cab2d6","#6a3d9a")
pie(Gear_Qtsac$percent_rank, labels = Gear_Qtsac$ENGIN_COD_SACROIS, col=color_paired, main="Pie Chart of Gear types catching Sardines")
Gear_Qtsac <- Gear_Qtsac %>%
  arrange(desc(sum_land))
t <- dplyr::slice(Gear_Qtsac, 1:5)

pie(Gear_Qtsac$percent_rank, labels = paste0(round(Gear_Qtsac$percent_rank),"%"), col=color_paired, main="Pie Chart of Gear types catching Sardines") +
  legend("topleft", legend=Gear_Qtsac$ENGIN_COD_SACROIS, fill=color_paired)

ggplot(t , aes(x=ENGIN_COD_SACROIS, y=sum_land, fill=zone)) + 
  geom_bar(stat ="identity") +
  scale_color_fish_d(option="Coryphaena_hippurus") + 
  theme_bw() +
  xlab("Gear (ENGIN_COD_SACROIS)") + ylab("Landings (kg)") +
  theme(axis.title.x = element_text(size=16), axis.title.y = element_text(size=16),
        axis.text.x = element_text(face="bold", color="#1a0730", 
                                   size=14, angle=0),
        axis.text.y = element_text(face="bold", color="#05000a", 
                                   size=12, angle=0))

### regroup METIERS
# 
# look at cumsum per metier
head(Channel_PIL_port$METIER_DCF_6_COD)
(prod_by_met <- Channel_PIL_port %>% 
  group_by(METIER_DCF_6_COD) %>% 
  dplyr::summarize(prod=sum(land_sac_kg, na.rm=T)) %>% 
  arrange(desc(prod)))

prod_by_met$cumsum <- cumsum(prod_by_met$prod) / sum(prod_by_met$prod)
(prod_by_met[1:20,])

# Organize the levels of the factor by the order in the cumsum
prod_by_met$METIER_DCF_6_COD <- factor(prod_by_met$METIER_DCF_6_COD, levels=prod_by_met$METIER_DCF_6_COD)

# Create a barplot with the threshold as a hline
ggplot(filter(prod_by_met, cumsum<0.99), aes(x=METIER_DCF_6_COD, y=cumsum)) + geom_bar(stat="identity") + theme_light() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + scale_x_discrete("Métier", expand=c(0, 0)) + scale_y_continuous("Cum. production (kg)") + geom_hline(aes(yintercept = 0.99), linetype=2)

# Check per year
(stats <- Channel_PIL_port %>% 
    group_by(Year) %>% 
    dplyr::summarize(production.kg=sum(land_sac_kg, na.rm = T)
                     #, effort=sum(TPS_MER, na.rm=T) #check effort calcul 
                     ))
# plot
ggplot(stats, aes(x=Year, y=production.kg)) + geom_smooth(color="gray40") + geom_point() + scale_y_continuous("Production (kg)") + theme_light()

# Plots the time series per final metiers (top5?)
(stats <- Channel_PIL_port %>% 
    group_by(Year, METIER_DCF_6_COD) %>% 
    dplyr::summarize(prd.kg=sum(land_sac_kg, na.rm = T)) %>% 
    group_by(Year) %>%
    arrange(desc(prd.kg)) %>% 
    slice(1:5))
# Check that with have the first five harbour per year
dplyr::count(stats, Year) # o

ggplot(stats, aes(x=Year, y=prd.kg)) + geom_smooth(color="gray") + geom_jitter() + facet_wrap(~METIER_DCF_6_COD)

# ggplot(stats, aes(x=Year, y=prd.kg)) + geom_bar(stat="identity") + facet_wrap(~METIER_DCF_6_COD, strip.position = "right", dir="v")
stats <- stats %>%
  filter(!str_starts(METIER_DCF_6_COD, "PS")) %>%
  mutate(Tcatch_tons = prd.kg/1000)

ggplot(stats, aes(x=Year, y=Tcatch_tons)) + 
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

### ~~~ Rename / check NAs in metiers --------------------------------------------------------------------------
##-------------------------------------------------------------------------------------------------------------.
round(tapply(Channel_PIL_port$land_sac_kg, 
             list(Channel_PIL_port$Year, 
                  Channel_PIL_port$quarter),sum, na.rm=TRUE),0)

# Select targeting Gear --------------------------------------------------------------------------------------.
# Keep met_6_DCF  in OTM/PTM/OTB & PS to rename & catch > 0.1% to classify 
#others in MIS_MIS

round(tapply(Channel_PIL_port$land_sac_kg, 
             list(Channel_PIL_port$METIER_DCF_6_COD,
                  Channel_PIL_port$ENGIN_COD_SACROIS), sum, na.rm=TRUE),0)

sum(Channel_PIL_port$land_sac_kg, na.rm=T)
Met_6_DCF_Qtsac <- ddply(Channel_PIL_port, c("METIER_DCF_6_COD"), summarise,
                         N_met    = length(METIER_DCF_6_COD), 
                         sum_land = sum(land_sac_kg, na.rm=T),
                         percent_rank = sum_land/202976148*100) #131 code met & 216 in NA to redistrib

Met_6_DCF_Qtsac <- Met_6_DCF_Qtsac %>% arrange(desc(sum_land))

Met_6_DCF_Qtsac_7de <- Met_6_DCF_Qtsac %>%
  filter(!str_starts(METIER_DCF_6_COD, "PS"))

#keep NA & metiers catching >1%
met_na <- Channel_PIL_port %>% filter_at(vars(METIER_DCF_6_COD),any_vars(is.na(.))) #216

#What in NAs --> knowledge!
round(tapply(met_na$land_sac_kg, 
             list(met_na$NAVS_COD,
                  met_na$ENGIN_COD_SACROIS), sum, na.rm=TRUE),0)

###Correct METIER_DCF_6
Channel_PIL_port$METIER_DCF_6_COD[is.na(Channel_PIL_port$METIER_DCF_6_COD) & Channel_PIL_port$ENGIN_COD_SACROIS %in%  c("OTB", "PTM") & Channel_PIL_port$NAVS_COD %in% 735383] <- "PTM_SPF_32_69_0"

Channel_PIL_port$METIER_DCF_6_COD[is.na(Channel_PIL_port$METIER_DCF_6_COD) & Channel_PIL_port$ENGIN_COD_SACROIS =="OTB" & Channel_PIL_port$NAVS_COD %in% 644260] <- "PTM_SPF_32_69_0"

Channel_PIL_port$METIER_DCF_6_COD[is.na(Channel_PIL_port$METIER_DCF_6_COD) & Channel_PIL_port$ENGIN_COD_SACROIS %in% "OTB" & Channel_PIL_port$NAVS_COD %in% 734964] <- "OTM_SPF_32_69_0"
Channel_PIL_port$METIER_DCF_6_COD[is.na(Channel_PIL_port$METIER_DCF_6_COD) & Channel_PIL_port$ENGIN_COD_SACROIS %in% "OTM" & Channel_PIL_port$NAVS_COD %in% 716582] <- "OTM_SPF_32_69_0"

#934685 - PTMMAX.. Dim maillage?? - PTM_SPF_32_69_0
Channel_PIL_port %>%
  filter(NAVS_COD =="934685") %>%
  dplyr::select(port_ret, Gear, ENGIN_COD_SACROIS, METIER_DCF_6_COD, rects, land_sac_kg) #Put in PTM

Channel_PIL_port$METIER_DCF_6_COD[is.na(Channel_PIL_port$METIER_DCF_6_COD) & Channel_PIL_port$ENGIN_COD_SACROIS %in% c("PTM","SPR") & Channel_PIL_port$NAVS_COD %in% 934685] <- "PTM_SPF_32_69_0"

## PS (898415 / 911295)
  ###-------------------- /!\ 933781 error code PS --> MIS_MIS
Channel_PIL_port %>%
  filter(NAVS_COD =="911295" #& is.na(METIER_DCF_6_COD)
         ) %>%
  dplyr::select(port_ret, Gear, ENGIN_COD_SACROIS, METIER_DCF_6_COD, rects, land_sac_kg)

Channel_PIL_port$METIER_DCF_6_COD[is.na(Channel_PIL_port$METIER_DCF_6_COD) & Channel_PIL_port$NAVS_COD %in% 898415] <- "PS_SPF_0_16_0"
Channel_PIL_port$METIER_DCF_6_COD[is.na(Channel_PIL_port$METIER_DCF_6_COD) & Channel_PIL_port$NAVS_COD %in% 911295] <- "PS_SPF_16_31_0"

## /!\ Nav_COD 735386 & 738594  - OTB/drague.. 19 + 8T à virer!? in MIS_MIS
Channel_PIL_port %>%
  filter(NAVS_COD =="738594" & is.na(METIER_DCF_6_COD )) %>%
  dplyr::select(port_ret, Gear, ENGIN_COD_SACROIS, METIER_DCF_6_COD, rects, land_sac_kg)

Channel_PIL_port$METIER_DCF_6_COD[is.na(Channel_PIL_port$METIER_DCF_6_COD) & Channel_PIL_port$NAVS_COD %in% c(735386, 738594)] <- "MIS_MIS_0_0_0"

#recheck
Met_6_DCF_Qtsac <- ddply(Channel_PIL_port, c("METIER_DCF_6_COD"), summarise,
                         N_met    = length(METIER_DCF_6_COD), 
                         sum_land = sum(land_sac_kg, na.rm=T),
                         percent_rank = sum_land/202976148*100) #Still 62 na

Met_6_DCF_Qtsac <- Met_6_DCF_Qtsac %>% arrange(desc(sum_land))
met_na <- Channel_PIL_port %>% filter_at(vars(METIER_DCF_6_COD),any_vars(is.na(.)))
round(tapply(met_na$land_sac_kg, 
             list(met_na$NAVS_COD,
                  met_na$ENGIN_COD_SACROIS), sum, na.rm=TRUE),0)

####----------------------Others in MIS_MIS
#keep OTM/PTM/OTB & PS
met_cod6 <- c(unique(subset(Met_6_DCF_Qtsac, substring(METIER_DCF_6_COD, 2,3) == "TM")$METIER_DCF_6_COD), unique(subset(Met_6_DCF_Qtsac, substring(METIER_DCF_6_COD, 1,3) == "OTB")$METIER_DCF_6_COD), unique(subset(Met_6_DCF_Qtsac, substring(METIER_DCF_6_COD, 1,2) == "PS")$METIER_DCF_6_COD)) #63 
## 

#########################################################################################.
#rename 
Channel_PIL_port$metiers <- rep(NA,nrow(NewF.7de))

#NewF.7de$metiers[!NewF.7de$ENGIN_COD_SACROIS %in% c("OTM", "PTM", "OTB")] <- "MIS_MIS_0_0_0"
#
round(tapply(Channel_PIL_port$land_sac_kg[Channel_PIL_port$ENGIN_COD_SACROIS %in% c("OTM", "PTM", "OTB")], 
             list(Channel_PIL_port$METIER_DCF_6_COD[Channel_PIL_port$ENGIN_COD_SACROIS %in% c("OTM", "PTM", "OTB")],
                  Channel_PIL_port$ENGIN_COD_SACROIS[Channel_PIL_port$ENGIN_COD_SACROIS %in% c("OTM", "PTM", "OTB")]), sum, na.rm=TRUE),0) 

##Refactoring to match ices fleet
Channel_PIL_port$metiers[subset(Channel_PIL_port, substring(METIER_DCF_6_COD, 1,3) == "MIS")$METIER_DCF_6_COD] <- "MIS_MIS_0_0_0"
#remaining NAs
Channel_PIL_port$metiers[is.na(Channel_PIL_port$METIER_DCF_6_COD)] <- "MIS_MIS_0_0_0"
## Keep METIER_DCF_6_COD name in met_cod6 & others in MIS_MIS 
Channel_PIL_port$metiers[Channel_PIL_port$METIER_DCF_6_COD %in% met_cod6] <- Channel_PIL_port$METIER_DCF_6_COD[Channel_PIL_port$METIER_DCF_6_COD %in% met_cod6]
 # check ok
table(Channel_PIL_port$METIER_DCF_6_COD[Channel_PIL_port$METIER_DCF_6_COD %in% met_cod6],
      Channel_PIL_port$metiers[Channel_PIL_port$METIER_DCF_6_COD %in% met_cod6])
# others 
Channel_PIL_port$metiers[!Channel_PIL_port$METIER_DCF_6_COD %in% met_cod6] <- "MIS_MIS_0_0_0"

table(Channel_PIL_port$METIER_DCF_6_COD, Channel_PIL_port$metiers)
Channel_PIL_port %>% filter_at(vars(metiers),any_vars(is.na(.))) #0


round(tapply(Channel_PIL_port$land_sac_kg[Channel_PIL_port$metiers %in% met_cod6], 
             list(Channel_PIL_port$METIER_DCF_6_COD[Channel_PIL_port$metiers %in% met_cod6],
                  Channel_PIL_port$ENGIN_COD_SACROIS[Channel_PIL_port$metiers %in% met_cod6]), sum, na.rm=TRUE),0)

# Gear_Qtsac <- ddply(Channel_PIL_port, c("ENGIN_COD_SACROIS"), summarise,
#                     N_gear    = length(ENGIN_COD_SACROIS), 
#                     sum_land = sum(land_sac_kg, na.rm=T),
#                     percent_rank = sum_land/202976148*100)

Metier_catch <- ddply(Channel_PIL_port, c("metiers"), summarise,
                      N_met    = length(metiers), 
                      sum_land = sum(land_sac_kg, na.rm=T),
                      percent_rank = sum_land/202976148*100) # 184 NA to redistrib

Metier_catch <- Metier_catch %>% arrange(desc(sum_land))
#Plot Gear type Vs metiers type
par(mfrow = c(1,2))
#
pie(Gear_Qtsac$percent_rank, labels = Gear_Qtsac$ENGIN_COD_SACROIS, main="Pie Chart of Gear types catching Sardines")

pie(Metier_catch$percent_rank, labels = Metier_catch$metiers, main="Pie Chart of Fleet (DCF_6_COD)")

##---->>>> Production gear/metiers/port_ret or quartiers in 7.de   <<<<----
NewF.7de <- filter(Channel_PIL_port, rects %in% Area7) #Without Douarn

(stats <- NewF.7de %>% 
   group_by(Year, ENGIN_COD_SACROIS) %>% 
   dplyr::summarize(prd.kg=sum(land_sac_kg, na.rm = T)) %>% 
   # group_by(Year) %>%
   arrange(Year)) #%>% 
   #slice(1:5))
# Check that with have the first five harbour per year
dplyr::count(stats, Year) # o

table(stats$ENGIN_COD_SACROIS,stats$Year)
sum(NewF.7de$land_sac_kg, na.rm=T) #87646159
Gear_Qtsac <- ddply(NewF.7de, c("ENGIN_COD_SACROIS"), summarise,
                                      N_gear    = length(ENGIN_COD_SACROIS), 
                                      sum_land = sum(land_sac_kg, na.rm=T),
                                      percent_rank = sum_land/87646159*100)
(Gear_Qtsac <- Gear_Qtsac %>% arrange(desc(sum_land)))

(stats <- stats %>%
  filter(ENGIN_COD_SACROIS%in%c("OTM","PTM", "OTB")) %>%
  mutate(Tcatch_tons = prd.kg/1000))

stats$ENGIN_COD_SACROIS <- factor(stats$ENGIN_COD_SACROIS, levels=c("OTM", "PTM", "OTB"))

ggplot(stats, aes(x=Year, y=Tcatch_tons)) + 
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
    dplyr::summarize(prd.kg=sum(land_sac_kg, na.rm = T)) %>% 
    group_by(Year) %>%
    arrange(desc(prd.kg)) %>% 
    slice(1:5))
# Check that with have the first five harbour per year
dplyr::count(stats, Year) # o
stats <- stats %>%
  mutate(Tcatch_tons = prd.kg/1000)

stats_5 <- stats %>%
  filter(quartiers%in%c("BL", "DP", "FC", "LV", "LS"))

stats_5$quartiers <- factor(stats_5$quartiers, levels=c("FC", "BL", "DP", "LS", "LV"))
# Plots the time series per harbour
ggplot(stats_5, aes(x=Year, y=Tcatch_tons)) + 
  geom_bar(stat="identity") +
  facet_grid(quartiers~., scales="free") +
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

# write.csv(Channel_PIL_port, "~/Data_extraction/sacrois_PIL7de_2000-2020.csv") #quarter/separate date/gear/metiers corrected ...

Channel_PIL_port <- read.csv("sacrois_PIL7de_2000-2020.csv")
Channel_PIL_port$ID <- seq.int(nrow(Channel_PIL_port))
Channel_PIL_port <- dplyr::select(Channel_PIL_port, -X)

# Subset 7de / Douarn + LIEU_COD return in 7de + the gear sampled and used for sardine ######################################################################################
# keep without the Douarn Bay

NewF.7de <- filter(Channel_PIL_port, rects %in% Area7) # just 7de
unique(NewF.7de$ENGIN_COD_SACROIS) #remove any remaining PS as not supposed to catch in this area

NewF.7de %>%
  filter(ENGIN_COD_SACROIS %in% bolinch) %>%
  dplyr::select(NAVS_COD, rects, ENGIN_COD_SACROIS, metiers, land_sac_kg, Year)

NewF.7de <- filter(NewF.7de, ENGIN_COD_SACROIS != "PS") #62,017

NewF.7de$rects <- as.factor(NewF.7de$rects)
table(NewF.7de$rects)
table(NewF.7de$port_ret)

NewF.7de <- dplyr::select(NewF.7de, -PAVILLON) #cause all in FRA

round(tapply(NewF.7de$land_sac_kg, 
             list(NewF.7de$metiers,
                  NewF.7de$ENGIN_COD_SACROIS), sum, na.rm=TRUE),0)
sum(NewF.7de$land_sac_kg, na.rm=T) # 87,646,159 kg!

####################################################################################.
### --> Douarnenez [2000-2020] ## --------------------------------------------

douarn <- filter(Channel_PIL, rects %in% Douarn) #36,739
glimpse(douarn)

# douarn <- dplyr::select(douarn, CFR_COD:MAREE_DATE_RET, TPS_MER, LIEU_COD_DEP_SACROIS:METIER_COD_SACROIS, METIER_DCF_5_COD:rects, CAPT_ID, STOCK_ORGP, ORIGINE_QUANT_POIDS_VIF:Time)

summary(douarn)
# ++ in 25E5 
sum(douarn$land_sac_kg, na.rm=T)
# 131,275 tons
# 95 Navires - NA 
##Catch by gear in respective rects
round(tapply(douarn$land_sac_kg, 
             list(douarn$ENGIN_COD_SACROIS, 
                  douarn$rects),sum, na.rm=TRUE),0)
#
round(tapply(douarn$land_sac_kg, 
             list(douarn$port_ret, 
                  douarn$rects),sum, na.rm=TRUE),0)

unique(douarn$port_ret) #31
unique(douarn$NAVS_COD) #95 - /!\ NA
#douarn <- filter(douarn, !is.na(NAVS_COD)) #36,983



## ~~  Production douarn #------
land_lieu <- ddply(douarn,c("port_ret", "rects"), summarise,
                   N_port = length(port_ret), 
                   sum_qt = sum(land_sac_kg, na.rm=T),
                   percent_rank = sum_qt/131275177*100)

land_port <- ddply(douarn,c("port_ret"), summarise,
                   N_port = length(port_ret), 
                   sum_qt = sum(land_sac_kg, na.rm=T),
                   percent_rank = sum_qt/131275177*100)

land_lieu <- land_port %>%
  mutate(Tcatch_tons = sum_qt/1000) %>%
  arrange(desc(Tcatch_tons))

land_lim <- land_lieu %>% 
  dplyr::slice(1:5) %>%
  arrange(desc(Tcatch_tons))

ggplot(land_lim,aes(x=port_ret,y=Tcatch_tons), fill=factor(rects))+
  geom_bar(stat="identity") + ylim(0, 120e+03) + 
  xlab("Landing port") + ylab("Landings (tons)") +
  theme_bw() +
  theme(axis.title.x = element_text(size=16), axis.title.y = element_text(size=16),
        axis.text.x = element_text(face="bold", color="#1a0730", 
                                   size=14, angle=0),
        axis.text.y = element_text(face="bold", color="#05000a", 
                                   size=14, angle=45))

#
douarn <- douarn %>%
  mutate(Tcatch_tons = land_sac_kg/1000)

round(tapply(douarn$Tcatch_tons, 
             list(douarn$Year, 
                  douarn$Month),sum, na.rm=TRUE),0)

round(tapply(douarn$Tcatch_tons, 
             list(douarn$METIER_DCF_6_COD),sum, na.rm=TRUE),0)
round(tapply(douarn$Tcatch_tons, 
             list(douarn$ENGIN_COD_SACROIS),sum, na.rm=TRUE),0)

##~~ recodage fleet----
land_gear <- ddply(douarn,c("ENGIN_COD_SACROIS"), summarise,
                   N_port = length(ENGIN_COD_SACROIS), 
                   sum_qt = sum(land_sac_kg, na.rm=T),
                   percent_rank = sum_qt/131275177*100)
(land_gear <- land_gear %>%
  arrange(desc(sum_qt)))

douarn %>%
  filter_at(vars(METIER_DCF_6_COD), any_vars(is.na(.))) %>%
  dplyr::select(NAVS_COD, port_ret, rects, ENGIN_COD_SACROIS, Gear, land_sac_kg, Year)
#
table(douarn$Gear, douarn$ENGIN_COD_SACROIS)
table(douarn$METIER_DCF_6_COD)
#### SX/SDV
douarn$METIER_DCF_6_COD[is.na(douarn$METIER_DCF_6_COD) & douarn$Gear %in% "SDV" & douarn$NAVS_COD %in% 899957] <- "PS_SPF_0_16_0"
douarn$METIER_DCF_6_COD[is.na(douarn$METIER_DCF_6_COD) & douarn$Gear %in% bolinch] <-"PS_SPF_0_16_0"
###DRB - MIS_MIS
douarn$METIER_DCF_6_COD[is.na(douarn$METIER_DCF_6_COD) & douarn$Gear %in%"DRB"] <-"MIS_MIS_0_0_0"

glimpse(douarn)
####regroup
douarn_metier<- douarn%>%#mutate(met6=gsub("","",foCatEu6))
  tidyr::separate(METIER_DCF_6_COD,c("gear","spp","min","max","sel"),sep="_",remove=FALSE)%>%
  mutate(max=ifelse(grepl(">=",min),gsub(">=","",min),max),
         min=ifelse(grepl(">=",min),gsub(">=","",min),min),
         min=as.numeric(min),max=as.numeric(max),
         autoices="MIS_MIS_0_0_0"
  )

table(douarn_metier$gear, douarn_metier$spp)
##PS-SPF
table(douarn_metier$gear[douarn_metier$spp=="SPF"], douarn_metier$autoices[douarn_metier$spp=="SPF"])

douarn_metier<-douarn_metier%>%
  mutate(autoices=ifelse(gear%in%bolinch&spp%in%c("SPF"),
                         "PS_SPF_All_0_0_All",autoices))
douarn_metier<-douarn_metier%>%
  mutate(autoices=ifelse(gear%in%bolinch&spp%in%c("SPF")&max<=16,
                         "PS_SPF_<16_0_0_all",autoices))
douarn_metier<-douarn_metier%>%
  mutate(autoices=ifelse(gear%in%bolinch&spp%in%c("SPF")&16<=min&max<=31,
                         "PS_SPF_16-31_0_0_all",autoices))
douarn_metier<-douarn_metier%>%
  mutate(autoices=ifelse(gear%in%bolinch&spp%in%c("SPF")&32<=min&max<=69,
                         "PS_SPF_32-69_0_0_all",autoices))
douarn_metier<-douarn_metier%>%
  mutate(autoices=ifelse(gear%in%bolinch&spp%in%c("SPF")&max>=70,
                         "PS_SPF_>=70_0_0_all",autoices))

##PS-DEF
douarn_metier<-douarn_metier%>%
  mutate(autoices=ifelse(gear%in%bolinch&spp%in%c("DEF","CEP", "CRU", "MOL"),
                         "PS_DEF_All_0_0_All",autoices))


table(douarn_metier$autoices, douarn_metier$ENGIN_COD_SACROIS)

sum(douarn_metier$land_sac_kg, na.rm=T)
drn_metier_ices <- ddply(douarn_metier,c("autoices"), summarise,
                         catch = sum(land_sac_kg, na.rm=T),
                         percent_rank = catch/131275177*100)

pct <- round(drn_metier_ices$percent_rank)
lbls <- paste(drn_metier_ices$autoices, pct)
lbls <- paste(lbls,"%",sep="")
pie(drn_metier_ices$percent_rank, labels = lbls, main="Pie Chart of fleet catching Sardines in 25E4 25E5")

round(tapply(douarn_metier$Tcatch_tons, 
             list(douarn_metier$autoices),sum, na.rm=TRUE),0)

table(douarn_metier$METIER_DCF_6_COD, douarn_metier$autoices)

# Plots the time series per top5 port_ret/metiers
(stats <- douarn_metier %>% 
    group_by(Year, port_ret) %>% 
    dplyr::summarize(prd.tons=sum(Tcatch_tons, na.rm = T)) %>% 
    group_by(Year) %>%
    arrange(desc(prd.tons)) %>% 
    slice(1:5))
# Check that with have the first five harbour per year
dplyr::count(stats, Year) # o

table(stats$port_ret,stats$Year)
 stats <- stats %>%
  filter(port_ret%in%c("XDZ","XCC","AGV","CGV", "XGV", "XLO"))

ggplot(stats, aes(x=Year, y=prd.tons)) + 
  geom_bar(stat="identity") + 
  facet_grid(port_ret~.) +
  theme_bw() +
  xlab("Year") + ylab("Landings (tons)") +
  theme(axis.title.x = element_text(size=16), axis.title.y = element_text(size=16),
        axis.text.x = element_text(face="bold", color="#1a0730", 
                                   size=14, angle=0),
        axis.text.y = element_text(face="bold", color="#05000a", 
                                   size=12, angle=0))


#write.csv(douarn_metier, "~/Data_extraction/Douarn_PIL_sac.csv")





## ~~ comp with ventes ##------------------------------------
#nav_douarn <- c(unique(douarn$NAVS_COD))
#keep only *DZ port in ventes to see
table(ventes_year$LIEU_COD)

douarn_ventes <- ventes_year %>%
  filter(str_ends(LIEU_COD, "DZ")) #155,044

glimpse(douarn_ventes)
table(douarn_ventes$LIEU_COD)

### separate Criée/Port/Mareyeur
table(douarn_ventes$Lieu_libelle)
douarn_ventes %>%
  filter(Lieu_libelle %in% c("Criée Douarnenez", "Port Douarnenez")) %>%
  dplyr::select(LIEU_COD, NAVS_COD, VENTE_DAT_DEB, SEQV_ID, SEQV_CATL, SEQV_QUANT, SEQV_MONTANT, Lieu_libelle)
#keep Port
douarn_ventes_port <- douarn_ventes  %>% 
  dplyr::filter(str_starts(Lieu_libelle, "Port"))#77,522

rm(douarn_ventes)

head(douarn_ventes_port)
unique(douarn_ventes_port$NAVS_COD) #IRL007??

douarn_ventes_port %>%
  filter(NAVS_COD == "IRL007") %>%
  summarise(sum(Tcatch = sum(SEQV_QUANT)))

round(tapply(douarn_ventes_port$SEQV_QUANT, 
             list(douarn_ventes_port$Year),sum, na.rm=TRUE),0)

## ~~ comp with Logbook ##------------------------------------
Log_sih <-read_excel("~/Data_extraction/ISIH_donneesMaree-2000_2008_CVignot.xlsx")#2000-2008
#Logbook for after 2008
names(Logbook)
names(Log_sih)
# select interest variables
Logbook <- dplyr::select(Logbook, ID, MAREE_ID, NAVS_COD, TLIEU_COD_RET, LIEU_COD_RET, MAREE_DAT_DEP, MAREE_DAT_RET, MAREE_DUREE, MAREE_QUAL_ENG, MAREE_QUAL_SECT, ENGIN_COD, ENGF_COD, SEQP_ID, SECT_COD, SEQP_NB_OPER, SEQP_LAT, SEQP_LNG, CAPT_QSAIS, CAPT_QTOT, Day, Month, Year,Time)

Log_sih <- dplyr::select(Log_sih, ID, MAREE_ID, NAVS_COD, TLIEU_COD_RET, LIEU_COD_RET, MAREE_DAT_DEP, MAREE_DAT_RET, MAREE_DUREE, MAREE_QUAL_ENG, MAREE_QUAL_SECT, ENGIN_COD, ENGF_COD, SEQP_ID, SECT_COD, SEQP_NB_OPER, SEQP_LAT, SEQP_LNG, CAPT_QSAIS, CAPT_QTOT, Day, Month, Year,Time)
#23 var - combine both datasets

Log_sih$MAREE_QUAL_SECT <- as.integer(Log_sih$MAREE_QUAL_SECT)
Logbook$MAREE_QUAL_SECT <- as.integer(Logbook$MAREE_QUAL_SECT)

Log <- bind_rows(Logbook, Log_sih) #
#
table(Log$Year)
Log <- Log %>%
  filter(Year >=2000) #144,782

Log %>%
  dplyr::filter(SECT_COD =="29E5" & LIEU_COD_RET =="XCC") %>%
  dplyr::select(NAVS_COD, ENGIN_COD, CAPT_QTOT)

## Correct some error as in SACROIS
Log$SECT_COD[Log$LIEU_COD_RET =="XCC" | Log$LIEU_COD_RET =="XDZ" & Log$SECT_COD =="29E5"] <- "25E5"
#Correct LIEU_COD_RET / Vessels / gear
Log_dt <- as.data.table(Log)
table(Logbook_dt$SECT_COD)
#WAR ROAG III
Log_dt[NAVS_COD %in% 898415 & SECT_COD %in% c("27","") & LIEU_COD_RET %in% "XDZ", c("SECT_COD") := list("25E5")]
Log_dt[NAVS_COD %in% 898415 & SECT_COD %in% c("27","") & LIEU_COD_RET %in% c("XLO","XCC"), c("SECT_COD") := list("24E6")]
Log_dt[NAVS_COD %in% 898415 & SECT_COD %in% c("27","") & LIEU_COD_RET %in% "AGV", c("SECT_COD") := list("24E5")]
#Le NATIF II
Log_dt[NAVS_COD %in% 719993 & SECT_COD %in% c("27",""), c("SECT_COD") := list("22E7")]

Log_dt %>% filter(SECT_COD %in% "007E00") %>%
  dplyr::select(NAVS_COD, SECT_COD, LIEU_COD_RET, ENGF_COD, Year)
#007E00
Log_dt[SECT_COD %in% "007E00" & LIEU_COD_RET %in% "AGV", c("SECT_COD") := list("24E5")]
Log_dt[SECT_COD %in% "007E00" & LIEU_COD_RET %in% "XDZ", c("SECT_COD") := list("25E5")]

#Put all *DZ in XDZ
Log_dt$LIEU_COD_RET[Log_dt$LIEU_COD_RET %in% c("CDZ", "DDZ")] <- "XDZ"
### still LIEU_COD_RET in *SB ???

douarn_log <- filter(Log_dt, SECT_COD %in% Douarn) #30,298

table(douarn_log$LIEU_COD_RET)

glimpse(douarn_log)

round(tapply(douarn_log$CAPT_QTOT, 
             list(douarn_log$Year),sum, na.rm=TRUE),0)


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
  geom_bar(stat="identity") + ylim(0, 61e+06) + 
  xlab("Landing port") + ylab("Landings (kg)") +
  theme_bw() +
  theme(axis.title.x = element_text(size=16), axis.title.y = element_text(size=16),
        axis.text.x = element_text(face="bold", color="#1a0730", 
                                   size=14, angle=0),
        axis.text.y = element_text(face="bold", color="#05000a", 
                                   size=14, angle=45))
        
#
land_lieu <- ddply(NewF.7de_2010,c("port_ret"), summarise,
                   N_port = length(port_ret), 
                   sum_qt = sum(land_sac_kg, na.rm=T))
land_lieu <- land_lieu %>%
  arrange(desc(sum_qt))
t <- dplyr::slice(land_lieu, 1:5)

ggplot(t,aes(x=port_ret,y=sum_qt))+
  geom_bar(stat="identity") + ylim(0, 60e+06) + 
  xlab("Landing port") + ylab("Landings (kg)") +
  theme_bw() +
  theme(axis.title.x = element_text(size=16), axis.title.y = element_text(size=16),
        axis.text.x = element_text(face="bold", color="#1a0730", 
                                   size=14, angle=0),
        axis.text.y = element_text(face="bold", color="#05000a", 
                                   size=14, angle=45))

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

#### VENTES ##---------------------------------------------------------------------------------
#~~ 7.de (-25E4/25E5) ####
ventes <- read.csv("~/Data_extraction/ISIH-503343-donneesVente-20210118160841.txt", sep=";")
#
#ventes.b <- read.csv("~/Data_extraction/ISIH-503343-donneesVente-20210203152138.txt", sep=";")

ref_harb <- read_excel("~/Data_extraction/REF_PORT.xls")
table(ventes$LIEU_COD, ventes$TLIEU_COD)

ventes_year <- merge(ventes, ref_harb, by.x = "LIEU_COD", by.y="Code_lieu", all.x=T)

lieu_vente <- c(unique(ventes_year$LIEU_COD)) #77
car <- c(unique(NewF.7de$port_ret)) #56 

unique(lieu_vente[!lieu_vente %in% car]) #40 de ventes pas dans sac! 
table(ventes_year$LIEU_COD)


summary(ventes_year$SEQV_QUANT)# keep neg values
which(is.na(ventes_year$SEQV_QUANT))#0

Navs_sac <- c(unique(NewF.7de$NAVS_COD)) #575

car <- c(unique(NewF.7de$port_ret)) #56

##-------------------------------------------------------------.
### Separate the date (with vente dat_deb)
str(ventes_year$VENTE_DAT_DEB)
tmp <- matrix(unlist(strsplit(as.character(ventes_year$VENTE_DAT_DEB), '/')), ncol=3, byrow=TRUE) #separe the date 

HH1 <- ventes_year$ID <-seq.int(nrow(ventes_year))
HH1 <- cbind(ventes_year$ID, as.data.frame(tmp))

names(HH1) <- c("ID", "Day", "Month", "Yhour") #renaming the columns
str(HH1)
ventes.date <- merge(ventes_year, HH1, c("ID")) #merge HH1 in HH
names(ventes.date)
anyDuplicated(ventes.date) #0 -but separate time also
tmp <- matrix(unlist(strsplit(as.character(ventes.date$Yhour), ' ')), ncol=2,
              byrow=TRUE) 
HH1 <- cbind(ventes.date$ID, as.data.frame(tmp))
names(HH1) <- c("ID", "Year", "Time")
ventes_year <- merge(ventes.date, HH1, c("ID")) #merge HH1 in HH
names(ventes_year)
head(ventes_year)
anyDuplicated(ventes_year) #0

#--------------------------------------------------------------.
#### select ventes in 7de according to sacrois vessels and port
ventes_sac <- ventes_year %>% 
  dplyr::filter(NAVS_COD %in% Navs_sac | LIEU_COD %in% car) #96,995!!

#----------------------------------------------------------------------.
### /!\ doublons de certaines lignes - repeat "Port"xxx or "Criée"xxx
table(douarn_ventes$Lieu_libelle, douarn_ventes$TLIEU_COD)
#create empty vector of strings
lib <- character(0)
#for each Libelle, exctract 
for (lieu in douarn_ventes$Lieu_libelle){
  code <- strsplit(lieu, " ")[[1]][1]
  # update empty list with the first component of (Lieu_libelle)
  lib <- c(lib, code)
}

#Finally, append to ventes
douarn_ventes$Libelle <- lib
head(douarn_ventes)
table(douarn_ventes$LIEU_COD, douarn_ventes$Lieu_libelle) #
#
table(douarn_ventes$LIEU_COD, douarn_ventes$Libelle)
table(douarn_ventes$LIEU_COD, douarn_ventes$TLIEU_COD)


# Total catch per cat check - Port to keep
ventes_sac <- ventes.dat %>%
  dplyr::filter(Libelle == "Port") #48,411
# check total catch per year
round(tapply(ventes_year$SEQV_QUANT, list(ventes_year$Year),sum, na.rm=TRUE),0)

ventes_ym <- ddply(ventes_year, c("Year", "Month"), summarise,
                   N_port    = length(LIEU_COD), 
                   land_kg = sum(SEQV_QUANT))
#-------------------------------------------------------------------.
##### Comp sacrois/ventes - spot the diff #--------------------------------------------------------------------
round(tapply(ventes_sac$SEQV_QUANT, 
             list(ventes_sac$LIEU_COD, 
                  ventes_sac$Year),sum, na.rm=TRUE),0)
#
Prod_in_harbour <- ddply(ventes_sac, c("LIEU_COD"), summarise,
                         N_port    = length(LIEU_COD), 
                         land_kg = sum(SEQV_QUANT))    #51 ports 
#combine with previous summarise quant in port return in SACROIS
comp_sac_ventes <- merge(quant_port_ret, Prod_in_harbour, by.x = "port_ret", by.y="LIEU_COD", all.x=T)
#
comp_sac_ventes <- comp_sac_ventes %>% dplyr::filter(sum_qt > 50)

 #more NAVS in Sac7de than in ventes..
nav_diff <- unique(Navs_sac[!Navs_sac %in% ventes_sac$NAVS_COD]) #51 in sac, not in ventes

NewF.7de %>%
  dplyr::filter(NAVS_COD %in% nav_diff) %>%
  summarise(total_catch = sum(land_sac_kg, na.rm=T)) #40.5 t

resume_ventes7de <- ventes_sac %>%
  dplyr::group_by(NAVS_COD, Year) %>%
  summarise(Tcatch = sum(SEQV_QUANT))

NewF.7de %>%
  dplyr::filter(NAVS_COD == c("726643","716582","716999") & Year == c("2004", "2005", "2009", "2010")) %>%
  dplyr::select(NAVS_COD, port_ret, rects, land_sac_kg, Year, Month)


ventes_sac %>%
  dplyr::filter(NAVS_COD == c("726643") & Year == c("2005")) %>%
  dplyr::select(NAVS_COD, LIEU_COD, SEQV_QUANT, SEQV_MONTANT, Year, Month)
NewF.7de %>%
  dplyr::filter(NAVS_COD == c("726643") & Year == c("2005")) %>%
  dplyr::select(NAVS_COD, port_ret, rects, land_sac_kg, Year, Month)

#### LOGBOOK  ##---------------------------------------------------------------------------------
#~~ 7.de (-25E4/25E5) ####
Logbook <- read.csv("~/Data_extraction/ISIH-503343-donneesMaree-2009_2020.txt", sep=";")


glimpse(Logbook)
table(Logbook$SECT_COD, Logbook$LIEU_COD_RET)
table(Logbook$ENGF_COD)

### Separate the date (with Maree_dat_dep) #------
str(Logbook$MAREE_DAT_DEP)
tmp <- matrix(unlist(strsplit(as.character(Logbook$MAREE_DAT_DEP), '/')), ncol=3, byrow=TRUE) #separe the date 

HH1 <- Logbook$ID <-seq.int(nrow(Logbook))
HH1 <- cbind(Logbook$ID, as.data.frame(tmp))

names(HH1) <- c("ID", "Day", "Month", "Yhour") #renaming the columns
str(HH1)
maree.date <- merge(Logbook, HH1, c("ID")) #merge HH1 in HH
names(maree.date)
anyDuplicated(maree.date) #0 -but separate time also
tmp <- matrix(unlist(strsplit(as.character(maree.date$Yhour), ' ')), ncol=2,
              byrow=TRUE) 
HH1 <- cbind(maree.date$ID, as.data.frame(tmp))
names(HH1) <- c("ID", "Year", "Time")
maree.dat <- merge(maree.date, HH1, c("ID")) #
names(maree.dat)
anyDuplicated(maree.dat) #0
Logbook <- maree.dat

#Correct LIEU_COD_RET / Vessels / gear
Logbook_dt <- as.data.table(Logbook)
#WAR ROAG III
Logbook_dt[NAVS_COD %in% 898415 & SECT_COD %in% c("27","") & LIEU_COD_RET %in% "XDZ", c("SECT_COD") := list("25E5")]
Logbook_dt[NAVS_COD %in% 898415 & SECT_COD %in% c("27","") & LIEU_COD_RET %in% c("XLO","XCC"), c("SECT_COD") := list("24E6")]
Logbook_dt[NAVS_COD %in% 898415 & SECT_COD %in% c("27","") & LIEU_COD_RET %in% "AGV", c("SECT_COD") := list("24E5")]
#Le NATIF II
Logbook_dt[NAVS_COD %in% 719993 & SECT_COD %in% c("27",""), c("SECT_COD") := list("22E7")]
#Logbook_dt[LIEU_COD_RET %in%"CRU" := list("ALS")]
#
Logbook_dt %>%
  filter(LIEU_COD_RET %in% "XCC" & NAVS_COD %in% 898415) %>%
  dplyr::select(NAVS_COD, SECT_COD, LIEU_COD_DEP, MAREE_DAT_DEP, ENGF_COD, CAPT_QTOT)

table(Logbook_dt$SECT_COD)

Log7de <- Logbook_dt[SECT_COD %in% Area7 | LIEU_COD_RET %in% car] #10,627


Log7de <- Log7de %>%
  dplyr::select(MAREE_ID, NAVS_COD, TLIEU_COD_RET, LIEU_COD_RET, LIEU_COD_DEP, MAREE_DAT_DEP,MAREE_DAT_RET, MAREE_ORI_ID, MAREE_DUREE, PECHE_ID, ENGF_COD, SEQP_ID, SECT_COD, CAPT_ID:CAPT_QTOT, Month, Year)
head(Log7de)

round(tapply(Log7de$CAPT_QTOT, 
             list(Log7de$SECT_COD, 
                  Log7de$LIEU_COD_RET),sum, na.rm=TRUE),0)

#rm XDZ
Log7de <- Log7de %>%
          filter(!LIEU_COD_RET %in% "XDZ")
Log7de %>%
  filter(LIEU_COD_RET == "CRU")
Log7de$LIEU_COD_RET[Log7de$LIEU_COD_RET =="CRU"] <- "ALS" #as in sacrois 


#### ~~ Prod Log -------
quant_port_mar <- Log7de %>%
  group_by(LIEU_COD_RET) %>%
  summarise(Tcatch = sum(CAPT_QTOT, na.rm=T))%>%
  mutate(Tcatch_tons = Tcatch/1000) %>%
  arrange(desc(Tcatch_tons))

land_lim <- quant_port_mar %>% 
  dplyr::filter(Tcatch_tons > 60) %>%
  arrange(desc(Tcatch_tons))

land_lim <- dplyr::slice(quant_port_mar, 1:5)

ggplot(land_lim,aes(x=LIEU_COD_RET,y=Tcatch)) +
  geom_bar(stat="identity") + ylim(0, 6e+07) + 
  xlab("Landing port") + ylab("Landings (kg)") +
  theme_bw() +
  theme(axis.title.x = element_text(size=16), axis.title.y = element_text(size=16),
        axis.text.x = element_text(face="bold", color="#1a0730", 
                                   size=14, angle=0),
        axis.text.y = element_text(face="bold", color="#05000a", 
                                   size=14, angle=45))
round(tapply(Log7de$CAPT_QTOT, 
             Log7de$Year, sum, na.rm=TRUE),0)
table(Log7de$Month, Log7de$Year)
#### Data before 2009------------------
Log_sih <- read_excel("~/Data_extraction/ISIH_donneesMaree-2000_2008_CVignot.xlsx")

summary(Log_sih)
#same trt as above and rm XDZ/XXX 
Log_sih <- Log_sih %>%
  filter(!LIEU_COD_RET %in% c("XDZ", "XXX"))

#Check year
table(Log_sih$Year) #rm < 2000
Log_sih <- Log_sih %>%
  filter(Year >= 2000)
#Prod as above
round(tapply(Log_sih$CAPT_QTOT, 
             Log_sih$Year, sum, na.rm=TRUE),0)




