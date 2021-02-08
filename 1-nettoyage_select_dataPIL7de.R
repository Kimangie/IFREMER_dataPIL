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
qplot(port_ret, sum_qt, data=t,
      geom = "bar", weight= sum_qt,
      ylab = "landings (kg)",
      xlab = "landing port",
      ylim = c(0, 60e+06))

ggplot(t,aes(x=port_ret,y=sum_qt))+
  geom_bar(stat="identity") + ylim(0, 60e+06) + 
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
### -- Check Douarnenez [2000-2020] ## --------------------------------------------
Douarn <- c("25E4", "25E5")
# /!\ check port XCC/XDZ - 29E5 --> 25E5
NewFrame %>%
  dplyr::filter(SECT_COD_SACROIS_NIV5 =="29E5" & LIEU_COD_RET_SACROIS =="XCC") %>%
  dplyr::select(NAVS_COD, ENGIN_COD_SACROIS, QUANT_POIDS_VIF_SACROIS) #PS AND same vessel!

## Correct some error
NewFrame$SECT_COD_SACROIS_NIV5[NewFrame$LIEU_COD_RET_SACROIS =="XCC" | NewFrame$LIEU_COD_RET_SACROIS =="XDZ" & NewFrame$SECT_COD_SACROIS_NIV5 =="29E5"] <- "25E5"
#WOR RAOG III 
NewFrame$SECT_COD_SACROIS_NIV5[NewFrame$NAVS_COD =="898415" & NewFrame$ENGIN_COD_SACROIS %in% bolinch & !NewFrame$SECT_COD_SACROIS_NIV5 %in% Douarn & !is.na(NewFrame$QUANT_POIDS_VIF_SACROIS)] <- "25E5"
#LA SARDANE - ~duplicates? all in 28E6 - correct just for XDZ..
NewFrame$SECT_COD_SACROIS_NIV5[NewFrame$NAVS_COD =="365109" & NewFrame$ENGIN_COD_SACROIS %in% bolinch & !NewFrame$SECT_COD_SACROIS_NIV5 %in% Douarn & !is.na(NewFrame$QUANT_POIDS_VIF_SACROIS) & NewFrame$LIEU_COD_RET_SACROIS == "XDZ"] <- "25E5"


douarn <- filter(NewFrame, SECT_COD_SACROIS_NIV5 %in% Douarn) #36,988 
glimpse(douarn)

douarn <- dplyr::select(douarn, CFR_COD:MAREE_DATE_RET, TPS_MER, LIEU_COD_DEP_SACROIS:METIER_COD_SACROIS, METIER_DCF_5_COD:ENGIN_COD, SECT_COD_SACROIS_NIV5, CAPT_ID,STOCK_ORGP, ORIGINE_QUANT_POIDS_VIF:Time)

summary(douarn)
 # ++ in 25E5 
sum(douarn$QUANT_POIDS_VIF_SACROIS, na.rm=T)
 # 131,986 tons
 # 95 Navires - NA 
##Catch by gear in respective rects
round(tapply(douarn$QUANT_POIDS_VIF_SACROIS, 
             list(douarn$ENGIN_COD_SACROIS, 
                  douarn$SECT_COD_SACROIS_NIV5),sum, na.rm=TRUE),0)
#
round(tapply(douarn$QUANT_POIDS_VIF_SACROIS, 
             list(douarn$LIEU_COD_RET_SACROIS, 
                  douarn$SECT_COD_SACROIS_NIV5),sum, na.rm=TRUE),0)

unique(douarn$LIEU_COD_RET_SACROIS) #34
unique(douarn$NAVS_COD) #95 - /!\ NA
douarn <- filter(douarn, !is.na(NAVS_COD)) #36,986

# New variable for regroup port/lieu


## ~~  Production #------
land_lieu <- ddply(douarn,c("LIEU_COD_RET_SACROIS", "SECT_COD_SACROIS_NIV5"), summarise,
                   N_port = length(LIEU_COD_RET_SACROIS), 
                   sum_qt = sum(QUANT_POIDS_VIF_SACROIS, na.rm=T))
land_lim <- land_lieu %>% 
              dplyr::filter(sum_qt > 50000)
ggplot(land_lim,aes(x=LIEU_COD_RET_SACROIS,y=sum_qt, fill=factor(SECT_COD_SACROIS_NIV5)))+
  geom_bar(stat="identity")

#
douarn_ym <- ddply(douarn, c("Year", "Month"), summarise,
                Nb_records = length(QUANT_POIDS_VIF_SACROIS),
                tot  = sum(QUANT_POIDS_VIF_SACROIS, na.rm=T))
#write.csv(douarn_ym, "~/Data_extraction/prod_douarn.csv")

#### VENTES ##---------------------------------------------------------------------------------
#~~ 7.de (-25E4/25E5) ####
ventes <- read.csv("~/Data_extraction/ISIH-503343-donneesVente-20210118160841.txt", sep=";")
#
ventes.b <- read.csv("~/Data_extraction/ISIH-503343-donneesVente-20210203152138.txt", sep=";")

ref_harb <- read_excel("~/Data_extraction/REF_PORT.xls")
ventes_year <- merge(ventes, ref_harb, by.x = "LIEU_COD",by.y="Code_lieu", all.x=T)


lieu_vente <- c(unique(ventes_year$LIEU_COD)) #77
car <- c(unique(NewF.7de$port_ret)) #56 

unique(lieu_vente[!lieu_vente %in% car]) #40 de ventes pas dans sac! 
table(ventes_year$LIEU_COD)


summary(ventes_year$SEQV_QUANT)# keep neg values
which(is.na(ventes_year$SEQV_QUANT))#0

Navs_sac <- c(unique(NewF.7de$NAVS_COD)) #575

car <- c(unique(NewF.7de$port_ret)) #56

ventes_year <- ventes_year %>% 
                dplyr::filter(NAVS_COD %in% Navs_sac | LIEU_COD %in% car) #96,995!!
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
ventes.dat <- merge(ventes.date, HH1, c("ID")) #merge HH1 in HH
names(ventes.dat)
anyDuplicated(ventes.dat) #0
#--------------------------------------------------------------.
### /!\ doublons de certaines lignes - repeat "Port"xxx or "Criée"xxx
table(ventes.dat$Lieu_libelle)
#create empty vector of strings
lib <- character(0)
#for each Libelle, exctract 
for (lieu in ventes.dat$Lieu_libelle){
  code <- strsplit(lieu, " ")[[1]][1]
  # update empty list with the first component of (Lieu_libelle)
  lib <- c(lib, code)
}
#Finally, append to ventes
ventes.dat$Libelle <- lib
head(ventes.dat)
table(ventes.dat$Libelle, ventes.dat$Lieu_libelle) #
#
table(ventes.dat$LIEU_COD, ventes.dat$Libelle)
# Total catch per cat check - Port to keep
ventes_year <- ventes.dat %>%
  dplyr::filter(Libelle == "Port") #48,411
# check total catch per year
round(tapply(ventes_year$SEQV_QUANT, list(ventes_year$Year),sum, na.rm=TRUE),0)

ventes_ym <- ddply(ventes_year, c("Year", "Month"), summarise,
                   N_port    = length(LIEU_COD), 
                   land_kg = sum(SEQV_QUANT))
#-------------------------------------------------------------------.
##### Comp sacrois/ventes - spot the diff #--------------------------------------------------------------------
round(tapply(ventes_year$SEQV_QUANT, 
             list(ventes_year$LIEU_COD, 
                  ventes_year$Year),sum, na.rm=TRUE),0)
#
Prod_in_harbour <- ddply(ventes_year, c("LIEU_COD"), summarise,
                         N_port    = length(LIEU_COD), 
                         land_kg = sum(SEQV_QUANT))    #51 ports 
#combine with previous summarise quant in port return in SACROIS
comp_sac_ventes <- merge(quant_port_ret, Prod_in_harbour, by.x = "port_ret", by.y="LIEU_COD", all.x=T)
#
comp_sac_ventes <- comp_sac_ventes %>% dplyr::filter(sum_qt > 50)




 #more NAVS in Sac7de than in ventes..
nav_diff <- unique(Navs_sac[!Navs_sac %in% ventes_year$NAVS_COD]) #51 in sac, not in ventes

NewF.7de %>%
  dplyr::filter(NAVS_COD %in% nav_diff) %>%
  summarise(total_catch = sum(land_sac_kg, na.rm=T)) #40.5 t

resume_ventes7de <- ventes_year %>%
  dplyr::group_by(NAVS_COD, Year) %>%
  summarise(Tcatch = sum(SEQV_QUANT))

NewF.7de %>%
  dplyr::filter(NAVS_COD == c("726643","716582","716999") & Year == c("2004", "2005", "2009", "2010")) %>%
  dplyr::select(NAVS_COD, port_ret, rects, land_sac_kg, Year, Month)


ventes_year %>%
  dplyr::filter(NAVS_COD == c("726643") & Year == c("2005")) %>%
  dplyr::select(NAVS_COD, LIEU_COD, SEQV_QUANT, SEQV_MONTANT, Year, Month)
NewF.7de %>%
  dplyr::filter(NAVS_COD == c("726643") & Year == c("2005")) %>%
  dplyr::select(NAVS_COD, port_ret, rects, land_sac_kg, Year, Month)

#### LOGBOOK  ##---------------------------------------------------------------------------------
#~~ 7.de (-25E4/25E5) ####
Logbook <- read.csv("~/Data_extraction/ISIH-503343-donneesMaree-20210106110539.txt", sep=";")
Logbook2 <- read.csv("~/Data_extraction/ISIH-503343-donneesMaree-20210205125504.txt", sep=";")

glimpse(Logbook)
table(Logbook$SECT_COD, Logbook$LIEU_COD_RET)
table(Logbook$ENGF_COD)

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

Log7de <- Logbook_dt[SECT_COD %in% Area7 | LIEU_COD_RET %in% car] #9,229


Log7de <- Log7de %>%
  dplyr::select(MAREE_ID, NAVS_COD, TLIEU_COD_RET, LIEU_COD_RET, LIEU_COD_DEP, MAREE_DAT_DEP,MAREE_DAT_RET, MAREE_ORI_ID, MAREE_DUREE, PECHE_ID, ENGF_COD, SEQP_ID, SECT_COD, CAPT_ID:CAPT_QTOT)
head(Log7de)

round(tapply(Log7de$CAPT_QTOT, 
             list(Log7de$SECT_COD, 
                  Log7de$LIEU_COD_RET),sum, na.rm=TRUE),0)

#rm XDZ
Log7de <- Log7de %>%
          filter(!LIEU_COD_RET %in% "XDZ")

quant_port_mar <- ddply(Log7de,c("LIEU_COD_RET"), summarise,
                        N_port = length(LIEU_COD_RET),
                        sum = sum(CAPT_QTOT, na.rm=T))

### Separate the date (with Maree_dat_dep) #------
str(Log7de$MAREE_DAT_DEP)
tmp <- matrix(unlist(strsplit(as.character(Log7de$MAREE_DAT_DEP), '/')), ncol=3, byrow=TRUE) #separe the date 

HH1 <- Log7de$ID <-seq.int(nrow(Log7de))
HH1 <- cbind(Log7de$ID, as.data.frame(tmp))

names(HH1) <- c("ID", "Day", "Month", "Yhour") #renaming the columns
str(HH1)
maree.date <- merge(Log7de, HH1, c("ID")) #merge HH1 in HH
names(maree.date)
anyDuplicated(maree.date) #0 -but separate time also
tmp <- matrix(unlist(strsplit(as.character(maree.date$Yhour), ' ')), ncol=2,
              byrow=TRUE) 
HH1 <- cbind(maree.date$ID, as.data.frame(tmp))
names(HH1) <- c("ID", "Year", "Time")
maree.dat <- merge(maree.date, HH1, c("ID")) #
names(maree.dat)
anyDuplicated(maree.dat) #0

Log7de <- maree.dat

round(tapply(Log7de$CAPT_QTOT, 
             Log7de$Year,sum, na.rm=TRUE),0)


Log7de %>%
  filter(NAVS_COD == "726643" & Year == c(2005, 2009))
