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

#sup XDZ
NewF.7de <- NewF.7de %>%
  dplyr::filter(LIEU_COD_RET_SACROIS!="XDZ") # -30 obs.
table(NewF.7de$LIEU_COD_RET_SACROIS)
unique(NewF.7de$LIEU_COD_RET_SACROIS) #78
## PLOT Qtité
ggplot(quant_port_ret,aes(x=LIEU_COD_RET_SACROIS,y=sum))+
  geom_bar(stat="identity")
#### Correct some gear code in sacrois

sac[ENGIN_COD %in% bolinche, ENGIN_COD := "PS1"]
sac[ENGIN_COD %in% "PT" & ESP_COD_FAO %in% c("PIL", "ANE", "MAC", "ALB","HOM"), ENGIN_COD := "PTB"