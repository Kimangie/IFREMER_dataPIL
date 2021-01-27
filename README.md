# IFREMER - Station de Lorient

# Angela Larivain - Jan2021 
#Chargement des données, nettoyage step-by-step des fichiers de données brutes ~SACROIS_Ventes_Marée
#set the directory source
setwd("~/Data_extraction/SACROIS_PIL_2000-2020")
# Création du script nettoyage_select.R dans ce projet
  #### ETAPES ####
## Compilation 2000-2020 with the Script_compilation_from_SACROIS.R
source(Script_compilation_from_SACROIS.R)
## Vérif the split the date for the period [2000-2020] in cleaning_bddPIL.R
## Filter area 7de with/without 25E5/25E4 for Douarn Bay
  # Channel with Douarnenez Bay
Area<-c("25E4", "25E5", "26E6","26E7","26E8","27E5", "27E6", "27E7", "27E9", "28E3", "28E4", "28E5", "28E6", "28E7", "28E8", "28E9", "28F0", "29E5", "29E7", "29E8", "29E9", "29F0", "29F1", "30E9", "30F0")
  # Channel without 25E4 25E5
Area7<-c("26E6","26E7","26E8","27E5", "27E6", "27E7", "27E9", "28E3", "28E4", "28E5", "28E6", "28E7", "28E8", "28E9", "28F0", "29E5", "29E7", "29E8", "29E9", "29F0", "29F1", "30E9", "30F0")
## Check then the LIEU_COD_RET_SACROIS as some error LIEU_COD (with BZH sud or miswriting)
## /!\ PS/PS1 still in the BDD whereas Douarn removed

## Gear 'ENGIN_COD' / 'METIERS_COD' to check and replace
## Sum QUANT_POIDS_VIF by harbor/Gear/Metiers/Month and Year




