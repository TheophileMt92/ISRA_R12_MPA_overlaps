##---------------------------------------------------------------------------------------------------------------------------------##
##----------------------          Dataframes of marine protected area expansion and extent             ----------------------------##
##---------------------------------------------------------------------------------------------------------------------------------##
##---------- Théophile L. Mouton 16/01/2023
##---------- IUCN SSC SSG 

library(sf)
library(ggplot2)
library(tidyverse)
library(countrycode)
library(ggflags)
library(dplyr)

sf::sf_use_s2(FALSE)

#EEZs
EEZs_R12=st_read(here::here("Fig. 2","EEZs R12 both coasts", "EEZs_R12_both_coasts.shp"))

#MPAs including zoned in R12
MPAs_p=st_read(here::here("Fig. 1","Latest MPA shapefile","MPAs_EEZ_coastline_mangroves_zoned_16.shp")) #Once you have done this for every shapefile, you will just need to copy the "Zoned MPAs cleaned" folder into the dropbox folder. 

#MPA not in R12
MPAs_not_R12=st_read(here::here("Fig. 2", "MPAs not in region 12","Version sent to Adriana", "MPAs not in region 12","MPAs_fullset_not_R12_2.shp"))

#Combine both MPA shapefiles 
MPAs=rbind(MPAs_p[,c("WDPAID","NAME","IUCN_CAT","STATUS_YR","PARENT_ISO")], 
           MPAs_not_R12[,c("WDPAID","NAME","IUCN_CAT","STATUS_YR","PARENT_ISO")])

# Create a dataframe of EEZ_size
EEZs_R12$AREA_KM2=as.numeric(st_area(EEZs_R12)/10^6)
attach(EEZs_R12)
df=cbind.data.frame(SOVEREIGN1, AREA_KM2)
EEZ_size=aggregate(df$AREA_KM2, by=list(Category=df$SOVEREIGN1), FUN=sum)
colnames(EEZ_size)=c("Country","EEZ")

##------------------------ Assess no-take and all MPA extent for each country
#No-take MPA area in each country 
MPAs_NT_EEZs = MPAs %>% 
  group_by(PARENT_ISO) %>% 
  filter((IUCN_CAT %in% c("I","Ia", "Ib", "II", "III"))) %>% 
  st_union() %>% 
  st_intersection(EEZs_R12, .) 

MPAs_NT_EEZs$Area=as.numeric(st_area(MPAs_NT_EEZs)/10^6)
MPAs_NT_EEZs=sf::st_drop_geometry(MPAs_NT_EEZs)

#All MPA area in each country 
MPAs=st_make_valid(MPAs)
MPAs_all_EEZs = MPAs %>% 
  group_by(PARENT_ISO) %>% 
  st_union() %>% 
  st_intersection(EEZs_R12, .) 

MPAs_all_EEZs$Area=as.numeric(st_area(MPAs_all_EEZs)/10^6)
MPAs_all_EEZs=sf::st_drop_geometry(MPAs_all_EEZs)

#Summarise both dataframes to only keep country names and protected area extent  
MPAs_NT_df = MPAs_NT_EEZs %>% group_by(ISO_SOV1) %>% summarise(sum(Area))
colnames(MPAs_NT_df)=c("Country","NT_area")

MPAs_all_df = MPAs_all_EEZs %>% group_by(ISO_SOV1) %>% summarise(sum(Area))
colnames(MPAs_all_df)=c("Country","MPA_area")

#Combine both dataframes
DF = as.data.frame(full_join(MPAs_all_df, MPAs_NT_df))
DF[is.na(DF)]=0

#Recode full country names 
DF$Country <- recode_factor(DF$Country, CHL = "Chile", CRI = "Costa Rica", PER = "Peru", 
                            COL = "Colombia", GTM = "Guatemala", PAN = "Panama", NIC = "Nicaragua", MEX = "Mexico",
                            ECU = "Ecuador", SLV = "El Salvador", HND = "Honduras")

#Add EZZ size and calcualte percentages of EEZs covered by MPAs 
DF=full_join(DF, EEZ_size)
DF$Perc_NT=DF$NT_area/DF$EEZ*100
DF$Perc_MPA=DF$MPA_area/DF$EEZ*100

##-------------- Repeat the analysis for the 2010-2023 period only  

#No-take MPAs only 
MPAs_NT_EEZs_1023 = MPAs[MPAs$STATUS_YR > 2009,] %>% 
  group_by(PARENT_ISO) %>% 
  filter((IUCN_CAT %in% c("I","Ia", "Ib", "II", "III"))) %>% 
  st_union() %>% 
  st_intersection(EEZs_R12, .) 

MPAs_NT_EEZs_1023$Area=as.numeric(st_area(MPAs_NT_EEZs_1023)/10^6)
MPAs_NT_EEZs_1023=sf::st_drop_geometry(MPAs_NT_EEZs_1023)

#All MPAs 
MPAs_all_EEZs_1023 = MPAs[MPAs$STATUS_YR > 2009,] %>% 
  group_by(PARENT_ISO) %>% 
  st_union() %>% 
  st_intersection(EEZs_R12, .) 

MPAs_all_EEZs_1023$Area=as.numeric(st_area(MPAs_all_EEZs_1023)/10^6)
MPAs_all_EEZs_1023=sf::st_drop_geometry(MPAs_all_EEZs_1023)

#Summarise both dataframes to only keep country names and protected area extent  
MPAs_NT_df_1023 = MPAs_NT_EEZs_1023 %>% group_by(ISO_SOV1) %>% summarise(sum(Area))
colnames(MPAs_NT_df_1023)=c("Country","NT_area")

MPAs_all_df_1023 = MPAs_all_EEZs_1023 %>% group_by(ISO_SOV1) %>% summarise(sum(Area))
colnames(MPAs_all_df_1023)=c("Country","MPA_area")

#Combine both dataframes
DF_1023 = as.data.frame(full_join(MPAs_all_df_1023, MPAs_NT_df_1023))
DF_1023[is.na(DF_1023)]=0

DF_1023$Country <- recode_factor(DF_1023$Country, CHL = "Chile", CRI = "Costa Rica", PER = "Peru", 
                                 COL = "Colombia", GTM = "Guatemala", PAN = "Panama", NIC = "Nicaragua", MEX = "Mexico",
                                 ECU = "Ecuador", SLV = "El Salvador", HND = "Honduras")

DF_1023=full_join(DF_1023, EEZ_size)
DF_1023$Perc_NT=DF_1023$NT_area/DF_1023$EEZ*100
DF_1023$Perc_MPA=DF_1023$MPA_area/DF_1023$EEZ*100

DF$Period=rep("1935_2023", nrow(DF))
DF_1023$Period=rep("2010_2023", nrow(DF_1023))

DF_rbind=rbind.data.frame(DF, DF_1023)
MPA_expansion_and_extent=reshape::melt.data.frame(DF_rbind[,c(1,5:7)], id=c(1,4))

#Get the flag codes
MPA_expansion_and_extent$iso2 <- countrycode(MPA_expansion_and_extent$Country, "country.name", "iso2c")

MPA_expansion_and_extent = MPA_expansion_and_extent %>% 
  mutate(code = tolower(iso2))  #Convert to lower case for the geom_flag function 

MPA_expansion_and_extent$Country=factor(MPA_expansion_and_extent$Country, levels=rev(c("Guatemala", "El Salvador", "Peru", "Honduras", "Colombia", "Nicaragua", "Ecuador", "Mexico", "Panama", "Costa Rica", "Chile")))

MPA_expansion_and_extent$Combined_type=factor(paste0(MPA_expansion_and_extent$variable, "_",MPA_expansion_and_extent$Period))

MPA_expansion_and_extent$Combined_type <- recode_factor(MPA_expansion_and_extent$Combined_type, 
                                       "Perc_MPA_1935_2023" = "All MPA extent 2023", 
                                       "Perc_MPA_2010_2023" = "All MPA expansion 2010-2023", 
                                       "Perc_NT_1935_2023" = "No-take MPA extent 2023", 
                                       "Perc_NT_2010_2023" = "No-take MPA expansion 2010-2023")

MPA_expansion_and_extent$Combined_type=factor(MPA_expansion_and_extent$Combined_type, levels=c("No-take MPA expansion 2010-2023", "No-take MPA extent 2023", "All MPA expansion 2010-2023",
                                                             "All MPA extent 2023"))
save(MPA_expansion_and_extent, file="MPA_expansion_and_extent.Rdata")



