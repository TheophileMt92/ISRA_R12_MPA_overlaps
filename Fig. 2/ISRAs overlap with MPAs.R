##---------------------------------------------------------------------------------------------------------------------------------##
##----------------------          Dataframes of marine protected area overlaps wih ISRAs               ----------------------------##
##---------------------------------------------------------------------------------------------------------------------------------##
##---------- Théophile L. Mouton 16/01/2023
##---------- IUCN SSC SSG 

library(raster)
library(sf)
library(lwgeom)
library(ggplot2)
library(countrycode)
library(ggflags)
library(dplyr)

sf::sf_use_s2(FALSE)

#-----  ISRA region 12
Region=sf::st_read(dsn=here::here("Fig. 2","ISRA Region 12 Shapefiles"), 
                   layer = "ISRA_Region_12_v2") #This opens only a single layer 
Region=st_make_valid(Region)

# ---  EEZs of the Region 12 
EEZ=sf::st_read(dsn=here::here("Fig. 2","ISRA Region 12 EEZ"), 
                layer = "EEZ_R12") #This opens only a single layer 

# --- ISRAs as combined polygons
ISRAs=list.files(path=here::here("Fig. 2","ISRAs"), pattern="shp$", recursive=TRUE, full.names=TRUE)
x <- lapply(ISRAs, sf::st_read) 
sa=do.call(what = rbind, args = x)
sb=st_cast(sa)
sb=st_make_valid(sb)
su=st_union(sb, by_feature = FALSE)

#save(su, file="ISRA_su.Rdata")
#load(here::here("Fig. 2","ISRA_su.Rdata"))

# --- MPAs
MPAs_p=st_read(here::here("Fig. 1","Latest MPA shapefile","MPAs_EEZ_coastline_mangroves_zoned_16.shp")) 
MPAs_p=st_make_valid(MPAs_p)
MPAs_NT=st_read(here::here("Fig. 1","Latest MPA shapefile","MPAs_EEZ_coastline_mangroves_zoned_NOTAKE_16.shp")) 

#The percentage of the region covered by MPAs
(sum(st_area(st_union(MPAs_NT))/10^6)/sum(st_area(Region)/10^6)*100) #Percentage of the region covered by no-take MPAs
(sum(st_area(st_union(MPAs_p))/10^6)/sum(st_area(Region)/10^6)*100) #Percentage of the region covered by all MPAs 

#The percentage of all ISRAs covered by MPAs
Area_NT=st_area(st_intersection(MPAs_NT, su))/10^6 #The area of no-take MPAs overlapping ISRAs 
sum(Area_NT)/(st_area(su)/10^6)*100 #The percentage of ISRAs overlapped by no-take MPAs 

Area_p=st_area(st_intersection(MPAs_p, su))/10^6 #The area of MPAs overlapping ISRAs 
sum(Area_p)/(st_area(su)/10^6)*100 #The percentage of ISRAs overlapped by MPAs

#----------   For all MPAs
#Correct the area column in the attribute table 
EEZ$AREA_KM2=as.vector(round(st_area(EEZ)/10^6,1)) 
MPAs_p$REP_M_AREA=as.vector(round(st_area(MPAs_p)/10^6,1)) 

EEZ_MPA_p_intersect=st_intersection(EEZ, MPAs_p)
EEZ_MPA_p_intersect$REP_M_AREA=as.vector(round(st_area(EEZ_MPA_p_intersect)/10^6,1)) 

#Area of EEZs
df_p = cbind.data.frame(EEZ$SOVEREIGN1,EEZ$AREA_KM2)
colnames(df_p)=c("Country", "Area")
df_p=aggregate(df_p$Area, by=list(Category=df_p$Country), FUN=sum)
colnames(df_p)=c("Country", "Area")

#Area of MPAs by country
df2_p = cbind.data.frame(EEZ_MPA_p_intersect$SOVEREIGN1,EEZ_MPA_p_intersect$AREA_KM2, EEZ_MPA_p_intersect$REP_M_AREA)
df2_p=aggregate(df2_p$`EEZ_MPA_p_intersect$REP_M_AREA`, by=list(Category=df2_p$`EEZ_MPA_p_intersect$SOVEREIGN1`), FUN=sum)
colnames(df2_p)[1]="Country"
df3_p=dplyr::full_join(df2_p, df_p)
df_p=df3_p
colnames(df_p)=c("Country", "All MPAs", "EEZ")
df_p = df_p[order(df_p$EEZ),]
df_p=df_p[!df_p$Country == "Argentina",] #Remove Argentina
df_p[is.na(df_p)]=0
df_p=df_p[,c(1,3,2)]

#----------   For no-take MPAs
#Correct the area column in the attribute table 
EEZ$AREA_KM2=as.vector(round(st_area(EEZ)/10^6,1)) 
MPAs_NT$REP_M_AREA=as.vector(round(st_area(MPAs_NT)/10^6,1)) 

EEZ_MPA_NT_intersect=st_intersection(EEZ, MPAs_NT)
EEZ_MPA_NT_intersect$REP_M_AREA=as.vector(round(st_area(EEZ_MPA_NT_intersect)/10^6,1)) 

#Area of EEZs
df = cbind.data.frame(EEZ$SOVEREIGN1,EEZ$AREA_KM2)
colnames(df)=c("Country", "Area")
df=aggregate(df$Area, by=list(Category=df$Country), FUN=sum)
colnames(df)=c("Country", "Area")

#Area of MPAs by country
df2 = cbind.data.frame(EEZ_MPA_NT_intersect$SOVEREIGN1,EEZ_MPA_NT_intersect$AREA_KM2, EEZ_MPA_NT_intersect$REP_M_AREA)
df2=aggregate(df2$`EEZ_MPA_NT_intersect$REP_M_AREA`, by=list(Category=df2$`EEZ_MPA_NT_intersect$SOVEREIGN1`), FUN=sum)
colnames(df2)[1]="Country"
df3=dplyr::full_join(df2, df)
df=df3
colnames(df)=c("Country", "No-take MPAs", "EEZ")
df = df[order(df$EEZ),]
df=df[!df$Country == "Argentina",] #Remove Argentina
df[is.na(df)]=0
df=df[,c(1,3,2)]

#Combine both dataframes 
library(tidyverse)
df_all=full_join(df, df_p)

delta=df_all$`All MPAs` - df_all$`No-take MPA` #All deltas are positive, good ! 

df_melt=reshape::melt.data.frame(df_all, id=1:2)
df_melt$label=df_melt$Country
df_melt$label[13:24]=""

df_melt$value=as.numeric(paste(df_melt$value))

#----------------------------------------------------------------------------------------------------#
#---------------------   Now add the area of ISRAs overlapped by all MPAs ---------------------------#
#----------------------------------------------------------------------------------------------------#
#Intersect ISRAs with EEZ
EEZ_ISRA_intersect=st_intersection(EEZ, su)
EEZ_ISRA_intersect$ISRA_EEZ_area=as.vector(round(st_area(EEZ_ISRA_intersect)/10^6,1)) 

DF = cbind.data.frame(EEZ_ISRA_intersect$SOVEREIGN1, EEZ_ISRA_intersect$ISRA_EEZ_area)
colnames(DF)=c("Country", "ISRA_EEZ_Area")
DF2=aggregate(DF$ISRA_EEZ_Area, by=list(Category=DF$Country), FUN=sum)
colnames(DF2)=c("Country", "ISRA")

#Now add this to the previous dataframe to measure percentage of ISRA protected  
df_ISRA = dplyr::full_join(df_all, DF2)

write.csv(df_ISRA, "df_ISRA.csv")
#---------------- Measure All MPA overlaps with ISRAs 
#Intersect MPAs with EEZ_ISRA_intersect 
MPA_p_ISRA_intersect=st_intersection(EEZ_ISRA_intersect, MPAs_p)
MPA_p_ISRA_intersect$ISRA_MPA_p_area=as.vector(round(st_area(MPA_p_ISRA_intersect)/10^6,1)) 

DFF_p = cbind.data.frame(MPA_p_ISRA_intersect$SOVEREIGN1, MPA_p_ISRA_intersect$ISRA_MPA_p_area)
colnames(DFF_p)=c("Country", "ISRA_MPA_p_Area")
DFF2_p=aggregate(DFF_p$ISRA_MPA_p_Area, by=list(Category=DFF_p$Country), FUN=sum)
colnames(DFF2_p)=c("Country", "ISRA_protec_p")

df_ISRA_2 = dplyr::full_join(df_ISRA, DFF2_p)
df_ISRA_2$"Perc_ISRA_prot_All"=df_ISRA_2$ISRA_protec_p/df_ISRA_2$ISRA*100

#---------------- Measure no-take MPA overlaps with ISRAs 
#Intersect MPAs with EEZ_ISRA_intersect 
MPA_NT_ISRA_intersect=st_intersection(EEZ_ISRA_intersect, MPAs_NT)
MPA_NT_ISRA_intersect$ISRA_MPA_NT_area=as.vector(round(st_area(MPA_NT_ISRA_intersect)/10^6,1)) 

DFF_NT = cbind.data.frame(MPA_NT_ISRA_intersect$SOVEREIGN1, MPA_NT_ISRA_intersect$ISRA_MPA_NT_area)
colnames(DFF_NT)=c("Country", "ISRA_MPA_NT_Area")
DFF2_NT=aggregate(DFF_NT$ISRA_MPA_NT_Area, by=list(Category=DFF_NT$Country), FUN=sum)
colnames(DFF2_NT)=c("Country", "ISRA_protec_NT")

df_ISRA_3 = dplyr::full_join(df_ISRA_2, DFF2_NT)
df_ISRA_3$"Perc_ISRA_prot_NT"=df_ISRA_3$ISRA_protec_NT/df_ISRA_3$ISRA*100

df_ISRA_plot=df_ISRA_3[,c(1,7,9)]
df_ISRA_plot[is.na(df_ISRA_plot)]=0

##--------------------- Make a nice graph with country flags and the numbers aside 
df_scat_melt2=reshape::melt(df_ISRA_plot, id=1) 

df_scat_melt2_NT = df_scat_melt2[df_scat_melt2$variable == "Perc_ISRA_prot_NT",]
df_scat_melt2_NT$Perc_ISRA_ov_NT=paste(round(df_scat_melt2_NT$value,1), "%", sep = "")

df_scat_melt2_P = df_scat_melt2[df_scat_melt2$variable == "Perc_ISRA_prot_All",]
df_scat_melt2_P$Perc_ISRA_ov_P=paste(round(df_scat_melt2_P$value,1), "%", sep = "")

#Get the flag codes
df_scat_melt2_NT$iso2 <- countrycode(df_scat_melt2_NT$Country, "country.name", "iso2c")

df_scat_melt2_NT = df_scat_melt2_NT %>% 
  mutate(code = tolower(iso2))  #Convert to lower case for the geom_flag function 

#Rank by percentage area as no-take 
df_scat_melt2_NT[-c(1,2),]=df_scat_melt2_NT[-c(1,2),] %>% arrange(value)
df_scat_melt2_P=df_scat_melt2_P %>% arrange(value)

df_scat_melt2_NT$Country=factor(df_scat_melt2_NT$Country, levels=df_scat_melt2_NT$Country)
df_scat_melt2_P$Country=factor(df_scat_melt2_P$Country, levels=df_scat_melt2_P$Country)

df_scat_melt2$Country=factor(df_scat_melt2$Country, levels=rev(unique(df_scat_melt2_P$Country)))

save(df_scat_melt2, file="df_scat_melt2.Rdata")
save(df_scat_melt2_NT, file = "df_scat_melt2_NT.Rdata")
save(df_scat_melt2_P, file = "df_scat_melt2_P.Rdata")


