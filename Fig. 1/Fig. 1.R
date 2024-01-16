##---------------------------------------------------------------------------------------------------------------------------------##
##------------------------------ Area charts of temporal trends (1935 - 2023) in marine protected area ----------------------------##
##------------------------------                      (A) numbers and (B) size                         ----------------------------##
##------------------------------             in the Central and South American Pacific                 ----------------------------##
##---------------------------------------------------------------------------------------------------------------------------------##
##---------- Théophile L. Mouton 16/01/2023
##---------- IUCN SSC SSG 

library(sf)
library(ggplot2)
library(lwgeom)
library(data.table)
library(tidyverse) 
library(lubridate)
library(viridis)
library(geomtextpath)
library(ggpubr)
library(data.table)

#Open the MPA shapefile 
MPAs=st_read(here::here("Fig. 1", "Latest MPA shapefile","MPAs_EEZ_coastline_mangroves_zoned_16.shp")) 

sf::sf_use_s2(FALSE) #Remove spherical geometry 

#Prepare the dataframes for plotting
Num_MPAs=rep(1, nrow(MPAs))
Year=paste(as.numeric(MPAs$STATUS_YR))
MPAs$REP_M_AREA=as.vector(round(st_area(MPAs)/10^6,1))
Area=MPAs$REP_M_AREA
IUCN_cat=as.factor(MPAs$IUCN_CAT)
Country=MPAs$PARENT_ISO

df_yr_cat=cbind.data.frame(Year, Num_MPAs, Area, IUCN_cat, Country)
df_yr_cat=df_yr_cat[order(df_yr_cat$Year),]
df_yr_cat$IUCN_cat <- plyr::revalue(df_yr_cat$IUCN_cat, c("I" = "II"))

#Number of protected areas dataframe
df_yr_cast_pa=reshape2::dcast(data = df_yr_cat[,-2], Year + Country ~ IUCN_cat)
df_yr_cast_pa$Other=rowSums(df_yr_cast_pa[,c(8,9)])
df_yr_cast_pa[,3:12]=cumsum(df_yr_cast_pa[,3:12])
df_yr_cast_pa=df_yr_cast_pa[,-c(8,9)]
df_yr_melt_pa=reshape2::melt(df_yr_cast_pa, id = c(1,2))
colnames(df_yr_melt_pa)[3]="IUCN category"

#Protected marine area (km2) dataframe
df_yr_cast_area=reshape2::dcast(data = df_yr_cat[,-2], Year + Country ~ IUCN_cat, fun.aggregate = sum, value.var = "Area")
df_yr_cast_area$Other=rowSums(df_yr_cast_area[,c(8,9)])
df_yr_cast_area[,3:12]=cumsum(df_yr_cast_area[,3:12])
df_yr_cast_area=df_yr_cast_area[,-c(8,9)]
df_yr_melt_area=reshape2::melt(df_yr_cast_area, id = c(1,2))
colnames(df_yr_melt_area)[3]="IUCN category"

#Cumulative number of MPAs cumulatively by IUCN categories 
cc <- scales::seq_gradient_pal("blue", "yellow", "Lab")(seq(0,1,length.out=8))

df_yr_melt_pa$`IUCN cat` %>% as.factor()
df_yr_melt_pa=as.data.frame(df_yr_melt_pa)
df_yr_melt_area=as.data.frame(df_yr_melt_area)
df_yr_melt_pa$Year=as.numeric(paste(df_yr_melt_pa$Year))

df_yr_melt_pa$`IUCN category` <- factor(df_yr_melt_pa$`IUCN category`,levels=c("Ia", "Ib", "II", "III", "IV", "V", "VI", "Other"))
df_yr_melt_area$`IUCN category` <- factor(df_yr_melt_area$`IUCN category`,levels=c("Ia", "Ib", "II", "III", "IV", "V", "VI", "Other"))

a1 = 
  ggplot(df_yr_melt_pa, aes()) + 
  geom_area(aes(x=Year, y = value, fill = `IUCN category`, color= `IUCN category`), position = position_stack(reverse = TRUE)) +
  ylab(expression("Cumulative number of marine protected areas")) +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) + 
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(color="black", size = 11),
        axis.title = element_text(size = 14))

a1

leg = ggpubr::get_legend(a1, position = NULL)
as_ggplot(leg)

#Cumulative protected area (km2) cumulatively by IUCN categories 
df_yr_melt_area$Year=as.numeric(paste(df_yr_melt_area$Year))

a2 = ggplot(df_yr_melt_area, aes(x=Year, y = value, fill = `IUCN category`, color= `IUCN category`)) + 
  geom_area(aes(x=Year, y = value, fill = `IUCN category`, color= `IUCN category`), position = position_stack(reverse = TRUE)) +
  ylab(expression ("Cumulative marine area protected "~(km^2))) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) + 
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(color="black", size = 11),
        axis.title = element_text(size = 14))

a2

##----------- Create single barplots to put on the right of both panels  

#Number of MPAs
df_2022_pa=df_yr_melt_pa[df_yr_melt_pa$Year == 2022,]
df_2022_pa$"MPA_type"=c(rep("No-take", 20),rep("Partial", 20))

dd = df_2022_pa[df_2022_pa$MPA_type == "No-take",]
sum(dd$value)/sum(df_2022_pa$value)*100

b1 = ggplot(df_2022_pa, aes(x=Year, y = value, fill= MPA_type, color=MPA_type)) + 
  geom_col(position = position_fill(reverse = TRUE)) +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  scale_color_manual(values=c("#69b3a2", "#404080")) +
  geom_text(aes(x = 2022, y = 0.18, label="No-take MPAs (41%)", color="white"), angle = 90, color="white") +
  geom_text(aes(x = 2022, y = 0.68, label="Partial MPAs (59%)", color="white"), angle = 90, color="white") +
  theme_minimal() + 
  theme(legend.position = "none",
        axis.text = element_text(color=NA),
        axis.title = element_text(color=NA),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
b1

#Area of MPAs
df_2022_area=df_yr_melt_area[df_yr_melt_area$Year == 2022,]
df_2022_area$"MPA_type"=c(rep("No-take", 20),rep("Partial", 20))

dd = df_2022_area[df_2022_area$MPA_type == "No-take",]
sum(dd$value)/sum(df_2022_area$value)*100

b2 = ggplot(df_2022_area, aes(x=Year, y = value, fill= MPA_type, color=MPA_type)) + 
  geom_col(position = position_fill(reverse = TRUE)) +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  scale_color_manual(values=c("#69b3a2", "#404080")) +
  geom_text(aes(x = 2022, y = 0.21, label="No-take MPAs (50%)", color="white"), angle = 90, color="white") +
  geom_text(aes(x = 2022, y = 0.68, label="Partial MPAs (50%)", color="white"), angle = 90, color="white") +
  theme_minimal() + 
  theme(legend.position = "none",
        axis.text = element_text(color=NA),
        axis.title = element_text(color=NA),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
b2

gg3 = ggpubr::ggarrange(a1, NULL, b1,
                        a2, NULL, b2, ncol = 6, widths=c(0.85, -0.10, 0.15, 0.85, -0.10, 0.15), 
                        labels=c("A", "", "", "B", "", ""),
                        font.label = list(size = 24, face = "bold", color ="black"),
                        align="hv", common.legend = T, legend = "right")
gg3

ggsave("Figure 2 - Area plots of trends in cumulative marine area protected with barplots on side_1312_2023.jpeg", 
       gg3, width = 500*0.7, height = 350*0.7*0.6, units = "mm") 

