library(sf)
library(ggplot2)

#load("MPAs_all.Rdata")
MPAs_p=st_read(here::here("MPAs_EEZ_coastline_mangroves_zoned_2","MPAs_EEZ_coastline_mangroves_zoned_16.shp")) 
MPAs=MPAs_p

sf::sf_use_s2(FALSE)

#Start the dataframes for ploting
Num_MPAs=rep(1, nrow(MPAs))
Year=paste(as.numeric(MPAs$STATUS_YR))
MPAs$REP_M_AREA=as.vector(round(st_area(MPAs)/10^6,1))
Area=MPAs$REP_M_AREA
IUCN_cat=as.factor(MPAs$IUCN_CAT)
Country=MPAs$PARENT_ISO

df_yr=cbind.data.frame(Year, Num_MPAs, Area)
df_yr=df_yr[order(df_yr$Year),]
df_yr$Cum_Num_MPAs=cumsum(df_yr$Num_MPAs)
df_yr$Cum_Area=cumsum(df_yr$Area)

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

#---  Measure the MPAs for the text 
#df_yr_2010_22=df_yr[df_yr$Year > 2009, ]
#sum(df_yr_2010_22$Area)/2827872.8*100

#R12_polygon=st_read(here::here("ISRA Region 12 Shapefiles", "ISRA_Region_12_v2.shp"))
#plot(st_geometry(R12_polygon))
#2827872.8/(st_area(R12_polygon)/10^6)*100

#7.7*(3.5/7.3) #No-take MPAs in the region 

#Cumulative number of MPAs cumulatively by IUCN categories 
library(data.table)

cc <- scales::seq_gradient_pal("blue", "yellow", "Lab")(seq(0,1,length.out=8))

df_yr_melt_pa$`IUCN cat` %>% as.factor()

df_yr_melt_pa=as.data.frame(df_yr_melt_pa)

df_yr_melt_area=as.data.frame(df_yr_melt_area)

library(tidyverse) 
library(lubridate)
library(viridis)
library(geomtextpath)

#df_yr_melt_pa$`IUCN cat`=factor(df_yr_melt_pa$`IUCN cat`,levels=rev(levels(df_yr_melt_pa$`IUCN cat`)))
df_yr_melt_pa$Year=as.numeric(paste(df_yr_melt_pa$Year))

df_yr_melt_pa$`IUCN category` <- factor(df_yr_melt_pa$`IUCN category`,levels=c("Ia", "Ib", "II", "III", "IV", "V", "VI", "Other"))
df_yr_melt_area$`IUCN category` <- factor(df_yr_melt_area$`IUCN category`,levels=c("Ia", "Ib", "II", "III", "IV", "V", "VI", "Other"))

a6 = 
  ggplot(df_yr_melt_pa, aes()) + 
#  geom_texthline(label = "No-take MPAs", yintercept = 70, vjust = 1.25, hjust=0.025, linetype="dashed") +
  #geom_line(size = 1.3) +
  geom_area(aes(x=Year, y = value, fill = `IUCN category`, color= `IUCN category`), position = position_stack(reverse = TRUE)) +
  ylab(expression("Cumulative number of marine protected areas")) +
   scale_x_continuous(expand = c(0,0)) +
  #scale_y_continuous(sec.axis = sec_axis(trans=~./172*100, name="Cumulative percentage of marine protected areas")) +
 # scale_fill_manual(values=cc) + 
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) + 
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(color="black", size = 11),
        axis.title = element_text(size = 14))

a6

library(ggpubr)
leg = ggpubr::get_legend(a6, position = NULL)
as_ggplot(leg)
#Cumulative protected area (km2) cumulatively by IUCN categories 
library(data.table)

df_yr_melt_area$Year=as.numeric(paste(df_yr_melt_area$Year))

a3 = ggplot(df_yr_melt_area, aes(x=Year, y = value, fill = `IUCN category`, color= `IUCN category`)) + 
#  geom_texthline(label = "No-take MPAs", yintercept = 1525529, vjust = 1.25, hjust=0.025, linetype="dashed") +
  geom_area(aes(x=Year, y = value, fill = `IUCN category`, color= `IUCN category`), position = position_stack(reverse = TRUE)) +
  ylab(expression ("Cumulative marine area protected "~(km^2))) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) +
  #                   sec.axis = sec_axis(trans=~./Region_area*100, name="Percentage of Central and South Pacific region")) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) + 
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(color="black", size = 11),
        axis.title = element_text(size = 14))

a3

gg = ggpubr::ggarrange(a6, a3, ncol = 2, labels=c("A", "B"), common.legend = T, legend = "right",  
                       font.label = list(size = 18, color = "black", face = "bold", family = NULL),
                       align = "h", widths =  c(1, 0.97))
gg

ggsave("Figure 1 - Area plots of trends in cumulative marine area protected.jpeg", 
       gg, width = 500*0.8, height = 350*0.7*0.6, units = "mm") 

gg2 = ggpubr::ggarrange(a5, a3, ncol = 1, labels=c("A", "B"), common.legend = T, legend = "right",  
                       font.label = list(size = 18, color = "black", face = "bold", family = NULL),
                       align = "hv")
gg2


ggsave("Figure 1 - Area plots of trends in cumulative marine area protected.jpeg", 
       gg, width = 500*0.8, height = 350*0.7*0.6, units = "mm") 


##----------- Create single barplots to put on the right of the figures 

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

gg1 = ggpubr::ggarrange(a6, NULL, b1, ncol=3, widths = c(0.85, -0.06, 0.15), align="hv")
gg1

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

gg2 = ggpubr::ggarrange(a3, NULL, b2, ncol = 3, widths = c(0.85, -0.06, 0.15), align="hv")
gg2
#gg3 = ggpubr::ggarrange(gg1, gg2, leg, ncol = 3, align="hv", common.legend = T, legend = "left")
#gg3

gg3 = ggpubr::ggarrange(a6, NULL, b1,
                        a3, NULL, b2, ncol = 6, widths=c(0.85, -0.10, 0.15, 0.85, -0.10, 0.15), 
                        labels=c("A", "", "", "B", "", ""),
                        font.label = list(size = 24, face = "bold", color ="black"),
                        align="hv", common.legend = T, legend = "right")
gg3

ggsave("Figure 2 - Area plots of trends in cumulative marine area protected with barplots on side_1312_2023.jpeg", 
       gg3, width = 500*0.7, height = 350*0.7*0.6, units = "mm") 


##Continue to include text on right of the plot instead of legend 

max(df_yr_melt_pa_cum$Year)

final<-df_yr_melt_pa_cum%>%
  filter(Year=="2022")%>%              # Keep only 2017 value
  arrange(`IUCN category`)%>%                # Inverse factor order (first is at the bottom of plot)
  mutate(                              # Create new column ypos and
    ypos=cumsum(value)       # fill with cumulative sum of invest for 2017
  )                                     

final

final$IUCN_cat_text=c("Ia – Strict nature reserve",
                             "Ib – Wilderness area",
                             "II – National park",
                             "III – Natural monument or feature",
                             "IV – Habitat or species\n management area",
                             "V – Protected landscape\n or seascape",
                             "VI – Protected area with sustainable\n use of natural resources",
                             "Other")


a5 = 
  ggplot(df_yr_melt_pa_cum, aes()) + 
#  geom_texthline(label = "No-take MPAs", yintercept = 70, vjust = 1.25, hjust=0.025, linetype="dashed", size=5) +
  geom_segment(aes(x = 2022, xend = 2060, y = 70, yend = 70), color="white") +
  geom_area(aes(x=Year, y = value, fill = `IUCN category`, color= `IUCN category`), position = position_stack(reverse = TRUE)) +
  geom_text(data=final, aes(y=ypos-(value/2),label=IUCN_cat_text, color= `IUCN category`), x=2025, hjust = 0, size=5) +
  ggtitle("Number of marine protected areas") + xlab("") +
  scale_x_continuous(expand = c(0,0), limits=c(1935,2060), breaks = c(1950,1975,2000,2020)) +
  scale_y_continuous(expand = c(0,0)) +
  # scale_fill_manual(values=cc) + 
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) + 
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(color="black", size = 16),
        axis.title = element_blank(),
        axis.line.x = element_blank(),
        plot.title = element_text(size = 20, face = "bold"))

a5

final_area<-df_yr_melt_area_cum%>%
  filter(Year=="2022")%>%              # Keep only 2017 value
  arrange(`IUCN category`)%>%                # Inverse factor order (first is at the bottom of plot)
  mutate(                              # Create new column ypos and
    ypos=cumsum(value)       # fill with cumulative sum of invest for 2017
  )                                     

final_area$IUCN_cat_text=c("Ia – Strict nature reserve",
                      "Ib – Wilderness area",
                      "II – National park",
                      "III – Natural monument or feature",
                      "IV – Habitat or species\n management area",
                      "V – Protected landscape\n or seascape",
                      "VI – Protected area with sustainable\n use of natural resources",
                      "Other")

final_area_big_sub=final_area[-c(2:4),]
final_area_small_sub=final_area[c(2:4),]

a3 = ggplot(df_yr_melt_area_cum, aes(x=Year, y = value, fill = `IUCN category`, color= `IUCN category`)) + 
#  geom_texthline(label = "No-take MPAs", yintercept = 1525529, vjust = 1.25, hjust=0.025, linetype="dashed", size=5) +
  geom_segment(aes(x = 2022, xend = 2060, y = 1525529, yend = 1525529), color="white") +
  geom_area(aes(x=Year, y = value, fill = `IUCN category`, color= `IUCN category`), position = position_stack(reverse = TRUE)) +
  geom_text(data=final_area_big_sub, aes(y=ypos-(value/2),label=IUCN_cat_text, color= `IUCN category`), x=2025, hjust = 0, size=5) +
  ggrepel::geom_text_repel(data=final_area_small_sub, aes(y=ypos-(value/2),label=IUCN_cat_text, color= `IUCN category`), 
                           x=2025, hjust = 0, direction = "y", size=5, segment.color = "transparent") +
  ggtitle(expression(bold("Marine area protected"~(km^2)))) + xlab("") +
  scale_x_continuous(expand = c(0,0), limits=c(1935,2060), breaks = c(1950,1975,2000,2020)) +
  scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6), expand = c(0,0)) +
  #                   sec.axis = sec_axis(trans=~./Region_area*100, name="Percentage of Central and South Pacific region")) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) + 
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(color="black", size = 16),
        axis.title = element_blank(),
        axis.line.x = element_blank(),
        plot.title = element_text(size = 20, face = "bold"))

a3

gg2 = ggpubr::ggarrange(a5, a3, ncol = 1, labels=c("A", "B"), common.legend = T, legend = "none",  
                        font.label = list(size = 22, color = "black", face = "bold", family = NULL),
                        align = "hv")
gg2

ggsave("Figure 1 - Area plots of trends in cumulative marine area protected with text_no line.jpeg", 
       gg2, width = 350, height = 400, units = "mm", dpi=600) 



##----------------------------- Make one for each country 
##Apply cumulative sum by country 
#Number of protected areas dataframe
df_yr_cast_pa=reshape2::dcast(data = df_yr_cat[,-2], Year + Country ~ IUCN_cat)
df_yr_cast_pa$Other=rowSums(df_yr_cast_pa[,c(8,9)])
df_yr_cast_pa=df_yr_cast_pa[,-c(8,9)]
df_yr_melt_pa=reshape2::melt(df_yr_cast_pa, id = c(1,2))
df_yr_melt_pa=df_yr_melt_pa %>% group_by(Country, variable) %>% mutate(cumsum = cumsum(value))
colnames(df_yr_melt_pa)[3]="IUCN category"

#Remove the french MPAs 
a5 = 
  ggplot(df_yr_melt_pa, aes()) + 
  #  geom_texthline(label = "No-take MPAs", yintercept = 70, vjust = 1.25, hjust=0.025, linetype="dashed") +
  #geom_line(size = 1.3) +
  geom_area(aes(x=Year, y = cumsum, fill = `IUCN category`, color= `IUCN category`), position = position_stack(reverse = TRUE)) +
  facet_wrap(~Country) + 
  ylab("Cumulative number of marine protected areas") +
  scale_x_continuous(expand = c(0,0)) +
  #scale_y_continuous(sec.axis = sec_axis(trans=~./172*100, name="Cumulative percentage of marine protected areas")) +
  # scale_fill_manual(values=cc) + 
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) + 
  theme_classic() +
  theme(axis.text = element_text(color="black", size = 11),
        axis.title = element_text(size = 14))

a5

#-------------------------------
#Protected marine area (km2) dataframe
df_yr_cast_area=reshape2::dcast(data = df_yr_cat[,-2], Year + Country ~ IUCN_cat, fun.aggregate = sum, value.var = "Area")
df_yr_cast_area$Other=rowSums(df_yr_cast_area[,c(8,9)])
df_yr_cast_area=df_yr_cast_area[,-c(8,9)]
df_yr_melt_area=reshape2::melt(df_yr_cast_area, id = c(1,2))
df_yr_melt_area=df_yr_melt_area %>% group_by(Country, variable) %>% mutate(cumsum = cumsum(value))
colnames(df_yr_melt_area)[3]="IUCN category"

a3 = ggplot(df_yr_melt_area, aes(x=Year, y = value, fill = `IUCN category`, color= `IUCN category`)) + 
  #  geom_texthline(label = "No-take MPAs", yintercept = 1525529, vjust = 1.25, hjust=0.025, linetype="dashed") +
  geom_area(aes(x=Year, y = cumsum, fill = `IUCN category`, color= `IUCN category`), position = position_stack(reverse = TRUE)) +
  facet_wrap(~Country, scales="free_y") + 
  ylab(expression ("Cumulative marine area protected "~(km^2))) +
  scale_x_continuous(expand = c(0,0)) +
#  scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) +
  #                   sec.axis = sec_axis(trans=~./Region_area*100, name="Percentage of Central and South Pacific region")) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) + 
  theme_classic() +
  theme(axis.text = element_text(color="black", size = 11),
        axis.title = element_text(size = 14))

a3

##----------------------  Divide area by EEZ size 

load("EEZ_size.Rdata")

df_yr_melt_area=df_yr_melt_area[!df_yr_melt_area$Country == "PYF",]

df_yr_melt_area$Country <- recode_factor(df_yr_melt_area$Country, CHL = "Chile", CRI = "Costa Rica", PER = "Peru", 
                                         COL = "Colombia", GTM = "Guatemala", PAN = "Panama", NIC = "Nicaragua", MEX = "Mexico",
                                         ECU = "Ecuador", SLV = "Salvador", HND = "Honduras")


df_yr_melt_area_EEZ=left_join(df_yr_melt_area, EEZ_size)
df_yr_melt_area_EEZ$value=df_yr_melt_area_EEZ$value/df_yr_melt_area_EEZ$EEZ*100
df_yr_melt_area_EEZ$cumsum=df_yr_melt_area_EEZ$cumsum/df_yr_melt_area_EEZ$EEZ*100

a3 = ggplot(df_yr_melt_area_EEZ, aes(x=Year, y = value, fill = `IUCN category`, color= `IUCN category`)) + 
  #  geom_texthline(label = "No-take MPAs", yintercept = 1525529, vjust = 1.25, hjust=0.025, linetype="dashed") +
  geom_area(aes(x=Year, y = cumsum, fill = `IUCN category`, color= `IUCN category`), position = position_stack(reverse = TRUE)) +
  facet_wrap(~Country) + 
  ylab("Cumulative marine area protected (%)") +
  scale_x_continuous(expand = c(0,0)) +
  #  scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) +
  #                   sec.axis = sec_axis(trans=~./Region_area*100, name="Percentage of Central and South Pacific region")) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) + 
  theme_bw() +
  theme(axis.text = element_text(color="black", size = 11),
        axis.title = element_text(size = 14))

a3

library(viridis)
library(sf)
##--------------------- Do it for no-take vs partial MPAs 
df_yr_cast_area_NTP=df_yr_cast_area
df_yr_cast_area_NTP$"No-take"=rowSums(df_yr_cast_area[,c(3:6)])
df_yr_cast_area_NTP$"Partial"=rowSums(df_yr_cast_area[,c(7:10)])

df_yr_melt_area_NTP=reshape2::melt(df_yr_cast_area_NTP[,c(1,2,11,12)], id = c(1,2))
colnames(df_yr_melt_area_NTP)[3]="NTP"
df_yr_melt_area_NTP=df_yr_melt_area_NTP %>% group_by(Country, NTP) %>% mutate(cumsum = cumsum(value))

#df_yr_melt_area_NTP=df_yr_melt_area_NTP[!df_yr_melt_area_NTP$Country == "PYF",]

df_yr_melt_area_NTP$Country <- recode_factor(df_yr_melt_area_NTP$Country, CHL = "Chile", CRI = "Costa Rica", PER = "Peru", 
                                         COL = "Colombia", GTM = "Guatemala", PAN = "Panama", NIC = "Nicaragua", MEX = "Mexico",
                                         ECU = "Ecuador", SLV = "El Salvador", HND = "Honduras", PYF="France")


#---------------------------------- % MPA expansion at the scale of the region 
df_region_sum = df_yr_melt_area_NTP %>% 
  group_by(Year, NTP) %>% 
  reframe(Area_Region = sum(value))

df_R_NT = df_region_sum[df_region_sum$NTP == "No-take",]
df_R_P = df_region_sum[df_region_sum$NTP == "Partial",]

df_R_NT$cumsum = cumsum(df_R_NT$Area_Region)
df_R_P$cumsum = cumsum(df_R_P$Area_Region)

df_R_NT_2010=df_R_NT[df_R_NT$Year > 2009,]
df_R_P_2010=df_R_P[df_R_P$Year > 2009,]

#Now divide by the size of region 
df_R_NT_2010$Perc_area_region=df_R_NT_2010$cumsum/(st_area(Region)/10^6)*100
df_R_P_2010$Perc_area_region=df_R_P_2010$cumsum/(st_area(Region)/10^6)*100

Perc_NT_Exp_Region=as.numeric(df_R_NT_2010[13,5]-df_R_NT_2010[1,5])
Perc_P_Exp_Region=as.numeric(df_R_P_2010[13,5]-df_R_P_2010[1,5])

##---------------- --------- % MPA expantion in 2010-2022 for each country 
df_yr_melt_area_NTP=left_join(df_yr_melt_area_NTP, EEZ_size)
df_yr_melt_area_NTP$value=df_yr_melt_area_NTP$value/df_yr_melt_area_NTP$EEZ*100
df_yr_melt_area_NTP$cumsum=df_yr_melt_area_NTP$cumsum/df_yr_melt_area_NTP$EEZ*100

df_yr_melt_area_NTP_2010=df_yr_melt_area_NTP[df_yr_melt_area_NTP$Year > 2009,]

a3 = ggplot(df_yr_melt_area_NTP, aes(x=Year, y = value, fill = NTP, color= NTP)) + 
  #  geom_texthline(label = "No-take MPAs", yintercept = 1525529, vjust = 1.25, hjust=0.025, linetype="dashed") +
  geom_area(aes(x=Year, y = cumsum, fill = NTP, color= NTP), position = position_stack(reverse = TRUE)) +
  facet_wrap(~Country) + 
  ylab("Cumulative marine area protected (%)") +
  scale_x_continuous(expand = c(0,0)) +
  #  scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) +
  #                   sec.axis = sec_axis(trans=~./Region_area*100, name="Percentage of Central and South Pacific region")) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) + 
  theme_bw() +
  theme(axis.text = element_text(color="black", size = 11),
        axis.title = element_text(size = 14))

a3

##--------------------------------- % MPA expansion in 2010-2022
#Maximum value 
library(dplyr)

#No-take 
df_NT = df_yr_melt_area_NTP_2010[df_yr_melt_area_NTP_2010$NTP == "No-take",]
df_P = df_yr_melt_area_NTP_2010[df_yr_melt_area_NTP_2010$NTP == "Partial",]

max_df_NT=df_NT[,c(2,5)] %>%
  group_by(Country) %>%
  summarise_each(funs(max))

min_df_NT=df_NT[,c(2,5)] %>%
  group_by(Country) %>%
  summarise_each(funs(min))

#Partial 
max_df_P=df_P[,c(2,5)] %>%
  group_by(Country) %>%
  summarise_each(funs(max))

min_df_P=df_P[,c(2,5)] %>%
  group_by(Country) %>%
  summarise_each(funs(min))

Delta_P=max_df_P$cumsum-min_df_P$cumsum
Delta_NT=max_df_NT$cumsum-min_df_NT$cumsum

df_deltas = cbind.data.frame(max_df_NT$Country,Delta_P, Delta_NT)
colnames(df_deltas)[1]="Country"
df_deltas[is.na(df_deltas)]=0
colnames(df_deltas)[2:3]=c("Delta_P", "Delta_NT")
df_add_data=data.frame(Country = c("Honduras", "Nicaragua","France","Whole Region"), Delta_P = c(0, 0,0.414360062,Perc_P_Exp_Region), Delta_NT = c(0,0,0,Perc_NT_Exp_Region))

df_deltas=df_deltas[!df_deltas$Country == "France",]

df_deltas_11=rbind(df_deltas, df_add_data)
colnames(df_deltas_11)[2:3]=c("Partial", "No-take")

df_deltas_11$"All MPAs"=df_deltas_11$Partial+df_deltas_11$`No-take`
df_deltas_11_melt=reshape::melt.data.frame(df_deltas_11[,-2], id = 1)

ggplot(df_deltas_11_melt, aes(x=Country, y=value, fill=variable)) + geom_bar(stat='identity', position='dodge')


##Create a barplot with both % MPA extent and % MPA growth (you need to source "df_scat_melt" from the Fig. 4 script)
#load("df_scat_melt.Rdata")

df_max_melt$variable <- recode_factor(df_max_melt$variable, Notake = "No-take", AllMPAs = "All MPAs")
load("df_scat_melt.Rdata")
df_scat_melt$variable <- recode_factor(df_scat_melt$variable, perc_NT_MPA = "No-take", perc_all_MPA = "All MPAs")
df_max_region=df_scat_melt[df_scat_melt$Country == "Whole Region",]
df_max_melt=rbind(df_max_melt,df_max_region)


df_deltas_11_melt$"Type"=rep("% MPA expansion from 2010 to 2022", nrow(df_deltas_11_melt))
#df_scat_melt$"Type"=rep("% MPA extent in 2022", nrow(df_scat_melt))
df_max_melt$"Type"=rep("% MPA extent in 2022", nrow(df_max_melt))


df_melt_MPAs=rbind(df_deltas_11_melt, df_max_melt)
#
df_melt_MPAs$Country=factor(df_melt_MPAs$Country, levels=c("Whole Region", "Mexico", "France", "Guatemala",
                                                           "Honduras", "El Salvador", "Nicaragua", "Costa Rica", "Panama", "Colombia",
                                                           "Ecuador", "Peru", "Chile"))

df_melt_MPAs$variable <- recode_factor(df_melt_MPAs$variable, "No-take" = "No-take MPAs", "All MPAs" = "All MPAs")

df_melt_MPAs$Country <- recode_factor(df_melt_MPAs$Country, "Whole Region" = "Whole region")

#row_France=df_melt_MPAs[df_melt_MPAs$Country == "France",]
#row_France[,4]=c("% MPA extent in 2022", "% MPA extent in 2022")
#df_melt_MPAs=rbind(df_melt_MPAs, row_France)
#df_melt_MPAs = df_melt_MPAs[!df_melt_MPAs$Country == "ABNJ",]

gg_MPA_exp_ext=ggplot(df_melt_MPAs, aes(x=Country, y=value, fill=variable)) + 
  geom_bar(stat='identity', position='dodge', color ="black") +
  facet_wrap(~Type, nrow = 2) +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
#  scale_y_continuous(limits=c(0,40)) + 
  theme_bw() + 
  theme(legend.title = element_blank(),
        strip.text = element_text(size = 12)) + 
  ylab(NULL) + xlab(NULL)

gg_MPA_exp_ext

ggsave("MPA expansion and extent.jpeg", gg_MPA_exp_ext, width = 400*0.70, height = 300*0.65, units = "mm")


##--------------------- Turn this into a lolipop graph 

#Combine MPA type and type (extent vs expansion)
df_melt_MPAs$Combined_type=c(rep("No-take MPA expansion 2010-2022",13),
                             rep("All MPA expansion 2010-2022",13),
                             rep("No-take MPA extent 2022",12),
                             rep("All MPA extent 2022",12),
                             rep("No-take MPA extent 2022",1),
                             rep("All MPA extent 2022",1))

#------------
df_melt_MPAs_NT_Exp = df_melt_MPAs[df_melt_MPAs$Combined_type == "No-take MPA expansion 2010-2022",]
df_melt_MPAs_NT_Exp$Perc_MPAs_NT_Exp=paste(round(df_melt_MPAs_NT_Exp$value,1), "%", sep = "")

df_scat_melt2_P = df_scat_melt2[df_scat_melt2$variable == "Perc_ISRA_proc_P",]
df_scat_melt2_P$Perc_ISRA_ov_P=paste(round(df_scat_melt2_P$value,1), "%", sep = "")
#-------------

library(countrycode)
library(ggplot2)
library(ggflags)
library(dplyr)

#Get the flag codes
df_melt_MPAs_NT_Exp$iso2 <- countrycode(df_melt_MPAs_NT_Exp$Country, "country.name", "iso2c")

df_melt_MPAs_NT_Exp = df_melt_MPAs_NT_Exp %>% 
  mutate(code = tolower(iso2))  #Convert to lower case for the geom_flag function 

df_melt_MPAs$Country=factor(df_melt_MPAs$Country, levels=rev(c("Whole region", "Guatemala", "France", "El Salvador", "Nicaragua",
                                                                         "Peru", "Colombia", "Ecuador", "Costa Rica", "Honduras", "Panama","Mexico", "Chile")))

df_melt_MPAs$Combined_type=factor(df_melt_MPAs$Combined_type, levels=c("No-take MPA expansion 2010-2022", "No-take MPA extent 2022", "All MPA expansion 2010-2022",
                                                                       "All MPA extent 2022"))
unique(df_melt_MPAs$Combined_type)

#df_melt_MPAs$Country=recode_factor(df_melt_MPAs$Country, "Whole region"="Central and South American Pacific")

a_4=ggplot(df_melt_MPAs, aes(x=value, y=Country)) +
  geom_line(aes(group = Country)) +
  #geom_vline(xintercept = c(10,30), color="darkgrey", linetype="dashed") + 
  geom_point(aes(color=Combined_type), size = 6) +
#  geom_text(data = df_scat_melt2_NT, aes(y = Country, x=value-4, label = Perc_ISRA_ov_NT), color="#69b3a2") + 
#  geom_text(data = df_scat_melt2_P, aes(y = Country, x=value+4, label = Perc_ISRA_ov_P), color = "#404080") + 
#  scale_color_manual(values=c("#69b3a2","#404080"), labels=c("No-take MPAs", "All MPAs")) +
  scale_color_manual(values=c("brown3", "brown4", "deepskyblue3", "deepskyblue4")) +
  scale_x_continuous(limits=c(-6,55), breaks = scales::breaks_pretty()) + 
  geom_flag(data= df_melt_MPAs_NT_Exp, x = -5, aes(country = code), size = 10)  +
  theme_bw() +
  theme(legend.position = c(0.75, 0.9),
        legend.title = element_blank(),
        legend.text = element_text(size = 14, color="black"),
       # panel.grid.major = element_blank(), 
    #    panel.grid.minor = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 14, color="black"),
        axis.text.x = element_text(color="black", size = 12),
        axis.title = element_text(size = 14)) +
  xlab(expression("% Waters covered by MPA(s)")) + ggtitle(NULL)
a_4


#Combine it with the area chart from above 

gg4 = ggpubr::ggarrange(gg3, 
                        ggpubr::ggarrange(NULL,a_4, NULL, ncol = 3, widths=c(0.1,0.7,0.2)), 
                        labels=c("","C"),
                        font.label = list(size = 24, face = "bold", color ="black"),
                        nrow=2, heights = c(1,1.5))
gg4

ggsave("Figure 2 - Area plots of trends in cumulative marine area protected with barplots on side and scatterplot of MPA extent and expansion_C_BC.jpeg", 
       gg4, width = 500*0.7, height = 350*0.7*0.6*2.5, units = "mm") 


