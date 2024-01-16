##---------------------------------------------------------------------------------------------------------------------------------##
##----------------------          Scatterplots of marine protected area (A) expansion 2010-2023        ----------------------------##
##----------------------               (B) extent in 2023 and (C) overlaps with ISRAS                  ----------------------------##
##----------------------                     in the Central and South American Pacific                 ----------------------------##
##---------------------------------------------------------------------------------------------------------------------------------##
##---------- Théophile L. Mouton 16/01/2023
##---------- IUCN SSC SSG 

library(countrycode)
library(ggplot2)
library(ggflags)
library(dplyr)

#Load dataset 
load(here::here("Fig. 2","DF_melt.Rdata"))

#Get the flag codes and add them to the dataframe
DF_melt$iso2 <- countrycode(DF_melt$Country, "country.name", "iso2c")

#Convert to lower case for the geom_flag function 
DF_melt = DF_melt %>% 
  mutate(code = tolower(iso2))  

DF_melt$Country=factor(DF_melt$Country, levels=rev(c("Guatemala", "El Salvador", "Peru", "Honduras", "Colombia", "Nicaragua", "Ecuador", "Mexico", "Panama", "Costa Rica", "Chile")))

DF_melt$Combined_type=factor(paste0(DF_melt$variable, "_",DF_melt$Period))
DF_melt$Combined_type <- recode_factor(DF_melt$Combined_type, 
                                       "Perc_MPA_1935_2022" = "All MPA extent 2022", 
                                       "Perc_MPA_2010_2022" = "All MPA expansion 2010-2022", 
                                       "Perc_NT_1935_2022" = "No-take MPA extent 2022", 
                                       "Perc_NT_2010_2022" = "No-take MPA expansion 2010-2022")

DF_melt$Combined_type=factor(DF_melt$Combined_type, levels=c("No-take MPA expansion 2010-2022", 
                                                             "No-take MPA extent 2022", 
                                                             "All MPA expansion 2010-2022",
                                                             "All MPA extent 2022"))

##------------------------ Make the first scatterplot for MPA expansion  
DF_melt_1=DF_melt[DF_melt$Combined_type %in% c("No-take MPA expansion 2010-2022", "All MPA expansion 2010-2022"),]
DF_melt_1$Country=factor(DF_melt_1$Country, levels=c("Panama","Chile","Colombia","Mexico","Costa Rica","Honduras","Peru",       
                                                     "Ecuador","Nicaragua","El Salvador","Guatemala"))

#Put the labels on the dots
DF_melt_1_NT = DF_melt_1[DF_melt_1$variable == "Perc_NT",]
DF_melt_1_NT$Perc_NT=paste(round(DF_melt_1_NT$value,1), "%", sep = "")

DF_melt_1_P = DF_melt_1[DF_melt_1$variable == "Perc_MPA",]
DF_melt_1_P$Perc_P=paste(round(DF_melt_1_P$value,1), "%", sep = "")

#Change the 0s that in fact <0.1% manually
#Peru 
DF_melt_1_NT[10,8]= "<0.1%"
#Honduras
DF_melt_1_NT[6,8]= "<0.1%"
#Costa Rica
DF_melt_1_NT[3,8]= "<0.1%"

#Panama: an MPA designated in 2023 was added manually 
DF_melt_1[20,4] = DF_melt_1[20,4] + 23.86
DF_melt_1_P[9,4] =  48.6 
DF_melt_1_P[9,8] = "48.6%" 

#Make the plot 
a_1=ggplot(DF_melt_1, aes(x=value, y=Country)) +
  geom_line(aes(group = Country)) +
  #geom_vline(xintercept = c(10,30), color="darkgrey", linetype="dashed") + 
  geom_point(aes(color=Combined_type), size = 6) +
  geom_text(data = DF_melt_1_NT, aes(y = Country, x=value-5, label = Perc_NT), color="#69b3a2") + 
  geom_text(data = DF_melt_1_P, aes(y = Country, x=value+5, label = Perc_P), color = "#404080") +
  scale_color_manual(values=c("#69b3a2","#404080"), labels=c("No-take MPAs", "All MPAs")) +
  scale_x_continuous(limits=c(-12,60), breaks = scales::breaks_pretty()) + 
  geom_flag(data= DF_melt, x = -11, aes(country = code), size = 10)  +
  #theme_bw() +
  theme_classic() +
  theme(legend.position = c(0.75, 0.9),
        legend.title = element_blank(),
        legend.text = element_text(size = 11, color="black"),
        # panel.grid.major = element_blank(), 
        #    panel.grid.minor = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 14, color="black"),
        axis.text.x = element_text(color="black", size = 12),
        axis.title = element_text(size = 14)) +
  xlab(expression("% EEZ covered by MPAs")) + ggtitle("MPA expansion 2010-2023")
a_1

##------------------------ Make the second scatterplot for MPA extent in 2023 
DF_melt_2=DF_melt[DF_melt$Combined_type %in% c("No-take MPA extent 2022", "All MPA extent 2022"),]

#Put the labels on the dots
DF_melt_2_NT = DF_melt_2[DF_melt_2$variable == "Perc_NT",]
DF_melt_2_NT$Perc_NT=paste(round(DF_melt_2_NT$value,1), "%", sep = "")

DF_melt_2_P = DF_melt_2[DF_melt_2$variable == "Perc_MPA",]
DF_melt_2_P$Perc_P=paste(round(DF_melt_2_P$value,1), "%", sep = "")

DF_melt_2$Country=factor(DF_melt_2$Country, levels=c("Panama","Chile","Colombia","Costa Rica","Mexico","Ecuador","Nicaragua",   
                                                     "Honduras","Peru","Guatemala","El Salvador"))

#Change the 0s that in fact <0.1% manually 
DF_melt_2_NT[10, 8] = "<0.1%"

#Manualy add the Banco Volcan expansion in Panama 
DF_melt_2[20,4] = DF_melt_2[20,4] + 23.86

DF_melt_2_P[9,4] =  50.1 
DF_melt_2_P[9,8] = "50.1%" 

#Make the plot 
a_2=ggplot(DF_melt_2, aes(x=value, y=Country)) +
  geom_line(aes(group = Country)) +
  geom_point(aes(color=Combined_type), size = 6) +
  geom_text(data = DF_melt_2_NT, aes(y = Country, x=value-5, label = Perc_NT), color="#69b3a2") + 
  geom_text(data = DF_melt_2_P, aes(y = Country, x=value+5, label = Perc_P), color = "#404080") +
  scale_color_manual(values=c("#69b3a2","#404080"), labels=c("No-take MPAs", "All MPAs")) +
  scale_x_continuous(limits=c(-12,60), breaks = scales::breaks_pretty()) + 
  geom_flag(data= DF_melt, x = -11, aes(country = code), size = 10)  +
  theme_classic() +
  theme(legend.position = c(0.75, 0.9),
        legend.title = element_blank(),
        legend.text = element_text(size = 11, color="black"),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 14, color="black"),
        axis.text.x = element_text(color="black", size = 12),
        axis.title = element_text(size = 14)) +
  xlab(expression("% EEZ covered by MPAs")) + ggtitle("MPA extent in 2023")
a_2

##------------------------ Third scatterplot for ISRAs overlaps with MPA
load(here::here("Fig. 2","df_scat_melt2.Rdata"))
load(here::here("Fig. 2","df_scat_melt2_NT.Rdata"))
load(here::here("Fig. 2","df_scat_melt2_P.Rdata"))

#Change the 0s that in fact <0.1% manually 
df_scat_melt2_NT[6,4]="<0.1%"

a_3=ggplot(df_scat_melt2, aes(x=value, y=Country)) +
  geom_line(aes(group = Country)) +
  #geom_vline(xintercept = c(10,30), color="darkgrey", linetype="dashed") + 
  geom_point(aes(color=variable), size = 6) +
  geom_text(data = df_scat_melt2_NT, aes(y = Country, x=value-5.5, label = Perc_ISRA_ov_NT), color="#69b3a2") + 
  geom_text(data = df_scat_melt2_P, aes(y = Country, x=value+5.5, label = Perc_ISRA_ov_P), color = "#404080") + 
  scale_color_manual(values=c("#404080", "#69b3a2"), labels=c("All MPAs", "No-take MPAs")) +
  scale_x_continuous(limits=c(-14,75)) + 
  geom_flag(data= df_scat_melt2_NT, x = -13, aes(country = code), size = 10)  +
  theme_classic() +
  guides(color = guide_legend(reverse = TRUE)) +
  theme(legend.position = c(0.85, 0.925),
        legend.title = element_blank(),
        legend.text = element_text(size = 11, color="black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 14, color="black"),
        axis.text.x = element_text(color="black", size = 12),
        axis.title = element_text(size = 14)) +
  xlab(expression("% ISRA covered by MPA(s)")) + ggtitle("MPA overlaps with ISRAs")
a_3

##------------- Grid of all three plots 
grid=ggpubr::ggarrange(a_1,a_2,a_3,align="hv", 
                       nrow=1, labels=c("A", "B", "C"))
#Save the grid 
ggsave(here::here("Fig. 2","Grid of results three scatterplots.jpeg"), grid, width=180*3, height = 160, units = "mm")
