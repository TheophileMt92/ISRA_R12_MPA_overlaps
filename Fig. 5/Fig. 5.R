##--------------------------------------------------------------------------------------------------------------------##
##----------------------          Principal Component Analysis (PCA) of the               ----------------------------## 
##----------------------                eleven indicators of protection                   ----------------------------##
##--------------------------------------------------------------------------------------------------------------------##
##---------- Théophile L. Mouton 16/01/2023
##---------- IUCN SSC SSG 

#Load necessary packages 
library(ade4)
library(countrycode)
library(ggflags)
library(dplyr)
library(ggrepel)

##------ Load datasets for PCA 
load(here::here("Fig. 5", "DF_melt.Rdata"))
load(here::here("Fig. 5","df_scat_melt2.Rdata"))
load(here::here("Fig. 5","re_all.Rdata"))

#Create the dataframe for the PCA
df1=DF_melt[,c(1,7,4)]
colnames(df1)=c("Country", "Mod", "value")
df2=df_scat_melt2[,c(1,2,3)]
colnames(df2)=c("Country", "Mod", "value")
df3=re_all[,c(1,5,13)]
colnames(df3)=c("value", "Country", "Mod")
df3=rbind.data.frame(df1, df2, df3)

#Cast the dataframe as this is the appropriate format  
d1=reshape::cast(df3, Country ~ Mod , value="value")
d1=d1[-12,] #Exclude France

rownames(d1)=d1$Country
pca=ade4::dudi.pca(d1[,-1],  col.w = c(c(1/4,1/4,1/4,1/4), c(1/2,1/2), c(1/5,1/5,1/5,1/5,1/5)),
         center = TRUE, scale = TRUE, 
         scannf = TRUE, nf = 2)
2
100 * pca$eig/sum(pca$eig)

#Prepare the dataframe for first PCA plot
groups=c(rep("MPA extent & expansion",4),
rep("MPA overlap ISRAs", 2),
rep("MPA governance", 5))

pca_df=pca$co #the column coordinates 
pca_df$groups=groups

# calculate group centroid locations for the PCA arrows
centroids <- aggregate(cbind(Comp1,Comp2)~groups,data=pca_df,mean)
gg <- merge(pca_df,centroids,by="groups",suffixes=c("",".centroid"))

#Make the plot 
PCA_1=ggplot(gg) + 
  geom_hline(yintercept=0, linetype="dashed", color="darkgrey") +
  geom_vline(xintercept=0, linetype="dashed", color="darkgrey") +
  geomtextpath::geom_textsegment(data=centroids, aes(x=0, y=0, xend = Comp1, yend = Comp2, label=groups),
                                 arrow = NULL) +
  geom_point(aes(x=Comp1, y=Comp2, color=groups), size = 4) +
  paletteer::scale_color_paletteer_d("colorBlindness::paletteMartin") +
  geom_point(data=centroids, aes(x=Comp1, y=Comp2, color=groups), size=0) +
  geom_segment(aes(x=Comp1.centroid, y=Comp2.centroid, xend=Comp1, yend=Comp2, color=groups)) +
  
  #geom_label_repel(data=centroids, aes(x=Comp1, y=Comp2, label = groups)) +
  scale_x_continuous(limits = c(-1,1)) +
  scale_y_continuous(limits = c(-1,1)) +
  theme_classic() +
  theme(legend.position = "none", #c(0.2, 0.85)
        legend.title = element_blank()) +
  xlab("PC1 (57.5%)") + ylab("PC2 (27.7%)")
PCA_1

# Prepare the dataframe for the first PCA plot 
pca_li=pca$li #the row coordinates i.e. the principal components 
pca_li$Country=rownames(pca_li)

#Country flags 
#Get the flag codes
pca_li$iso2 <- countrycode(pca_li$Country, "country.name", "iso2c")
pca_li = pca_li %>% 
  mutate(code = tolower(iso2))  #Convert to lower case for the geom_flag function 

rownames(pca$li)=c("CHL", "CRI", "PAN", "MEX", "ECU", "NIC", "COL", "HND", "PER", "SLV", "GTM")

PCA_2=ggplot(pca_li, aes(x=Axis1, y=Axis2)) + 
  geom_hline(yintercept=0, linetype="dashed", color="darkgrey") +
  geom_vline(xintercept=0, linetype="dashed", color="darkgrey") +
  geom_point() +
  geom_flag(data= pca_li, aes(country = code), size = 10)  +
  geom_text_repel(aes(label = rownames(pca$li))) + 
  theme_classic() + 
  xlab("PC1 (57.5%)") + ylab("PC2 (27.7%)")

PCA_2

#Make a grid of all plots 
PCA_grid=ggpubr::ggarrange(PCA_1, PCA_2, nrow=1, labels=c("A","B"))

#Save the grid as .jpeg file 
ggsave("PCAs of countries_1812_23.jpeg", PCA_grid, width=300, height = 130, units = "mm")

