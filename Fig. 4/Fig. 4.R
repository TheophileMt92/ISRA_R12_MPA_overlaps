##---------------------------------------------------------------------------------------------------------------------------------##
##----------------------          Results of Shark Robustness Management Index for Central and         ----------------------------##
##----------------------               South American Pacific MPAs as obtained from                    ----------------------------##
##----------------------                     Generalised Linear Mixed-effects Models.                  ----------------------------##
##---------------------------------------------------------------------------------------------------------------------------------##
##---------- Théophile L. Mouton 16/01/2023
##---------- IUCN SSC SSG 

library(tidyverse)
library(sf)
library(lme4)
library(sjPlot)
library(performance)
library(MuMIn)
library(visreg)
library(dplyr)
library(visreg)
library(countrycode)
library(ggflags)
library(viridis)
library(grid)

load(here::here("Fig. 4","df_join.Rdata"))

#------------------------------ GLMM with family poisson 
#GLMM of the Max score = 3 
mod3 = glmer(Score3 ~ STATUS_YR + AREA + (1 | Country), data = df_join, family = poisson(link = "log"))

#GLMM of the Max score = 4 
mod4 = glmer(Score4 ~ STATUS_YR + AREA + (1 | Country), data = df_join, family = poisson(link = "log"))

#GLMM of the Max score = 5 
mod5 = glmer(Score5 ~ STATUS_YR + AREA + (1 | Country), data = df_join, family = poisson(link = "log"))

#GLMM of the Max score = 6 
mod6 = glmer(Score6 ~ STATUS_YR + AREA + (1 | Country), data = df_join, family = poisson(link = "log"))

#GLMM of the Max score = 7
mod7 = glmer(`Total score` ~ STATUS_YR + AREA + (1 | Country), data = df_join, family = poisson(link = "log"))

#Table of marginal and conditional r-squared
RS=rbind(performance::model_performance(mod3)[4:5],
         performance::model_performance(mod4)[4:5],
         performance::model_performance(mod5)[4:5],
         performance::model_performance(mod6)[4:5],
         performance::model_performance(mod7)[4:5])
RS$Group=c("3","4","5","6","7")

#Model comparison with AICc 
options(na.action = "na.fail")

dd <- dredge(mod3)
dd

dd <- dredge(mod4)
dd

dd <- dredge(mod5)
dd

dd <- dredge(mod6)
dd

dd <- dredge(mod7)
dd

#Marginal effects Marine protected area size 
p.3 <- visreg(mod3,  scale="response", "AREA", line.par = list(col = "red"), plot=FALSE)
p.4 <- visreg(mod4,  scale="response", "AREA", plot=FALSE)
p.5 <- visreg(mod5,  scale='response', "AREA", plot = FALSE)
p.6 <- visreg(mod6,  scale='response', "AREA", plot = FALSE)
p.7 <- visreg(mod7,  scale='response', "AREA", plot = FALSE)


dplyr::bind_rows(
  dplyr::mutate(p.3$fit, plt = "P3"),
  dplyr::mutate(p.4$fit, plt = "P4"),
  dplyr::mutate(p.5$fit, plt = "P5"),
  dplyr::mutate(p.6$fit, plt = "P6"),
  dplyr::mutate(p.7$fit, plt = "P7")
  
) -> fits


p1=ggplot() +
  scale_y_continuous(limits=c(0,4), expand=c(0,0)) +
  geom_ribbon(
    data = fits, 
    aes(AREA, ymin=visregLwr, ymax=visregUpr, group=plt), fill="white"
  ) +
  geom_line(data = fits, aes(AREA, visregFit, group=plt, color=plt), size=1.2) +
  paletteer::scale_colour_paletteer_d("rtist::vangogh", labels=c("4","6","8"), name="Max score", direction = -1) +
  theme_classic() +
  theme(legend.position = "none") +
  xlab("MPA size") + ylab("Shark management robustness index")

p1

#Marginal effects Marine protected area year of establishment 
p.3 <- visreg(mod3,  scale="response", "STATUS_YR", line.par = list(col = "red"), plot=FALSE)
p.4 <- visreg(mod4,  scale='response', "STATUS_YR", plot = FALSE)
p.5 <- visreg(mod5,  scale='response', "STATUS_YR", plot = FALSE)
p.6 <- visreg(mod6,  scale='response', "STATUS_YR", plot = FALSE)
p.7 <- visreg(mod7,  scale='response', "STATUS_YR", plot = FALSE)

dplyr::bind_rows(
  dplyr::mutate(p.3$fit, plt = "P3"),
  dplyr::mutate(p.4$fit, plt = "P4"),
  dplyr::mutate(p.5$fit, plt = "P5"),
  dplyr::mutate(p.6$fit, plt = "P6"),
  dplyr::mutate(p.7$fit, plt = "P7")
  
) -> fits

p2=ggplot() +
  scale_y_continuous(limits=c(0,4), expand=c(0,0)) +
  geom_ribbon(
    data = fits, 
    aes(STATUS_YR, ymin=visregLwr, ymax=visregUpr, group=plt), fill="white"
  ) +
  geom_line(data = fits, aes(STATUS_YR, visregFit, group=plt, color=plt),size=1.2) +
  paletteer::scale_colour_paletteer_d("rtist::vangogh", labels=c("4","6","8"), name="Max score", direction = -1) +
  theme_classic() +
  theme(legend.position = "none") +
  xlab("Year of MPA establishment") + ylab("Shark management robustness index")

p2

#Plot predicted random effects 
re3 = plot_model(mod3, type = "re")
re3 + xlab("Shark robustness management index") 

re4 = plot_model(mod4, type = "re")
re4 + xlab("Shark robustness management index") 

re5 = plot_model(mod5, type = "re")
re5 + xlab("Shark robustness management index")

re6 = plot_model(mod6, type = "re")
re6 + xlab("Shark robustness management index")

re7 = plot_model(mod7, type = "re")
re7 + xlab("Shark robustness management index") 

re3_df = as.data.frame(re3$data)
re3_df$Group=rep("P3", nrow(re3_df))

re4_df = as.data.frame(re4$data)
re4_df$Group=rep("P4", nrow(re4_df))

re5_df = as.data.frame(re5$data)
re5_df$Group=rep("P5", nrow(re5_df))

re6_df = as.data.frame(re6$data)
re6_df$Group=rep("P6", nrow(re6_df))

re7_df = as.data.frame(re7$data)
re7_df = re7_df[order(re7_df$estimate),] #Order the dataframe by Max score = 7
re7_df$Group=rep("P7", nrow(re7_df))

re_all=rbind.data.frame(re3_df, re4_df, re5_df, re6_df, re7_df)

re_all$term = factor(re_all$term, levels=re7_df$term)
re_all$Group = as.factor(re_all$Group)

#Add flags 
re_all$iso2 <- countrycode(re_all$term, "country.name", "iso2c")
re_all = re_all %>% 
  mutate(code = tolower(iso2))  #Convert to lower case for the geom_flag function 

p_re = ggplot(re_all, aes(x=term, y=estimate, colour=Group)) +
  geom_point(size = 4, position = position_dodge(width=0.75)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.0, position = position_dodge(width=0.75)) +
  scale_y_continuous(limits=c(-0.3,3)) + 
  geom_flag(data= re_all, y = -0.2, aes(country = code), size = 10)  +
  theme_classic() +
  paletteer::scale_colour_paletteer_d("rtist::vangogh", labels=c("3","4","5","6","7"), name="Max score", direction = -1) +
  theme(axis.text.y = element_text(size=11, color="black")) +
  #theme(axis.text.x = element_text(angle = -30, vjust = 0.25, hjust=0.15)) +
  xlab(NULL) + ylab("Shark robustness management index") + 
  coord_flip()
p_re


#Barplot of R-squared
gRS=ggplot(reshape::melt(RS), aes(x=Group, y=value, fill=forcats::fct_rev(variable))) +
  geom_bar(stat = "identity", position = position_dodge(0.6), width=0.5) +
  scale_fill_manual(values=c("darkmagenta", "#333333"), labels=c("R2m", "R2c"), name="") +
  theme_bw()+
  theme(legend.position = "top",
          panel.background = element_rect(fill='white'),
          plot.background = element_rect(fill='transparent', color=NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.background = element_blank(),
          legend.box.background = element_blank()) +
  scale_y_continuous(limits=c(0, 0.4)) +
  ylab("R-squared") + xlab("")
gRS  

#Add the barplot as inset to the scatterplot of predicted random effects
RE_in = p_re +
  ggmap::inset(ggplotGrob(gRS), xmin = 0.2, xmax = 4, ymin = 1.5, ymax = 3)
RE_in

# Grid of all plots 
GG = ggpubr::ggarrange(RE_in,ggpubr::ggarrange(p2, p1, ncol=1, labels = c("B", "C")), widths = c(1, 0.65),
                       common.legend = T, legend="right", labels=c("A",""))
GG

ggsave("Fig. 4.jpeg", GG, width = 280, height = 200, units = "mm")

