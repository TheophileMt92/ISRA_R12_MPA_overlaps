
# Shortfalls in the protection of Important Shark and Ray Areas undermine shark conservation efforts in the Central and South American Pacific


This folder contains R code, data and outputs for all analyses in this paper. There are 4 main folders, each of which are given their figure names (Fig. 1, Fig. 2 etc.), to reproduce the figures in the paper.


## Authors

Théophile L. Mouton, Adriana Gonzalez-Pestana, Christoph A. Rohner, Ryan Charles, Emiliano García-Rodríguez, Peter M. Kyne, Amanda Batlle-Morera, Giuseppe
Notarbartolo di Sciara, Asia O. Armstrong, Enzo Acuñah, Joanna Alfaro-Shigueto, Randall Arauz, Cristopher G. Avalos-Castillo, Ely Augustinus, Sandra Bessudo,
Enrique Barraza, Carlos Bustamantea, Elpis J. Chávez, Eduardo Ramon Espinoza, Mario Espinozaa, Ana Hacohen-Domené, Alex R. Hearn, Grettel M. Hernández,
Felipe Galván-Magaña, José A. Gonzalez-Leiva, James T. Ketchum, Felipe Ladino, Frida Lara-Lizardi, Jorge Manuel Morales-Saldañaa, Naití Morales Serrano, Jeffry
Madrigal-Mesén, Paola A. Mejía-Fallaa, Andrés F. Naviaa, Gabriela M. Ochoa, Marta D. Palacios, César R. Peñaherrera-Palma, Francisco Polanco-Vásquez,
Yehudi Rodríguez-Arriatti, Luz E. Saldaña-Ruiz, Oscar Sosa-Nishizaki, Javier Tovar-Ávilaap, Ángel J. Vegaap, Ximena Velez-Zuazo, Melany Villate-Morenoas, 
Ilena Zanellaat, Rima W. Jabado


## R packages 

To run the R code contained in this repository, 21 packages are needed to be installed, use the code below to install them:  

```bash
if (!requireNamespace(tidyverse, quietly = TRUE)) {install.packages(tidyverse)}
if (!requireNamespace(sf, quietly = TRUE)) {install.packages(sf)}
if (!requireNamespace(ggplot2, quietly = TRUE)) {install.packages(ggplot2)}
if (!requireNamespace(lwgeom, quietly = TRUE)) {install.packages(lwgeom)}
if (!requireNamespace(data.table, quietly = TRUE)) {install.packages(data.table)}
if (!requireNamespace(lubridate, quietly = TRUE)) {install.packages(lubridate)}
if (!requireNamespace(viridis, quietly = TRUE)) {install.packages(viridis)}
if (!requireNamespace(geomtextpath, quietly = TRUE)) {install.packages(geomtextpath)}
if (!requireNamespace(ggpubr, quietly = TRUE)) {install.packages(ggpubr)}
if (!requireNamespace(data.table, quietly = TRUE)) {install.packages(data.table)}
if (!requireNamespace(countrycode, quietly = TRUE)) {install.packages(countrycode)}
if (!requireNamespace(remotes, quietly = TRUE)) {install.packages(remotes)}
if (!requireNamespace(ggflags, quietly = TRUE)) {remotes::install_github('rensa/ggflags')}
if (!requireNamespace(dplyr, quietly = TRUE)) {install.packages(dplyr)}
if (!requireNamespace(lme4, quietly = TRUE)) {install.packages(lme4)}
if (!requireNamespace(sjPlot, quietly = TRUE)) {install.packages(sjPlot)}
if (!requireNamespace(performance, quietly = TRUE)) {install.packages(performance)}
if (!requireNamespace(MuMIn, quietly = TRUE)) {install.packages(MuMIn)}
if (!requireNamespace(visreg, quietly = TRUE)) {install.packages(visreg)}
if (!requireNamespace(grid, quietly = TRUE)) {install.packages(grid)}
if (!requireNamespace(ade4, quietly = TRUE)) {install.packages(ade4)}
if (!requireNamespace(ggrepel, quietly = TRUE)) {install.packages(ggrepel)}
```


## Documentation

The R scripts are as follows: 

1. Fig. 1.R - Area charts of temporal trends (1935 - 2023) in marine protected area  (A) numbers and (B) size in the Central and South American Pacific. 

2. Fig. 2.R - Scatterplots of marine protected area (A) expansion 2010-2023 (B) extent in 2023 and (C) overlaps with ISRAS in the Central and South American Pacific. 

3. Fig. 4.R - Results of Shark Robustness Management Index for Central and South American Pacific MPAs as obtained from Generalised Linear Mixed-effects Models.

4. Fig. 5.R - Principal Component Analysis of the eleven indicators of protection.

