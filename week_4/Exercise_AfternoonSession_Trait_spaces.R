######################################################
# Estimations of Functional Trait Diversity II
# Trait-value-based approach sensu Blonder et al 
# Code: Paola Barajas: paola.barajas@idiv.de
# FromM. Paola Barajas Barbosa et al (2023) Assembly of functional diversity in an oceanic island flora
######################################################

library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(ggExtra)

setwd("C:/_Teaching/Lecture_Jan_Msc/Class_2023")

# Tenerife data
data_Tenerife <- read.csv("C:/_idiv/_Teaching/Lecture_Jan_Msc/Class_2023/Barajasetal2023_data_22052023.csv")

data_Tenerife %>% group_by(Biogeo_status) %>% summarise(n = n())
data_Tenerife %>% group_by(Ende_status) %>% summarise(n = n())

# # PCA 
PCA             <- prcomp(data_Tenerife[, c("Leaf_area","LMA","Leaf_N","Leaf_th","LDMC","Seed_mass","Stem_density","Height")] )
summary(PCA)

PCAvalues       <- data.frame(Species = data_Tenerife$Species1, Biogeo_status = data_Tenerife$Biogeo_status, 
                              Ende_status = data_Tenerife$Ende_status, PCA$x)
PCAvalues$PC1   <- PCAvalues$PC1*-1 ; PCAvalues$PC2 <- PCAvalues$PC2*-1 # for visualization purposes

PCAloadings     <- data.frame(Variables = rownames(PCA$rotation), PCA$rotation)  
PCAloadings$PC1 <- PCAloadings$PC1*-1 ; PCAloadings$PC2 <- PCAloadings$PC2*-1

# # 
# 1. Plots Figure 1  
Tenerife <- PCAvalues %>% ggplot(aes(PC1,  PC2), size = 1)+ # Plot Tenerife PCA 
  stat_density_2d(geom = "polygon", contour = TRUE, aes(fill = after_stat(level)),  
                  colour = "gray", bins = 34) +
  
  scale_fill_distiller(palette = "BuGn", direction = 1) +
  geom_jitter(alpha=0.6,  size = 2  , colour = "turquoise4") +     #  Display the points 
  geom_text(data = PCAloadings, aes(x = PC1*4.7, y = PC2*4.7, label = Variables), size = 2.3) +
  geom_segment(data = PCAloadings, size = 0.2,    # Plots the loadings, i.e., traits 
               aes(x = 0, xend = PC1*4.2, y = 0, yend = PC2*4.2),
               arrow = arrow(length = unit(0.1, "cm")),colour = "black")   + 
  xlab(" n = 348") + ylab("PC2 (25%)")   +
  xlim(-5 , 5) + ylim(-5, 4) + theme_minimal()
Tenerife 
  
# Plot each group separately
NNE <- dplyr::filter(PCAvalues, Biogeo_status == "NNE")
NNE<- NNE  %>% ggplot(aes(PC1,  PC2))+ 
  stat_density_2d(geom = "polygon", contour = TRUE, aes(fill = after_stat(level)), colour = "gray", bins = 10) +
  scale_fill_distiller(palette = "Greys", direction = 1) +
  geom_jitter(alpha=0.5,  size = 2  , colour = "black") +    
  xlim(-5 , 5) + ylim(-5, 4) +
  xlab("n = 54") + ylab("")   + theme_minimal()
NNE

TE <- dplyr::filter(PCAvalues, Biogeo_status == "TE")
TE <-  TE  %>% ggplot(aes(PC1,  PC2))+  
  stat_density_2d(geom = "polygon", contour = TRUE, aes(fill = after_stat(level)), colour = "gray", bins = 15) +
  scale_fill_distiller(palette = "Greys", direction = 1) +
  geom_jitter(alpha=0.5,  size =2   , colour = "dodgerblue3") +     
  xlim(-5 , 5) + ylim(-5, 4) +
  xlab("PC1 (30%) n = 85") + ylab("PC2 (25%)")   + theme_minimal()
TE

CLADO <- dplyr::filter(PCAvalues, Ende_status == "Cla")
CLADO <- CLADO %>% ggplot(aes(PC1,  PC2))+ 
  stat_density_2d(geom = "polygon", contour = TRUE, aes(fill = after_stat(level)), colour = "gray", bins = 20) +
  scale_fill_distiller(palette = "Greys", direction = 1) +
  geom_jitter(alpha=0.5, size = 2  , colour = "mediumorchid2") +  
  xlim(-5 , 5) + ylim(-5, 4) +
  xlab("PC1 (30%) n = 195") + ylab("")   +theme_minimal()
CLADO

Ten<- ggExtra:: ggMarginal(TENERIFE, type = "density", fill="transparent", size = 15)
ns <- ggExtra:: ggMarginal(NNE, type = "density", fill="transparent", size = 15) 
sie<- ggExtra:: ggMarginal(TE, type = "density", fill="transparent", size = 15)
cla<- ggExtra:: ggMarginal(CLADO, type = "density", fill="transparent", size = 15)

ggpubr::ggarrange(Tenerife, ns,sie, cla, ncol =2, nrow = 2) 

# FIN