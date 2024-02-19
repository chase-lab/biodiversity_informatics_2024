######################################################
# Functional diversity Quantification I
# Species-pairwise functional distances sensu Anne Chao et al
# Code: Paola Barajas: paola.barajas@idiv.de
######################################################

# First make sure you have installed the following packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)
library(cluster)
library(iNEXT.3D)

setwd("C:/_Teaching/Lecture_Jan_Msc/Class_2023")

# Load data
traits <- read.csv("Trait_imputed_144.csv") # Species by trait matrix
Abun_p <- read.csv("Abun_p.csv")            # Species by site matrix

min_ind <- 20 # minimum amount of individuals

Abun_p <- tibble::column_to_rownames(Abun_p, var = "Scientific_name") # columns are plots
Abun_p <- Abun_p[,colSums(Abun_p) > min_ind]  

# DIVERSITY METRICS ----

Cmax <- apply(Abun_p, 2, function(x)
  iNEXT.3D:::Coverage(x, 'abundance', 2*sum(x))) %>% min %>% round(., 4) # we use maximum coverage

# - Taxonomic diversity
TD_est <- iNEXT.3D::estimate3D(data = Abun_p, diversity = 'TD', q = c(0, 1, 2), datatype = 'abundance', base = 'coverage',
                     level = Cmax, nboot = 0)

plot(TD_est$Order.q, TD_est$qD)

# - Functional diversity
mask   <- as.data.frame(rownames(Abun_p))
mask$Scientific_name <- mask$`rownames(Abun_p)`
traits <- left_join(mask, traits, by = "Scientific_name")
rownames(traits) <- traits$Scientific_name # arrange only traits in columns. Species names as rownames
traits$Scientific_name    <- NULL
traits$`rownames(Abun_p)` <- NULL

for (i in 1:ncol(traits)) {
  if (class(traits[,i]) == "character") traits[,i] <- factor(traits[,i], levels = unique(traits[,i]))
}

distM     <- cluster::daisy(x = traits, metric = "gower") %>% as.matrix()
distM_seq <- seq(min(distM[distM>0]), max(distM), length.out=16) 

FD_est <- iNEXT.3D::estimate3D(data = Abun_p, diversity = 'FD', q = c(0, 1, 2), datatype = 'abundance', base = 'coverage',
                     level = Cmax, nboot = 0, FDdistM = distM, FDtype = "tau_values", FDtau = distM_seq)

FD_est$Order.q <- as.character(FD_est$Order.q)

FD_est_tmp <- dplyr::summarize(group_by(FD_est, threshold, Order.q),
                               qFD = mean(qFD)) # aggregates data by: mean value 

# Plot the the Tau profile, that is, Functional diversity (qFD) as a function of level of threshold distinctiveness 
# for q = 0, 1, and 2. 

ggplot(FD_est_tmp, aes( x=threshold, y=qFD) ) + geom_point()  + geom_line(aes(colour=Order.q)) +
  xlab("Tau (functional threshold of distinctiveness)") + ylab("Functional Diversity") +
  theme_minimal()

#_____________________________________________________________________________
# Task: Let's talk about what is the ecological meaning of the graph.

# What are the different Hill number (q) telling us?
# Why is q=0 values higher than q=2
# Why functional diversity decreases with increasing Tau value?
