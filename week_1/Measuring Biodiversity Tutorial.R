##################################################################
##       R Practical for MLU Block Course: Modelling            ##
##      Species Distribution and Biodiversity Patterns          ##
##                                                              ##
##                            Week 2                            ##
##                    Tuesday, Jan. 16, 2024                    ##
##                    Measuring Biodiversity                    ##
##                    Dr. Kimberly Thompson                     ##
##                                                              ##
##################################################################

##     R code adapted from:                                     
##       https://jonlefcheck.net/2012/10/23/diversity-as-effective-numbers/



########## clean workspace and load required packages ####################

# clean workspace to improve efficiency: #
rm(list = ls() )
gc() #releases memory


# install packages (only needs to be done once if you don't have these loaded
# already)
install.packages("vegan")
install.packages("ggplot2")
install.packages("reshape2")

# Load the packages
library(vegan)
library(ggplot2)
library(reshape2)


#################################
##                             ##
##       Data Loading          ##
##                             ##
#################################

# The vegan package has some convenient built-in datasets that we can use to
# to illustrate different biodiversity metrics

data(dune) # Vegetation and Environment in Dutch Dune Meadows
str(dune) #a data frame of observations of 30 species at 20 sites

# Note that the sites are the numbered rows with each species in a column 
# and the abundance of each species in each cell --> site x species matrix



#################################
##      SPECIES RICHNESS       ##
#################################

# We can calculate the species richness (S) for each site with the specnumber
# function

vegan :: specnumber(dune)

# Here we also create a new dataframe to store these values
dune.metrics <- data.frame(Site = seq(1:20),
                           S = vegan :: specnumber(dune))


#################################
##         ABUNDANCE           ##
#################################

# The data itself provides the abundance of each species (n), but we can use
# the rowSums function to find the total abundance for each site.

rowSums(dune)

# Add this data to the metrics dataframe
dune.metrics <- data.frame(dune.metrics, N = rowSums(dune))



##########################################
##   Shannon-Wiener Diversity Index     ##
##########################################

# Also called Shannon entropy

# This index can be computed with the diversity function and by specifying 
# 'shannon'

vegan :: diversity(dune, index = "shannon")

# Add this data to the metrics dataframe
dune.metrics <- data.frame(dune.metrics,
                           H = vegan :: diversity(dune, index = "shannon"))


##########################################
##         Shannon's Evenness           ##
##########################################

# Also called Pielou's evenness or Pielou's J

# We know that the formula for Shannon's evennes is H / Hmax, where H
# max is equal to the natural log of the species richness.

# Using this information calculate Shannon's evenness for each site.

dune.metrics$H / log(dune.metrics$S)

# Add this data to the metrics dataframe
dune.metrics <- data.frame(dune.metrics,
                           J = dune.metrics$H / log(dune.metrics$S))


##########################################
##         Gini-Simpson Index           ##
##########################################

# This index can be computed with the diversity function and by specifying 
# 'simpson'


# Remember that the Simpson index is referred to with the letter 'D", and
# the Gini-Simpson index is 1-D. Since it is not good practice to start a 
# column name with a number, we will simply call this column GS.

# Note that althogh this index is referred to as "simpson" it is actually
# calculating the Gini-Simpson index. If you are so inclined, you can confirm
# this by typing ?diversity and reading the details section that comes up
# in the help tab.

# Add this data to the metrics dataframe
dune.metrics <- data.frame(dune.metrics,
                           GS = vegan :: diversity(dune, index = "simpson"))



##########################################
##         Rarefied Richness            ##
##########################################

# We can standardize our observed values of species richness based on the 
# number of individuals sampled at each site.

# First we find the minimum number of individuals found in each plot using
# our total abundance calculation

raremin <- min(dune.metrics$N)

# From looking at the data we can see that site 17 only has 15 individuals.

# Now, we rarefy (i.e., standardize) the richness values for the other sites
# as if they also only had 15 individuals.

vegan :: rarefy(dune, raremin)

# Add this data to the metrics dataframe
dune.metrics <- data.frame(dune.metrics,
                           Sn = vegan :: rarefy(dune, raremin))

# We can also create a rarefaction curve graph where each square shows a site
# number
# Note: to 'rarefy' a larger site, follow the rarefaction curve until the curve
# corresponds with the minimum observed number of species for a given site.

rarecurve(dune, col = "blue")



##########################################
##     Effective Number of Species      ##
##########################################

# What is the benefit of converting the Shannon and Simpson indices to Effective
# Number of Species?

# The following code will demonstrate.


# First, consider the simplest case: a community with S species, all with equal
# abundances A

# For community 1, S = 500 species, and A = 1 individual of each species
# For community 2, S = first 250 species from community 1, and A = 1 individuals
# of each species
community1 = data.frame(t(rep(1,500))); colnames(community1)=paste("sp",1:500)
community2 = data.frame(t(c(rep(1,250)))); colnames(community2)=paste("sp",1:250)


# Calculate the species richness of each community using the `specnumber` 
# function
S1=specnumber(community1); S1
S2=specnumber(community2); S2

# We know this is true because we created these two communities to have
# 500 and 250 species each

# Furthermore, we know that the diversity in community 2 is half that of 
# community 1
S2==0.5*S1


# Now let's incorporate information on species abundances
# We'll begin with the Shannon index.
# Recall that the Shannon index quantifies the uncertainty that any two species
# drawn from the community are different.

# Shannon diversity ranges from 0 (total certainty) to log(S) (total uncertainty)

# Let's again use function `diversity` to calculate Shannon diversity 
# for both communities
H1 = diversity(community1,index="shannon"); H1
H2 = diversity(community2,index="shannon"); H2

# Community 1 has equal representation of all 500 species, so H1 should be 
# equal to log(S)
H1 == log(S1)

# Community 2 has equal represention of only 250 species, so we might expect
# H2 to equal half of H1
H2 == 0.5*H1
# Hmm...let's investigate this further to see why this expectation wasn't
# supported

# Let's calculate Shannon diversity for all levels of species richness 
# from S = 1 to 1000 (this will take a minute)

shannon = matrix(ncol = 2,nrow = 1000)

for(i in 1:1000) {
  community = data.frame(t(rep(1,i))); colnames(community) = paste("sp",1:i)
  shannon[i,1] = i
  shannon[i,2] = diversity(community,index="shannon") }

# And plot the results
plot(shannon[,1], shannon[,2], xlab="Species Richness",
     ylab="Shannon Diversity", main="Shannon")


# As we saw in the lecture, the relationship between richness and Shannon
# diversity is non-linear! This is why H2 is not half of H1

# The concept of 'effective numbers' solves this issue by imposing a linear 
# transformation on Shannon diversity

# Effective numbers are the number of equally abundant species necessary to 
# produce the observed value of diversity

# Effective numbers range from 1 to S, where a value of S would indicate all 
# species are present and in equal abundances

# The conversion of Shannon diversity to effective numbers is exp(H)
HE1 = exp(diversity(community1, index="shannon")); HE1 
# This is what we expect given what we know about Community 1

HE2 = exp(diversity(community2,index = "shannon")); HE2 
# And this is also what we expect

# And now diversity of community 2 is exactly half of the diversity of 
# community 1!
as.character(HE2) == as.character(0.5 * HE1)
# This outcome of effective numbers is known as the 'doubling property'


# Calculate effective numbers of species (from Shannon diversity) for all 
# levels of species richness from 1:1000
shannon_effective = matrix(ncol = 2, nrow = 1000)

for (i in 1:1000) {
  community = data.frame(t(rep(1,i))); colnames(community) = paste("sp",1:i)
  shannon_effective[i,1] = i
  shannon_effective[i,2] = exp(diversity(community,index="shannon")) }

# Plot the result
plot(shannon_effective[,1], shannon_effective[,2], xlab="Species Richness",
     ylab="Effective Numbers of Species", main="Shannon (Effective)")
# This relationship is now linear!



# Let's use another common diversity index: Simpson diversity
# Recall that Gini-Simpson diversity is the probability that two randomly selected
# individuals are drawn from the community are the same

# We will again use the `diversity` function from the vegan package
D1 = diversity(community1,index="simpson"); D1
D2 = diversity(community2,index="simpson"); D2
# YIKES! These values are nearly identical, yet we know community 2 is half 
# as diverse as community 1!

# Again, let's calculate Simpson diversity for all levels of species richness 
# from 1 to 1000
simpson = matrix(ncol = 2,nrow = 1000)

for (i in 1:1000) {
  community = data.frame(t(rep(100,i))); colnames(community) = paste("sp",1:i)
  simpson[i,1] = i
  simpson[i,2] = diversity(community,index="simpson") }

# Plot the results
plot(simpson[,1], simpson[,2], xlab="Species Richness",
     ylab="Simpson Diversity", main="Simpson")

# Similar to what we saw in the lecture, the Gini-Simpson index increases very
# fast at low values of specie richness

# This is why we see misleading values from our two communities.

# Once again, effective numbers to the rescue!

# Since we are using the Gini-Simpson index here, the conversion of 
# the index to effective numbers is 1/1-D (rather than 1/D if we were
# using the Simpson index)
DE1 = 1/(1-D1); DE1
DE2 = 1/(1-D2); DE2

# DE1 and DE2 simply return the species richness value: 
# this is because abundances are equal

DE2 == 0.5*DE1 
# And, as we expected, diversity in community 2 is 
# half of that in community 1

# Calculate effective numbers of species (from Simpson diversity) for all 
# levels of species richness from 1:1000
simpson_effective = matrix(ncol = 2,nrow = 1000)

for(i in 1:1000) {
  community = data.frame(t(rep(100,i))); colnames(community) = paste("sp",1:i)
  simpson_effective[i,1] = i
  simpson_effective[i,2] = 1/(1-diversity(community,index="simpson")) }

# Plot the results
plot(simpson_effective[,1], simpson_effective[,2], xlab="Species Richness",
     ylab="Effective Numbers of Species", main="Simpson (Effective)")


##########################################
##      ENS for the dune diversity      ##
##########################################

# Now that we have a better understanding of ENS, let's add both the Shannon and
# the Simpson effective number of species to our dune dataframe.

# Shannon ENS
exp(dune.metrics$H)

# Add this data to the metrics dataframe
dune.metrics <- data.frame(dune.metrics,
                           Hens = exp(dune.metrics$H))

# Simpson ENS
1/(1 - dune.metrics$GS)

# Add this data to the metrics dataframe
dune.metrics <- data.frame(dune.metrics,
                           GSens = 1/(1 - dune.metrics$GS))




##########################################
##             Hill Numbers             ##
##########################################

# Like the conversion of Shannon diversity, effective numbers from Simpson 
# diversity yields a linear relationship with species richness

# Both Shannon and Simpson diversity are special cases entropy = the measure of 
# disorder in a system
# (more disorder = more diversity)
# There is a general equation for entropy from which Shannon and Simpson diversity
# are derived
# This equation has a parameter `q` that defines its sensitivity to rare species
# Low values of q favor rare species, high values of q favor abundant species
# When q = 0, diversity = S (richness), because rare species are treated 
# the same as abundant ones

# This is the basis of Hill numbers!

# The following function `divprof` calculates the diversity from this equation
# along a continuum of q values, creating what is called a 'diversity profile'

# Diversity profiles allow you to gauge the relative contribution of rare species
# to diversity your system
divprof = function(community) {
  cbind(
    seq(0, 5, by=0.099),
    unlist(lapply(seq(0, 5, by=0.099),
                  function(q) sum(apply(community, 1,
                                        function(x) (x/sum(x))^q))^(1/(1-q))))) }

# Let's apply function `divprof` to our community of species with equal abundances
community1.divprof = divprof(community1)

# Plot the results
plot(community1.divprof[,1], community1.divprof[,2], ylim=c(0,500),
     pch=16, cex=2, xlab="q value", ylab="Diversity")

# Not very interesting, since there are neither rare nor abundant species:
# every species is present in equal numbers,
# and therefore diversity is the same at all levels of q



# Let's create a new community of where abundances of S = 500 species are
# randomly sampled between 1 - 1000
set.seed(9)

community3 = data.frame(t(sample(1:1000,500))); colnames(community3) =
  paste("sp",1:500)


# Apply function `divprof` to this community
community3.divprof = divprof(community3)

# Plot the results
plot(community3.divprof[,1], community3.divprof[,2],
     pch=16, cex=2, xlab="q value", ylab="Diversity")

# Where q=0, diversity is species richness, which we can highlight graphically
text(0.6, 501, labels=c("Richness"))
points(community3.divprof[1,1], community3.divprof[1,2],
       col="red", pch=16, cex=2)


# Where q=1, diversity is Shannon diversity, we can find the exact value of
# the point at q=1
exp(diversity(community3, index="shannon"))

# Highlight this graphically
text(1.5, exp(diversity(community3,index="shannon"))+5,
     labels=c("Shannon Diversity"))
points(community3.divprof[11,1], community3.divprof[11,2],
       col="red", pch=16, cex=2)

# Where q=2, diversity is Simpson diversity, we can find the exact value of
# the point at q=2
1/(1-diversity(community3,index="simpson"))

# Highlight this graphically
text(2.5, 1/(1-diversity(community3,index="simpson"))+5,
     labels=c("Simpson Diversity"))
points(community3.divprof[21,1], community3.divprof[21,2],
       col="red", pch=16, cex=2)

# This plot shows that richness is high, but that as we increasingly favor 
# abundant species (increase q, move to the right along the x-axis), 
# diversity drops, indicating that community 3 is composed of some abundant and
# some rare species



# For kicks, let's create a community of 5 very abundant species, and 450 
# very rare species
set.seed(6)
 
community4 = data.frame(t(c(sample(500:1000, 5), sample(1:5, 495, replace=T))))

colnames(community4)=paste("sp",1:500)

# Apply function `divprof` to community 4
community4.divprof=divprof(community4)

# Plot the results and label q0 through q2
plot(community4.divprof[,1], community4.divprof[,2],
     pch=16, cex=2, xlab="q value", ylab="Diversity")

# Where q=0, diversity is species richness
text(0.45, 500, labels=c("Richness")) 
points(community4.divprof[1,1], community4.divprof[1,2],
       col="red", pch=16, cex=2)

# Where q=1, diversity is Shannon diversity
text(1, 35+exp(diversity(community4,index="shannon")),
     labels=c("Shannon Diversity")) 
points(community4.divprof[11,1], community4.divprof[11,2],
       col="red", pch=16, cex=2)

# where q=2, diversity is Simpson diversity
text(2.1,30+1/(1-diversity(community4,index="simpson")),
     labels=c("Simpson Diversity"))
points(community4.divprof[21,1], community4.divprof[21,2],
       col="red", pch=16,cex=2)

# Diversity drops off more steeply, since our community is dominated by a 
# few very abundant species!





##########################################
##          Whittaker's Beta            ##
##########################################

# Back to the dune data!

# Whittaker's beta is simply gamma (the regional species richness) divided
# by alpha (the local species richness)

# We can find gamma by simply looking at the number of columns in our data
# since the columns represent species

ncol(dune)

# Calculate Whittaker's beta for each site in the dune data
ncol(dune) / dune.metrics$S

# Add this info to the dune.metrics df
dune.metrics <- data.frame(dune.metrics,
                           Beta = ncol(dune) / dune.metrics$S)



##########################################
##      Bray-Curtis Dissimilarity       ##
##########################################

# Quantifies dissimilarity between pairs of sites

# Bray Curtis is a measure of structure, it takes into account who is at 
# each site and in what abundance


# For the dune data we have 20 sites, so our matrix will be 20x20
braycurtis = vegan :: vegdist(dune, method = "bray")

# Note there are many more metrics available with the vegdist function in 
# vegan. You can explore what others are available, by typing ?vegdist and
# looking through the possible entries in the methods argument

braycurtis <- as.matrix(braycurtis)



# We can create a heat map to understand which communities are more or less 
# dissimilar

# Create a long form dataframe that ggplot can use
braycurtis_df <- reshape2 :: melt(braycurtis)

# Filter out the upper triangle values (they're simply a duplicate of the 
# lower triangle)
braycurtis_df <- subset(braycurtis_df, Var1 >= Var2)

# Create a heatmap using ggplot2
ggplot(braycurtis_df, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Bray-Curtis Dissimilarity - Dune Data") +
  theme_bw()



####################################################
##      PUT WHAT WE'VE LEARNED INTO PRACTICE      ##
####################################################

# The BCI data in the vegan package represents counts of trees on Barro Colorado
# Island.

# Using the BCI data in the vegan package, calculate follow this workflow and 
# calculate the different metrics.

# You should submit a dataframe similar to what we generated for the dune data,
# with the following columns:
# "Site"  "S"     "N"     "H"     "J"     "GS"    "Sn"    "Hens"  "GSens"  "Beta"

# And a heatmap of Bray-Curtis Dissimilarity 



#########################################################
##      BONUS: Non-metric multidimensional scaling     ##
#########################################################

# One of the ways to reduce the number of dimensions through ordination
# is called non-metric multidimensional scaling.

# In a community with 100 sites that means 100 dimensions when we start
# thinking about pairwise dissimilarities, which is impossible to visualize
# all at once.

# The goal of NMDS is to collapse information from multiple dimensions
# into just a few, so that they can be visualized and interpreted.

# NMDS does not produce any statistical output (though there are ways to 
# use the results statistically, but this is not covered here).

# The goal of NMDS is to represent the position of each site/community
# in multidimensional space as accurately as possible using a reduced number
# of dimensions that can easily be visualized.

# NMDS does not use the absolute abundances of species in communities, but 
# rather their rank orders and as a result is a flexible technique that accepts
# a variety of types of data.

# We can use our site x species dune data to demonstrate.
# The k argument specifies how many dimensions we would like to reduce to.
example_NMDS <- vegan :: metaMDS(dune, k = 2)

# Plot the results
plot(example_NMDS)

# it doesn't look all the informative yet. But let's see what we can do to fix
# that.

# Ordination plot function especially for congested plots
ordiplot(example_NMDS,type="n") 

# add text or points to ordination plots
orditorp(example_NMDS, display="species", col="red", air=0.01) 
orditorp(example_NMDS, display="sites", cex=1.25, air=0.01)


# Let's imagine that communities 1-10 ad some treatment applied, and communities
# 11-20 a different treatment.

# Using ordihull we can draw convex hulls connecting the vertices of the points
# made by these communities on the plot.

# This is an intuitive way to understand how communities and species cluster
# based on treatments.

treat=c(rep("Treatment1", 10), rep("Treatment2", 10))

ordiplot(example_NMDS, type = "n")

ordihull(example_NMDS, groups=treat, draw="polygon", col="grey90", label=F)

orditorp(example_NMDS, display="species", col = "red", air = 0.01)

orditorp(example_NMDS, display="sites", col = c(rep("green", 10), rep("blue", 10)),
         air = 0.01,cex = 1.25)





