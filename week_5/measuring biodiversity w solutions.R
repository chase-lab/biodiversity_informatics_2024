# R code for lecture Biodiversity informatics 
# Lecture 4 by Dr Roel van Klink



# generate a vector of species abundances: 

nSpecies <- 50


abundData <- matrix(data = NA,
                    ncol = nSpecies) # empty data frame
rownames(abundData)<- "abundDataNormal"
# draw a number from a normal distribution  # not very realistic
rnorm(1, mean = 5, sd = 2.5)


for(s in 1:nSpecies){
  abundData[s] <- rnorm(1,mean = 5, sd = 2.5)
}

# make integers:
abundData<- round(abundData, 0)

# look at data
abundData
hist(abundData)



#more realistic: 
abundDataPois<- rpois(nSpecies, 5) # Poisson distribution (5 = mean and sd)
abundDataNB <- rnbinom (nSpecies, 2, 0.05) # negative binomial distribution



# make a matrix for later
abundmatrix<- rbind(abundData,
  abundDataNB,
  abundDataPois)

# or make up some numbers
abundData4<-rbind(
  c(0,1,1,2,4), 
  c(1,1,3,5,7), 
  c(1,2,5,8,13)
)


# exercise 1: 
# visualize the communities using 

# Instructions: 
# find a package that does what you want 
# find the right function
# read the help page to find out how it needs the data input

# 1) species abundance distribution #####
hist(abundDataPois)
hist(log(abundDataPois))

library(ggplot2)
ggplot(data=as.data.frame(abundDataPois),  aes(x=abundDataPois)) + 
  geom_histogram(bins = 8)+
  scale_x_log10()

# 2) rank abundance distribution
library(goeveg)
racurve(t(abundDataPois))
racurve(t(abundDataNB))
racurves(abundData4)
racurves(rbind(abundDataNB, abundDataPois))

# hard coding from scratch:
plot(sort(abundDataPois, decreasing = T), plot = F)
lines(sort(abundDataPois, decreasing = T))

#relative abundances
abundRel<- abundDataPois/sum(abundDataPois)
plot(sort(abundRel, decreasing = T), plot = F)
lines(sort(abundRel, decreasing = T))


# 3) species accumulation curves
library(iNEXT)
inextAs1<- iNEXT(list(abundDataPois))
plot(inextAs1)




# Excercise 2: #####
# calculate biodiversity metrics on these abundance vectors
library(vegan)


specnumber(abundData)
specnumber(abundDataNB)

diversity(abundData)
diversity(abundDataNB)
diversity(abundDataPois)

exp(diversity(abundData))
exp(diversity(abundDataNB))

diversity(abundData, "invsimpson")
diversity(abundDataNB, "invsimpson")
diversity(abundDataPois, "invsimpson")

abundmatrix<- rbind(abundData,
                    abundDataNB,
                    abundDataPois)

diversity(abundmatrix, "invsimpson")

exp(diversity(abundmatrix, "shannon"))


# Pielou evenness
H <- diversity(abundDataNB)
H/log(specnumber(abundDataNB))






# Beta diversity #####
# make a matrix of species abundances at different sites
nSites <- 3

abundMatrix <- matrix(data = NA,
                      ncol = nSpecies,
                      nrow = nSites)

abundMatrix[1,]<- rpois(nSpecies, 5)
abundMatrix[2,]<- rpois(nSpecies, 7)
abundMatrix[3,]<- rpois(nSpecies, 9)
# etc.



# add some random zeroes
indexZeroes<- sample(1:50, 5) # these are the places we will put zeroes
abundMatrix[1,indexZeroes] <- 0 # put 0's in these rows
indexZeroes<- sample(1:50, 8) # these are the places we will put zeroes
abundMatrix[2,indexZeroes] <- 0 # put 0's in these rows
indexZeroes<- sample(1:50, 15) # these are the places we will put zeroes
abundMatrix[3,indexZeroes] <- 0 # put 0's in these rows





# or combine the matrices from before
abundMatrix2<- rbind(abundData,
                    abundDataNB,
                    abundDataPois)


# excercise 3: #####
# calculate beta diversity between the generated communities 

# Whittaker beta (hard coded)
meanAlpha<- mean(rowSums(abundMatrix!=0))
gamma<- ncol(abundMatrix)

gamma/meanAlpha


vegdist(abundMatrix2)

vegdist(abundMatrix2, method = "jaccard", binary = T )

jac<- vegdist(abundMatrix, method = "jaccard" )
mean(jac)




# exercise 4: 
# visualizing pairwise dissimilarities
nmds<- metaMDS(dune, plot = T)
#not much to be seen here, except that the distances are almost the same. 
#this is logical because we randomly generated data. no pattern is to be expected


# better examples are available in the vegan package:
?metaMDS

# the vegan help page on ordinations is very helpful: 
https://cran.r-project.org/web/packages/vegan/vignettes/intro-vegan.pdf


nmds<- metaMDS(dune, plot = T)
sitescores<- scores(nmds)[[1]]
ggplot(as.data.frame(sitescores), aes(x = NMDS1, y = NMDS2))+
       geom_point()


# fitting environmental variables to the ordination
?envfit

# visualizing site clustering (similar sites cluster together) 
?ordispider

