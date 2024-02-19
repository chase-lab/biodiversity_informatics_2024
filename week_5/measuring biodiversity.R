# R code for lecture Biodiversity informatics 
# Lecture 4 by Dr Roel van Klink



# generate a vector of species abundances: 


nSpecies <- 50


abundData <- matrix(data = NA,
                    ncol = nSpecies) # empty data frame

# draw a number from a normal distribution  # not very realistic
rnorm(1, mean = 5, sd = 2.5)

# draw a string of numbers from this distribution
for(s in 1:nSpecies){
  abundData[s] <- rnorm(1,mean = 5, sd = 2.5)
}

# make integers by rounding:
abundData<- round(abundData, 0)

# inspect data
abundData
hist(abundData)



#more realistic: 
abundDataPois<- rpois(nSpecies, 5) # Poisson distribution (5 = mean and sd)
abundDataNB <- rnbinom (nSpecies, 2, 0.05) # negative binomial distribution


# or make up some numbers
abundData4<-rbind(
  c(0,1,1,2,4), 
  c(1,1,3,5,7), 
  c(1,2,5,8,13)
)





# excercise 1: 
# visualize the communities using 
# 1) species abundance distribution
# 2) rank abundance distribution
# 3) species accumulation curves

# Instructions: 
# find a package that does what you want 
# find the right function
# read the help page to find out how it needs the data input




# Excercise 2: 
# calculate biodiversity metrics on these abundance vectors






# Beta diversity
# make a matrix of species abundances at different sites
nSites <- 3

abundMatrix <- matrix(data = NA,
                    nrow = nSpecies,
                    ncol = nSites)

abundMatrix[,1]<- rpois(nSpecies, 5)
abundMatrix[,2]<- rpois(nSpecies, 7)
abundMatrix[,3]<- rpois(nSpecies, 9)
# etc.



# add some random zeroes
indexZeroes<- sample(1:50, 5) # these are the places we will put zeroes
abundMatrix[indexZeroes,1] <- 0 # put 0's in these rows
abundMatrix[indexZeroes,2] <- 0 # put 0's in these rows
abundMatrix[indexZeroes,3] <- 0 # put 0's in these rows





# or combine the matrices from before
abundmatrix<- rbind(abundData,
                    abundDataNB,
                    abundDataPois)


# excercise 3: 
# calculate beta diversity between the generated communities 



# exercise 4: 
# visualizing pairwise dissimilarities
library(vegan)
metaMDS(abundMatrix2, plot = T)
#not much to be seen here, except that the distances are almost the same. 
#this is logical because we randomly generated data. no pattern is to be expected


# better examples are available in the vegan package:
?metaMDS

# fitting environmental variables to the ordination
?envfit



