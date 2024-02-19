

##########################################
### ODE Solver for Predator Prey model ### START 
##########################################

h <- 0.0001 # step length -- you dont need to change this. It is for the solver 


t <- ??? ## how many time steps will you simulate 

## here is set up the two vectors to collect the time series data from the simulations
N <- numeric(t/h)
N[1] <- runif(1)
P <- numeric(t/h)
P[1] <- runif(1)


for(i in 1:(t/h)){ ## need to set this part up for the solver to run correctly
		
	prey_equation <- ???
	
	pred_equation <- ???
	
		
	N[i+1] <- N[i] + h*prey_equation ## dont change -- part of differential equation solver 
	P[i+1] <- P[i] + h*pred_equation ## dont change -- part of differential equation solver 
		
}

## clean up the data so you can plot it 
prey <- N[seq(1,(t/h),(1/h))]
pred <- P[seq(1,(t/h),(1/h))]
time <- seq(1,t,1)


##########################################
### ODE Solver for Predator Prey model ### END 
##########################################


### what do the results look like?

### Try adding density depence on the prey similar to the logistic growth equation we worked with. 




