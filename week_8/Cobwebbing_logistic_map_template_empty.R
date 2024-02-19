
###################################
### basic set-up for cobwebbing ### START 
###################################

## Note the model needs to be put in here. This is set up for the Logistic Map
curve(lambda*x*(1-x), 0,1,xlab="N[t]",ylab="N[t+1]",col="black",lwd=2,ylim=c(0,1)) # create iterative map for logistic map
curve(1*x+0,add=TRUE,col="red") # add the 1:1 line for cobwebbing
for(t in 1:num_runs){
	
	

	segments(N[t],N[t],N[t],N[t+1],lwd=0.5,col="blue") # add the cobwebbing dynamics 
	segments(N[t],N[t+1],N[t+1],N[t+1],lwd=0.5,col="blue") # add the cobwebbing dyamics
}


###################################
### basic set-up for cobwebbing ### END 
###################################

