
###############################################
#### Full Bifurcation of the logistic map  ####
###############################################

par(mfrow=c(1,1))
sweeping_vector <- seq(0,4,0.0001)
local_min_vec<-c()
local_max_vec<-c()
x_value_vec_min<-c()
x_value_vec_max<-c()
for(xxx in 1:length(sweeping_vector)){
	cat("processing",xxx, "of", length(sweeping_vector),"\n")
#xxx <- 1

	### ********************* ####start
	### insert the model here #### 
	
	### --- select the parameter to bifurcate across --- ### start 
	 <- sweeping_vector[xxx] 
	
	### --- select the parameter to bifurcate across --- ###  end 



	### ********************* ####	end 
	

	trajectory_ts <- N[(num_runs-100): num_runs] ## make sure that the variable/parameter namse match up here
	trajectory_ts <- trajectory_ts[is.numeric(trajectory_ts)]
	trajectory_ts <- trajectory_ts[!is.nan(trajectory_ts)]
	trajectory_ts <- trajectory_ts[trajectory_ts != -Inf]
	
	local_maxima<-c()
	local_minima<-c()
	for(i in 1:length(trajectory_ts)){
		if( i > 1 & i < length(trajectory_ts)){## makes sure it doesnt use the fist r last value in the time series
			if(trajectory_ts[i] > trajectory_ts[i - 1] & trajectory_ts[i] > trajectory_ts[i + 1]){ ## This finds local maxima
				local_maxima <- c(local_maxima,trajectory_ts[i] )
			}
			if(trajectory_ts[i] < trajectory_ts[i - 1] & trajectory_ts[i] < trajectory_ts[i + 1]){ ## This finds local minima
				local_minima <- c(local_minima, trajectory_ts[i] )
			}
		}
	}


local_minima <-  unique(local_minima)
local_maxima <-  unique(local_maxima)


x_value_vec_min <-c(x_value_vec_min,rep(sweeping_vector[xxx],length(local_minima)))
x_value_vec_max <-c(x_value_vec_max,rep(sweeping_vector[xxx],length(local_maxima)))
local_min_vec <-c(local_min_vec, local_minima)
local_max_vec <-c(local_max_vec, local_maxima )
}## end of bifurcation

par(mfrow=c(1,1),mai=c(0.7,0.7,0.1,0.1))
plot(x_value_vec_min, local_min_vec,ylim=c(min(local_min_vec),1) ,ylab="",xlab="",yaxt="n",pch=1,cex=0.01)
points(x_value_vec_max, local_max_vec,cex=0.01,pch=1)
mtext("N",side=2,line=2.5,las=1)
mtext(expression(lambda),side=1,line=2)
axis(2,las=2)

####################################
###### End of bifurcation code #####
####################################
