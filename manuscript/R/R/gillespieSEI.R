#Gillespie algorithm running stochastic SEI model 
require(GillespieSSA)

simSEI <- function(pop, params, finalTime){
	v0 <- c(S=pop$susc, V=pop$vacc, E=pop$latent, I=pop$infect, cumE=0, cumI=0)
	# We are not considering natural death of latent dogs.
	# This is equivalent to some assumption about how we think about progression probability, 
  # we hope (ie., dogs that are going to die naturally are treated as if their bites were not 
  # infectious, and remain in the susceptible class).
	
	# Events (7):
	# Birth of susceptible
	# 2: Natural deaths (of S, V) (don't look at exposed/infected deaths)
	# Loss of immunity (rate zero for now)
	# Susceptible animal exposed and starts to incubate infection
	# Latent animal shows signs and becomes infectious
	# Infectious animal dies due to disease
	
	propensity <- c(
		"br*(S+V)",
		"dr*S",
		"dr*V",
		"loss*V",
		"beta*I*S", 
		"mu*E",
		"alpha*I"
	)
	
	gill_params = c(
		br=params$births,
		dr=params$deaths,
		loss=params$loss,
		beta=params$beta,
		mu=params$mu,
		alpha=params$alpha
	)

	state_change <- (matrix(
		c(
			 1,  0,  0,  0,  0,  0,
			-1,  0,  0,  0,  0,  0,
			 0, -1,  0,  0,  0,  0,
			 1, -1,  0,  0,  0,  0,
			-1,  0,  1,  0,  1,  0,
			 0,  0, -1,  1,  0,  1,
			 0,  0,  0, -1,  0,  0
		), nrow=6
	))
	state_change
	
	out <- ssa(v0, propensity, state_change, gill_params, finalTime)

	times=seq(0, finalTime, repTime)
	timeseries=out$data[findInterval(times, out$data[,1]),]
	
	data.frame(
    times=times, 
		S=timeseries[,"S"], 
		V=timeseries[,"V"], 
		exposures=c(pop$latent, diff(timeseries[,"cumE"])), 
		incidence=c(pop$infect, diff(timeseries[,"cumI"]))
    )
}

#END


#	To un-suppress this test, make sure to require library up top as well.
# finalTime = 3
# repTime = 2/52
# 
# simSEI(
# 	pop=list(susc=1000, vacc=10, latent=3, infect=0),
# 	params=list(births=0.36, deaths=0.33, loss=0, beta=beta, mu=mu, alpha=alpha),
# 	finalTime=finalTime
# )


#Formulate for a frequency dependent formulation......
simSEI_FD <- function(pop, params, finalTime){
	v0 <- c(S=pop$susc, V=pop$vacc, E=pop$latent, I=pop$infect, cumE=0, cumI=0)
	
  # Note that this uses a simple formulation of beta (without the contribution of births)
	propensity <- c(
		"br*(S+V)",
		"dr*S",
		"dr*V",
		"loss*V",
		"(R0*alpha*I*S)/(S+I+E+V)",
		"mu*E",
		"alpha*I"
	)
	
	gill_params = c(
		br=params$births,
		dr=params$deaths,
		loss=params$loss,
		R0=params$R0,
		mu=params$mu,
		alpha=params$alpha
	)

	state_change <- (matrix(
		c(
			 1,  0,  0,  0, 0, 0,
			-1,  0,  0,  0, 0, 0,
			 0, -1,  0,  0, 0, 0,
			 1, -1,  0,  0, 0, 0,
			-1,  0,  1,  0, 1, 0,
			 0,  0, -1,  1, 0, 1,
			 0,  0,  0, -1, 0, 0
		), nrow=6
	))
	state_change
	
	out <- ssa(v0, propensity, state_change, gill_params, finalTime)

	times=seq(0, finalTime, repTime)
	timeseries=out$data[findInterval(times, out$data[,1]),]
	
	data.frame(times=times, 
		S=timeseries[,"S"], 
		V=timeseries[,"V"], 
		exposures=c(pop$latent, diff(timeseries[,"cumE"])), 
		incidence=c(pop$infect, diff(timeseries[,"cumI"])))
}

#SimEpidemicFD(
#	pop=list(susc=1000, vacc=10, latent=10, infect=0),
#	params=list(births=0.36, deaths=0.33, loss=0, R0=R0, mu=mu, alpha=alpha),
#	time=finalTime
#)



