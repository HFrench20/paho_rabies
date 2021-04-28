#TSI model for dog rabies (adopted from Ottar's TSIR for measles)

# DETERMINISTIC
simTSI = function(pop, params, finalTime) {
  times = seq(0, finalTime, repTime)
  lambda = I = S = rep(NA, length(times)) # Initialize empty vectors
  I[1] = pop$infect
  lambda[1] = pop$infect
  S[1] = pop$susc
  beta = params$beta
  alfa = params$alfa

  for (i in 2:length(times)) {
    lambda[i] = beta * S[i - 1] * I[i - 1]^alfa #??standard TSI (so assume no incubation period - or exactly 1 month!)
    if (lambda[i] < 0) {lambda[i] = 0}
    I[i] = lambda[i]
    S[i] = S[i - 1] - I[i]
  }
  list(I = I, S = S)
}

# explore variability due to beta and include incursions, and susceptible proportion (1-vc)
# note this also includes demography - population growth (birth rate) limited by K
simTSI1 = function(pop, params, finalTime) {
  times = seq(0, finalTime, repTime)
  lambda = I = S = rep(NA, length(times)) # Initialize empty vectors
  I[1] = pop$infect
  lambda[1] = pop$infect
  S[1] = pop$susc

  beta_shape = params$beta^2/params$var_beta # Introduce stochasticity into Beta
  beta_rate = params$beta/params$var_beta
  alfa = params$alfa

  for (i in 2:length(times)) {
    lambda[i] = rgamma(1, shape = beta_shape, rate = beta_rate) * S[i - 1] * I[i - 1]^alfa
    if (lambda[i] < 0) {lambda[i] = 0}
    I[i] = round(lambda[i]) + rbinom(1, 1, params$intro_rate) # plus random introductions from outside the area
    # introductions currently added in according to binomial distribution - but better as a poisson!

#    if (S[i-1] < 0) {S[i] = 0}
    tpop = round(S[i-1] + (S[i-1] * (1-params$sp[i-1]))) +1
    L = params$br * (S[1] - S[i-1])/params$K
    if (L < 0) {L = 0}
    births = sum(rpois(tpop, L))
    S[i] = rbinom(n=1, size=tpop, prob=params$sp[i-1]) + births - I[i]

  }
  list(I = I, S = S)
}


# Replace function with a more natural parameterization using negative binomial offspring distribution
#??(beta = R0/pop$susc)
simTSInb = function(pop, params, finalTime) {
  times = seq(0, finalTime, repTime)
  lambda = I = S = rep(NA, length(times)) # Initialize empty vectors
  I[1] = pop$infect
  lambda[1] = pop$infect
  S[1] = pop$susc
  mu_bites = params$mu_bites
  k = params$k
  p = params$p

  for (i in 2:length(times)) {
    # Incorporate susceptible depletion into the mean bites/rabid dog
    bites = sum(rnbinom(I[i - 1], mu = mu_bites * S[i - 1]/pop$susc, size = k))
    lambda[i] = rbinom(1, bites, prob = p) # p is the probability that a bite results in infection
    I[i] = lambda[i] + rbinom(1, 1, params$intro_rate) # plus random introductions from outside the area
    S[i] = S[i - 1] - I[i]
  }
  list(I = I, S = S)
}


# Incorporate demography - carrying capacity
simTSInb1 = function(pop, params, finalTime) {
  times = seq(0, finalTime, repTime)
  lambda = I = S = rep(NA, length(times)) # Initialize empty vectors
  I[1] = pop$infect
  lambda[1] = pop$infect
  S[1] = pop$susc
  mu_bites = params$mu_bites
  k = params$k
  p = params$p

  for (i in 2:length(times)) {
    # Incorporate susceptible depletion into the mean bites/rabid dog
    bites = sum(rnbinom(I[i - 1], mu = mu_bites * S[i - 1]/pop$susc, size = k))
    lambda[i] = rbinom(1, bites, prob = p) # p is the probability that a bite results in infection
    I[i] = lambda[i] + rpois(1, params$intro_rate) # plus random introductions from outside the area

    tpop = round(S[i-1] + (S[i-1] * (1-params$sp[i-1]))) +1
    L = params$br * (S[1] - S[i-1])/params$K
    if (L < 0) {L = 0}
    births = sum(rpois(tpop, L))
    S[i] = rbinom(n=1, size=tpop, prob=params$sp[i-1]) + births - I[i]
  }
  list(I = I, S = S)
}


# change introduction so using possion introduction rate (not binomial)
simTSInb_vacc = function(pop, params, finalTime) {
  times = seq(0, finalTime, repTime)
  lambda = I = S = rep(NA, length(times)) # Initialize empty vectors
  I[1] = pop$infect
  lambda[1] = pop$infect
  S[1] = pop$susc
  mu_bites = params$mu_bites
  k = params$k
  p = params$p

  for (i in 2:length(times)) {
    # Incorporate susceptible depletion into the mean bites/rabid dog
    bites = sum(rnbinom(I[i - 1], mu = mu_bites, size = k))
    lambda[i] = rbinom(1, bites, prob = p * S[i - 1]/tpop)  # p is the probability that a bite results in infection
    I[i] = lambda[i] + rpois(1, params$intro_rate) # plus random introductions from outside the area

    tpop = round(S[i-1] + (S[i-1] * (1-params$sp[i-1]))) +1
    L = params$br * (S[1] - S[i-1])/params$K
    if (L < 0) {L = 0}
    births = sum(rpois(tpop, L))
    S[i] = rbinom(n=1, size=tpop, prob=params$sp[i-1]) + births - I[i]
  }
  list(I = I, S = S)
}




# pop <- list(susc=1000, infect=1) # set up initial population with some latent dogs and proportion vaccinated
# params <- list(beta = 0.0012, alfa=1.005, br=br)
# finalTime <- 6; repTime <- 4/52; times <- seq(0, finalTime, repTime) # Time - 5 years checking every 2 week interval
# out = simTSI(pop, params, finalTime)
#
# par(mfrow = c(1, 2))
# plot(out$I, ylab = "infected", xlab = "time")
# plot(out$S, out$I, ylab = "infected", xlab = "susceptible")
#
# pop <- list(susc=1000, infect=1) # set up initial population with some latent dogs and proportion vaccinated
# params <- list(beta = R0/pop$susc, var_beta = 5e-7, alfa=1, br=br, intro_rate = 3/52) # 3 introductions per year
# finalTime <- 15; repTime <- 4/52; times <- seq(0, finalTime, repTime) # Time - 5 years checking every 2 week interval
# out = simTSIv(pop, params, finalTime)
#
# par(mfrow = c(1, 2))
# plot(out$I, ylab = "infected", xlab = "time", type="l")
# plot(out$S, out$I, ylab = "infected", xlab = "susceptible")

# # WITH VACCINATION
# pop <- list(susc=1000, infect=1) # set up initial population with some latent dogs and proportion vaccinated
# finalTime <- 15; repTime <- 4/52; times <- seq(0, finalTime, repTime) # Time - 5 years checking every 2 week interval
# params <- list(beta = R0/pop$susc, var_beta = 5e-7, alfa=1, intro_rate = 10/52,
#                sp = rep(1, length(times)+1), br = 1.2/52, K = pop$susc*1.1)
#   # 10 introductions per year, K of 110% of susceptibles, br = growth rate of 1.1/yr
# out = simTSIv(pop, params, finalTime)
#
# par(mfrow = c(3, 1), mar=c(2,3,1,1))
# plot(out$I, ylab = "infected", xlab = "time", type="l")
# plot(out$S, out$I, ylab = "infected", xlab = "susceptible")
# plot(out$S, ylab = "infected", xlab = "time", type="l")
#
# BIRTHS
# Assume dog population growth of 1.2/yr
# Growth according to possion draw given birth rate * susceptibles
# and assuming carrying capacity K (1.1 * N)

# sp = rep(1, length(times)+1) # susceptible proportion
# vp = seq(9, length(sp), 12) # vaccination pulse
# vc = c(0, rnorm(length(vp), mean = 0.5, sd=0.1)) #??coverage in each campaign - with no vc at start
# tsv = rep(1:12, finalTime+2); lag = 1:5; tsv = c(tsv[-lag], lag) # note that vacc starts in Sept
# campaign = rep(1:(finalTime+2), each = 12)[-lag]
#
# for(i in 2:length(sp)){
#   sp[i] = 1-vc[campaign[i]] * exp(-br * tsv[i]) # allows susceptible proportion to increase unrealistically
#   }


# WITH VACCINATION
# pop <- list(susc=1000, infect=1) # set up initial population with some latent dogs and proportion vaccinated
# finalTime <- 15; repTime <- 4/52; times <- seq(0, finalTime, repTime) # Time - 5 years checking every 2 week interval
# params <- list(beta = R0/pop$susc, alfa=1, intro_rate = 10/52, sp = rep(1, length(times)+1), br = 1.2/52, K = pop$susc*1.1)
# # 10 introductions per year, K of 110% of susceptibles, br = growth rate of 1.1/yr
# out = simTSIv(pop, params, finalTime)
#
# par(mfrow = c(3, 1), mar=c(2,3,1,1))
# plot(out$I, ylab = "infected", xlab = "time", type="l")
# plot(out$S, out$I, ylab = "infected", xlab = "susceptible")
# plot(out$S, ylab = "infected", xlab = "time", type="l")
#
